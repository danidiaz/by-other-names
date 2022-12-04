{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module ByOtherNamesH where

import Control.Applicative
import Data.Foldable.WithIndex
import Data.Functor.WithIndex
import Data.Kind
import Data.Proxy
import Data.Traversable.WithIndex
import GHC.Generics
import GHC.TypeLits

type AliasesH :: (Type -> Type) -> Type -> (Type -> Type) -> Type
data AliasesH rep a h where
  Field :: 
    KnownSymbol fieldName => 
    a ->
    h v -> 
    AliasesH (S1 ('MetaSel ('Just fieldName) unpackedness strictness laziness) (Rec0 v)) a h
  Branch :: 
    KnownSymbol branchName => 
    a -> 
    BranchFieldsH v h ->
    AliasesH (C1 ('MetaCons branchName fixity sels) v) a h
  FieldTree ::
    AliasesH left a h ->
    AliasesH right a h ->
    AliasesH (left :*: right) a h
  BranchTree ::
    AliasesH left a h ->
    AliasesH right a h ->
    AliasesH (left :+: right) a h
  -- | We force the sum to contain at least two branches.
  Sum ::
    AliasesH (left :+: right) a h ->
    AliasesH (D1 x (left :+: right)) a h
  Record ::
    AliasesH fields a h ->
    AliasesH (D1 x (C1 y fields)) a h

type BranchFieldsH :: (Type -> Type) -> (Type -> Type) -> Type
data BranchFieldsH rep h where
  BranchFieldTree :: 
    BranchFieldsH left h ->  
    BranchFieldsH right h -> 
    BranchFieldsH (left :*: right) h
  BranchField ::
    h v ->
    BranchFieldsH (S1 ('MetaSel 'Nothing unpackedness strictness laziness) (Rec0 v)) h

data TupleH :: [Type] -> (Type -> Type) -> Type where
  EmptyTuple  :: TupleH '[] h
  ConsTuple :: h x -> TupleH xs h -> TupleH (x ': xs) h

-- | An intermediate datatype for specifying the aliases.  See
-- 'aliasListBegin', 'alias' and 'aliasListEnd'.
type AliasListH :: [(Symbol, [Type])] -> Type -> (Type -> Type) -> Type
data AliasListH code a h where
  EmptyAliasList :: AliasListH '[] a h
  ConsAliasList :: 
    Proxy name -> 
    a -> 
    TupleH slots h -> 
    AliasListH prev a h -> AliasListH ('(name,slots) : prev) a h

type AssertNamesAreEqual :: Symbol -> Symbol -> Constraint
type family AssertNamesAreEqual given expected where
  AssertNamesAreEqual expected expected = ()
  AssertNamesAreEqual given expected =
    TypeError
      ( Text "Expected field or constructor name \"" :<>: Text expected :<>: Text "\","
          :$$: Text "but instead found name \"" :<>: Text given :<>: Text "\"."
      )

type ToAliasesH :: [(Symbol, [Type])] -> (Type -> Type) -> [(Symbol, [Type])] -> Constraint
class ToAliasesH before rep after | before rep -> after, after rep -> before where
  parseAliasTree :: AliasListH before a h -> (AliasesH rep a h, AliasListH after a h)

type ToBranchFieldsH :: [Type] -> (Type -> Type) -> [Type] -> Constraint 
class ToBranchFieldsH before rep after | before rep -> after, after rep -> before where
  parseBranchFields :: TupleH before h -> (BranchFieldsH rep h, TupleH after h)

instance ToBranchFieldsH (v ': vs) (S1 ('MetaSel 'Nothing unpackedness strictness laziness) (Rec0 v)) vs where
  parseBranchFields (ConsTuple hv rest) = (BranchField hv, rest) 

instance (ToBranchFieldsH before left middle, 
          ToBranchFieldsH middle right end) 
  =>  ToBranchFieldsH before (left :*: right) end where
  parseBranchFields t0 = do
    let (leftResult, leftLeftover) = parseBranchFields @before t0
        (rightResult, rightLeftover) = parseBranchFields @middle leftLeftover
    (BranchFieldTree leftResult rightResult, rightLeftover)

instance ToAliasesH before tree '[] => ToAliasesH before (D1 x (C1 y tree)) '[] where
  parseAliasTree as =
    let (aliases', as') = parseAliasTree as
     in (Record aliases', as')

instance (ToAliasesH before left middle, ToAliasesH middle right end) 
  => ToAliasesH before (left :*: right) end where
  parseAliasTree as =
    let (left, middle) = parseAliasTree @before as
        (right, end) = parseAliasTree @middle middle
     in (FieldTree left right, end)

instance  KnownSymbol name 
  => ToAliasesH ('(name, '[v]) : rest) (S1 ('MetaSel (Just name) x y z) (Rec0 v)) rest where
  parseAliasTree (ConsAliasList _ a (ConsTuple hv EmptyTuple) rest) = (Field a hv, rest)

instance ToAliasesH before (left :+: right) '[] => ToAliasesH before (D1 x (left :+: right)) '[] where
  parseAliasTree as =
    let (aliases', as') = parseAliasTree as
     in (Sum aliases', as')

instance (ToAliasesH before left middle, ToAliasesH middle right end) => ToAliasesH before (left :+: right) end where
  parseAliasTree as =
    let (left, middle) = parseAliasTree @before as
        (right, end) = parseAliasTree @middle middle
     in (BranchTree left right, end)

instance (KnownSymbol name,
          ToBranchFieldsH vs slots '[]) =>
  ToAliasesH ('(name, vs) : rest) (C1 ('MetaCons name fixity False) slots) rest where
    parseAliasTree (ConsAliasList _ a branchFields rest) = do
        let (theBranchFields, EmptyTuple) = parseBranchFields @vs branchFields
        (Branch a theBranchFields, rest)

--
--

type AliasedH :: k -> Type -> Constraint
class (RubricH k, Generic r) => AliasedH k r where
  aliasesH :: AliasesH (Rep r) (AliasTypeH k) (WrapperTypeH k)

type RubricH :: k -> Constraint
class RubricH k where
  type AliasTypeH k :: Type
  type WrapperTypeH k :: Type -> Type

aliasListBeginH :: forall names a h rep. (ToAliasesH names rep '[]) 
  => AliasListH names a h
  -> AliasesH rep a h
aliasListBeginH names =
  let (aliases, EmptyAliasList) = parseAliasTree @names @rep names
   in aliases

-- | The empty `AliasList`.
aliasListEndH :: AliasListH '[] a h
aliasListEndH = EmptyAliasList

aliasH :: forall name slots a h names. 
  a -> 
  TupleH slots h ->
  AliasListH names a h -> 
  AliasListH ('(name, slots) : names) a h
aliasH = ConsAliasList (Proxy @name)
