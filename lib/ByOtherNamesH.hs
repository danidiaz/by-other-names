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

module ByOtherNamesH () where

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

type MissingAlias :: Symbol -> Constraint
type family MissingAlias expected where
  MissingAlias expected =
    TypeError
      (Text "No alias given for field or constructor name \"" :<>: Text expected :<>: Text "\".")

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

