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

-- | 
-- This module provides the general mechanism for defining field and branch
-- aliases for algebraic datatypes.
-- 
-- Aliases can be defined for multiple contexts (json serialization, orms...).
-- Each of those contexts is termed a Rubric, basically a marker datakind used
-- to namespace the aliases.
-- 
-- This module should only be imported if you want to define your own adapter
-- package for some new 'Rubric'. See "ByOtherNamesH.Aeson" for a concrete
-- example.
-- 
-- This module provides a more versatile, but also more verbose, version of the
-- functionality provided by "ByOtherNames". If you plan to use both
-- "ByOtherNames" and "ByOtherNamesH", import this module qualified to avoid
-- name collisions:
--
-- > import qualified ByOthernamesH as H
--
module ByOtherNamesH (
  -- * Aliases 
  Aliases,
  AliasList,
  aliasListBegin,
  alias,
  aliasListEnd,
  SlotList,
  singleSlot,
  slot,
  slotListEnd,
  -- * Rubrics
  Rubric (..),
  Aliased (..),
  -- * Generic helpers
  GRecord(..),
  -- * Re-exports
  Symbol,
) where

import Control.Applicative
import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Data.Functor.Identity

-- | This datatype carries the field/branch aliases, along with a value wrapped in @h@
-- for each field in the original datatype. 
--
-- It matches the shape of the generic 'Rep'.
type Aliases :: (Type -> Type) -> Type -> (Type -> Type) -> Type
data Aliases rep a (h :: Type -> Type) where
  Field :: 
    KnownSymbol fieldName => 
    a ->
    h v -> 
    Aliases (S1 ('MetaSel ('Just fieldName) unpackedness strictness laziness) (Rec0 v)) a h
  Branch :: 
    KnownSymbol branchName => 
    a -> 
    BranchFields v h ->
    Aliases (C1 ('MetaCons branchName fixity sels) v) a h
  EmptyBranch ::
    KnownSymbol branchName => 
    a -> 
    Aliases (C1 ('MetaCons branchName fixity sels) U1) a h
  FieldTree ::
    Aliases left a h ->
    Aliases right a h ->
    Aliases (left :*: right) a h
  BranchTree ::
    Aliases left a h ->
    Aliases right a h ->
    Aliases (left :+: right) a h
  -- | We force the sum to contain at least two branches.
  Sum ::
    Aliases (left :+: right) a h ->
    Aliases (D1 x (left :+: right)) a h
  Record ::
    Aliases fields a h ->
    Aliases (D1 x (C1 y fields)) a h

type BranchFields :: (Type -> Type) -> (Type -> Type) -> Type
data BranchFields rep h where
  BranchFieldTree :: 
    BranchFields left h ->  
    BranchFields right h -> 
    BranchFields (left :*: right) h
  BranchField ::
    h v ->
    BranchFields (S1 ('MetaSel 'Nothing unpackedness strictness laziness) (Rec0 v)) h

-- | A list of slots associated an alias. Indexed by the types of each slot
-- and a type constructor that wraps each slot value.
-- 
-- For records, each field alias will have one and only one slot: the
-- corresponding record field. See 'singleSlot'.
-- 
-- For sum types, each branch alias might have zero or more slots, depending on
-- the structure of the datatype. See 'slot' and 'slotListEnd'.
data SlotList :: [Type] -> (Type -> Type) -> Type where
  EmptyTuple  :: SlotList '[] h
  ConsTuple :: h x -> SlotList xs h -> SlotList (x ': xs) h

-- | An intermediate helper datatype for specifying the aliases.  
--
-- Indexed by a list of names accompanied by field types.
--
-- See 'aliasListBegin', 'alias' and 'aliasListEnd'.
type AliasList :: [(Symbol, [Type])] -> Type -> (Type -> Type) -> Type
data AliasList (names_slots :: [(Symbol, [Type])]) a (h :: Type -> Type) where
  EmptyAliasList :: AliasList '[] a h
  ConsAliasList :: 
    Proxy name -> 
    a -> 
    SlotList slots h -> 
    AliasList prev a h -> AliasList ('(name,slots) : prev) a h

type ToAliases :: [(Symbol, [Type])] -> (Type -> Type) -> [(Symbol, [Type])] -> Constraint
-- | The second functional dependency is needed for type inference to work. 
class ToAliases before rep after | before rep -> after, after rep -> before where
  parseAliasTree :: AliasList before a h -> (Aliases rep a h, AliasList after a h)

type ToBranchFields :: [Type] -> (Type -> Type) -> [Type] -> Constraint 
-- | The second functional dependency is needed for type inference to work. 
class ToBranchFields before rep after | before rep -> after, after rep -> before where
  parseBranchFields :: SlotList before h -> (BranchFields rep h, SlotList after h)

instance (ToBranchFields before left middle, 
          ToBranchFields middle right end) 
  =>  ToBranchFields before (left :*: right) end where
  parseBranchFields t0 = do
    let (leftResult, leftLeftover) = parseBranchFields @before t0
        (rightResult, rightLeftover) = parseBranchFields @middle leftLeftover
    (BranchFieldTree leftResult rightResult, rightLeftover)

instance ToBranchFields (v ': vs) (S1 ('MetaSel 'Nothing unpackedness strictness laziness) (Rec0 v)) vs where
  parseBranchFields (ConsTuple hv rest) = (BranchField hv, rest) 

instance ToAliases before tree '[] => ToAliases before (D1 x (C1 y tree)) '[] where
  parseAliasTree as =
    let (aliases', as') = parseAliasTree as
     in (Record aliases', as')

instance (ToAliases before left middle, ToAliases middle right end) 
  => ToAliases before (left :*: right) end where
  parseAliasTree as =
    let (left, middle) = parseAliasTree @before as
        (right, end) = parseAliasTree @middle middle
     in (FieldTree left right, end)

instance  KnownSymbol name 
  => ToAliases ('(name, '[v]) : rest) (S1 ('MetaSel (Just name) x y z) (Rec0 v)) rest where
  parseAliasTree (ConsAliasList _ a (ConsTuple hv EmptyTuple) rest) = (Field a hv, rest)

instance ToAliases before (left :+: right) '[] => ToAliases before (D1 x (left :+: right)) '[] where
  parseAliasTree as =
    let (aliases', as') = parseAliasTree as
     in (Sum aliases', as')

instance (ToAliases before left middle, ToAliases middle right end) => ToAliases before (left :+: right) end where
  parseAliasTree as =
    let (left, middle) = parseAliasTree @before as
        (right, end) = parseAliasTree @middle middle
     in (BranchTree left right, end)

instance (KnownSymbol name,
          ToBranchFields vs (S1 u v) '[]) =>
  ToAliases ('(name, vs) : rest) (C1 ('MetaCons name fixity False) (S1 u v)) rest where
    parseAliasTree (ConsAliasList _ a branchFields rest) = do
        let (theBranchFields, EmptyTuple) = parseBranchFields @vs branchFields
        (Branch a theBranchFields, rest)

instance (KnownSymbol name,
          ToBranchFields vs (left :*: right) '[]) =>
  ToAliases ('(name, vs) : rest) (C1 ('MetaCons name fixity False) (left :*: right)) rest where
    parseAliasTree (ConsAliasList _ a branchFields rest) = do
        let (theBranchFields, EmptyTuple) = parseBranchFields @vs branchFields
        (Branch a theBranchFields, rest)

instance KnownSymbol name =>
  ToAliases ('(name, '[]) : rest) (C1 ('MetaCons name fixity False) U1) rest where
    parseAliasTree (ConsAliasList _ a  EmptyTuple rest) = do
        (EmptyBranch a, rest)

--
--

type Aliased :: k -> Type -> Constraint
class (Rubric k, Generic r) => Aliased k r where
  aliases :: Aliases (Rep r) (AliasType k) (WrapperType k)

type Rubric :: k -> Constraint
class Rubric k where
  type AliasType k :: Type
  type WrapperType k :: Type -> Type

aliasListBegin :: forall names_slots a h rep. (ToAliases names_slots rep '[]) 
  => AliasList names_slots a h -- ^ indexed by a list of alias names / slots types
  -> Aliases rep a h -- ^ indexed by a generic 'Rep' 
aliasListBegin names =
  let (aliases, EmptyAliasList) = parseAliasTree @names_slots @rep names
   in aliases

-- | The empty 'AliasList'.
aliasListEnd :: AliasList '[] a h
aliasListEnd = EmptyAliasList

alias :: forall name slots a h names_slots. 
  -- | The alias value
  a -> 
  -- | \"wrapped\" values for each slot of the alias
  SlotList slots h ->
  AliasList names_slots a h -> 
  AliasList ('(name, slots) : names_slots) a h
alias = ConsAliasList (Proxy @name)

-- | The empty 'SlotList'.
slotListEnd :: SlotList '[] h 
slotListEnd = EmptyTuple

singleSlot :: h v -> SlotList '[v] h 
singleSlot hv = ConsTuple hv EmptyTuple

slot :: h v -> SlotList rest h  -> SlotList (v ': rest) h
slot hv = ConsTuple hv 

class GRecord rep where
  -- | Builds a parser for the entire generic 'Rep' out of parsers for each field.
  gToRecord ::
    Applicative g =>
    -- | Field aliases.
    Aliases rep a h ->
    (forall v. a -> h v -> g v) ->
    g (rep z)
  gFromRecord ::
    -- | Record representation.
    rep z ->
    Aliases rep String Identity
  gBiliftA2RecordAliases ::
    -- | Combine aliases
    (a1 -> a2 -> ar) -> 
    -- | Combine slots
    (forall v. h1 v -> h2 v -> hr v) ->
    Aliases rep a1 h1 ->
    Aliases rep a2 h2 ->
    Aliases rep ar hr

instance GRecord prod => GRecord (D1 x (C1 y prod)) where
  gToRecord (Record as) parseField =
    M1 . M1 <$> gToRecord as parseField
  gFromRecord (M1 (M1 prod)) =
    Record (gFromRecord prod)
  gBiliftA2RecordAliases f g (Record a1) (Record a2) =
    Record (gBiliftA2RecordAliases f g a1 a2)

instance
  (GRecord left, GRecord right) =>
  GRecord (left :*: right)
  where
  gToRecord (FieldTree aleft aright) parseField =
    (:*:) <$> gToRecord aleft parseField <*> gToRecord aright parseField
  gFromRecord (left :*: right) =
    FieldTree (gFromRecord left) (gFromRecord right)
  gBiliftA2RecordAliases f g (FieldTree left1 right1) (FieldTree left2 right2) =
    FieldTree (gBiliftA2RecordAliases f g left1 left2) (gBiliftA2RecordAliases f g right1 right2)

instance KnownSymbol fieldName => GRecord (S1 ('MetaSel ('Just fieldName) unpackedness strictness laziness) (Rec0 v)) where
  gToRecord (Field a hv) parseField =
    M1 . K1 <$> parseField a hv
  gFromRecord (M1 (K1 v)) = Field (symbolVal (Proxy @fieldName)) (Identity v)
  gBiliftA2RecordAliases f g (Field a1 h1) (Field a2 h2) =
    Field (f a1 a2) (g h1 h2)