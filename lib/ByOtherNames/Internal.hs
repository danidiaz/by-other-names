{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
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
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | This package provides the general mechanism for defining field and branch
-- aliases for algebraic datatypes.
--
-- Aliases can be defined for multiple contexts (json serialization, orms...).
-- Each of those contexts is termed a 'Rubric', basically a marker datakind
-- used to namespace the aliases.
--
-- This module should only be imported if you want to define your own adapter
-- package for some new `Rubric`.
module ByOtherNames.Internal
  ( Aliases (..),
    AliasList,
    aliasListBegin,
    alias,
    aliasListEnd,
    Aliased (aliases),
    Rubric (..),
    -- * Generic helpers
    GSum (..),
    Slot (..),
    GRecord (..),
    -- * Re-exports
    Symbol,
  )
where

import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Data.Functor.WithIndex
import Data.Foldable.WithIndex
import Data.Traversable.WithIndex
import Control.Applicative

-- | This datatype carries the field aliases and matches the structure of the
--   generic Rep' shape.
--
--   Note that the type of the aliases is polymorphic; it depends on the
--   'Rubric'.
type Aliases :: (Type -> Type) -> Type -> Type
data Aliases rep a where
  Field :: KnownSymbol fieldName => a -> Aliases (S1 ('MetaSel ('Just fieldName) unpackedness strictness laziness) v) a
  Branch :: KnownSymbol branchName => a -> Aliases (C1 ('MetaCons branchName fixity sels) v) a
  FieldTree ::
    Aliases left a ->
    Aliases right a ->
    Aliases (left :*: right) a
  BranchTree ::
    Aliases left a ->
    Aliases right a ->
    Aliases (left :+: right) a
  -- | We force the sum to contain at least two branches.
  Sum ::
    Aliases (left :+: right) a ->
    Aliases (D1 x (left :+: right)) a
  Record ::
    Aliases fields a ->
    Aliases (D1 x (C1 y fields)) a

instance Functor (Aliases rep) where
  fmap f as = case as of 
    Field a -> Field (f a)
    Branch a -> Branch (f a)
    FieldTree left right -> FieldTree (fmap f left) (fmap f right)
    BranchTree left right -> BranchTree (fmap f left) (fmap f right)
    Sum a -> Sum (fmap f a)
    Record a -> Record (fmap f a)

instance Foldable (Aliases rep) where
    foldMap f as = case as of
      Field a -> f a
      Branch a -> f a
      FieldTree left right -> foldMap f left <> foldMap f right
      BranchTree left right -> foldMap f left <> foldMap f right
      Sum a -> foldMap f a
      Record a -> foldMap f a

instance Traversable (Aliases rep) where
  traverse f as = case as of 
      Field a -> Field <$> f a
      Branch a -> Branch <$> f a
      FieldTree left right -> FieldTree <$> traverse f left <*> traverse f right
      BranchTree left right -> BranchTree  <$> traverse f left <*> traverse f right
      Sum a -> Sum <$> traverse f a
      Record a -> Record <$> traverse f a

deriving anyclass instance (FunctorWithIndex String (Aliases rep))

deriving anyclass instance (FoldableWithIndex String (Aliases rep))

instance TraversableWithIndex String (Aliases rep) where
  itraverse f as = case as of 
    afield@(Field a) -> Field <$> traverseField f afield a
    abranch@(Branch a) -> Branch <$> traverseBranch f abranch a
    FieldTree left right -> FieldTree <$> itraverse f left <*> itraverse f right
    BranchTree left right -> BranchTree <$> itraverse f left <*> itraverse f right
    Sum a -> Sum <$> itraverse f a
    Record a -> Record <$> itraverse f a
    where
      traverseField :: forall fieldName a m b v proxy unpackedness strictness laziness. KnownSymbol fieldName => (String -> a -> m b) -> proxy (S1 ('MetaSel ('Just fieldName) unpackedness strictness laziness) v) a -> a -> m b
      traverseField f _ a = let fieldName = symbolVal (Proxy @fieldName)
        in f fieldName a
      traverseBranch :: forall branchName a m b v proxy fixity sels. KnownSymbol branchName => (String -> a -> m b) -> proxy (C1 ('MetaCons branchName fixity sels) v) a -> a -> m b
      traverseBranch f _ a = let branchName = symbolVal (Proxy @branchName)
        in f branchName a

-- | An intermediate datatype that makes it easier to specify the aliases.  See
-- 'aliasListBegin', 'alias' and 'aliasListEnd'.
type AliasList :: [Symbol] -> Type -> Type
data AliasList names a where
  Null :: AliasList '[] a
  Cons :: Proxy name -> a -> AliasList names a -> AliasList (name : names) a

-- | Add an alias to an `AliasList`.
--
-- __/TYPE APPLICATION REQUIRED!/__ You must provide the field/branch name using a type application.
alias :: forall name a names. a -> AliasList names a -> AliasList (name : names) a
alias = Cons (Proxy @name)

-- | Define the aliases for a type by listing them.
--
-- See also 'alias' and 'aliasListEnd'.
aliasListBegin :: forall before a tree. (AliasTree before tree '[]) => AliasList before a -> Aliases tree a
aliasListBegin names =
  let (aliases, Null) = parseAliasTree @before @tree names
   in aliases

-- | The empty `AliasList`.
aliasListEnd :: AliasList '[] a
aliasListEnd = Null

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

-- type ExcessAliasError :: Symbol -> Constraint
-- type family ExcessAliasError name where
--   ExcessAliasError name =
--     TypeError
--       ( Text "Alias given for nonexistent field or constructor \"" :<>: Text name :<>: Text "\".")

-- | This typeclass converts the list-representation of aliases `AliasList` to
-- the tree of aliases 'Aliases' that matches the generic Rep's shape.
--
-- Also, quite importantly, it ensures that the field names in the list match
-- the field names in the Rep.
type AliasTree :: [Symbol] -> (Type -> Type) -> [Symbol] -> Constraint
-- Note that we could add the functional dependency "rep after -> before", but
-- we don't want that because it would allow us to omit the field name
-- annotation when giving the aliases. We *don't* want inference there!
class AliasTree before rep after | before rep -> after where
  parseAliasTree :: AliasList before a -> (Aliases rep a, AliasList after a)

--
instance (AssertNamesAreEqual name name', KnownSymbol name') => AliasTree (name : names) (S1 ('MetaSel (Just name') x y z) v) names where
  parseAliasTree (Cons _ a rest) = (Field a, rest)

instance MissingAlias name' => AliasTree '[] (S1 ('MetaSel (Just name') x y z) v) '[]

instance (AliasTree before left middle, AliasTree middle right end) => AliasTree before (left :*: right) end where
  parseAliasTree as =
    let (left, middle) = parseAliasTree @before as
        (right, end) = parseAliasTree @middle middle
     in (FieldTree left right, end)

instance AliasTree before tree '[] => AliasTree before (D1 x (C1 y tree)) '[] where
  parseAliasTree as =
    let (aliases', as') = parseAliasTree as
     in (Record aliases', as')

-- doesn't work because of the functional dependency :(
-- instance ExcessAliasError name => AliasTree before (D1 x (C1 y tree)) (name : names) where

--
instance (AssertNamesAreEqual name name', KnownSymbol name') => AliasTree (name : names) (C1 ('MetaCons name' fixity False) slots) names where
  parseAliasTree (Cons _ a rest) = (Branch a, rest)

instance MissingAlias name' => AliasTree '[] (C1 ('MetaCons name' fixity False) slots) '[]

instance (AliasTree before left middle, AliasTree middle right end) => AliasTree before (left :+: right) end where
  parseAliasTree as =
    let (left, middle) = parseAliasTree @before as
        (right, end) = parseAliasTree @middle middle
     in (BranchTree left right, end)

instance AliasTree before (left :+: right) '[] => AliasTree before (D1 x (left :+: right)) '[] where
  parseAliasTree as =
    let (aliases', as') = parseAliasTree as
     in (Sum aliases', as')

-- doesn't work because of the functional dependency :(
-- instance ExcessAliasError name => AliasTree before (D1 x (left :+: right)) (name : names) where

-- | Typeclass for datatypes @r@ that have aliases for some 'Rubric' @k@.
type Aliased :: k -> Type -> Constraint
class (Rubric k, Generic r) => Aliased k r where
  aliases :: Aliases (Rep r) (AliasType k) 

-- | Typeclass for marker datakinds used as rubrics, to classify aliases.
--
-- The associated type family `ForRubric` gives the type of the aliases.
type Rubric :: k -> Constraint
class Rubric k where
  type AliasType k :: Type


--
--
class GRecord (c :: Type -> Constraint) rep where
  gToRecord :: Applicative m =>  
    Aliases rep a ->
    (forall v . c v => a -> m v) ->
    m (rep z)
  gFromRecord ::
    Aliases rep a ->
    (forall v. c v => a -> v -> o) ->
    rep z ->
    [o]
  gRecordEnum :: 
    Aliases rep a ->
    (forall v. c v => a -> Proxy v -> o) ->
    [o]

instance GRecord c prod => GRecord c (D1 x (C1 y prod)) where
  gToRecord (Record as) parseField =
    M1 . M1 <$> gToRecord @c  as parseField
  gFromRecord (Record as) renderField (M1 (M1 prod)) = 
    gFromRecord @c as renderField prod
  gRecordEnum (Record as) renderField = gRecordEnum @c @prod as renderField

instance c v => GRecord c (S1 x (Rec0 v)) where
  gToRecord (Field a) parseField = 
    M1 . K1 <$> parseField a
  gFromRecord (Field a) renderField (M1 (K1 v)) = [renderField a v]
  gRecordEnum (Field a) renderField = [renderField a (Proxy @v)]

instance (GRecord c left, GRecord c right) =>
  GRecord c (left :*: right) where
  gToRecord (FieldTree aleft aright) parseField =
    (:*:) <$> gToRecord @c aleft parseField <*> gToRecord @c aright parseField
  gFromRecord (FieldTree aleft aright) renderField (left :*: right) = 
    gFromRecord @c aleft renderField left  ++ gFromRecord @c aright renderField right
  gRecordEnum (FieldTree aleft aright) renderField =
    gRecordEnum @c @left aleft renderField ++ gRecordEnum @c @right aright renderField 

--
--
data Slot m1 m2 v =
        ZeroSlots v
      | SingleSlot (m1 v)
      | ManySlots (m2 v)
      deriving stock (Show, Functor)

class GSum (c :: Type -> Constraint) rep where
  gToSum :: (Functor n, Applicative m2) =>
    Aliases rep a ->
    (forall b . a -> Slot m1 m2 b -> n b) ->
    (forall v . c v => m1 v) ->
    (forall v . c v => m2 v) ->
    [n (rep z)]
  gFromSum ::
    Aliases rep a ->
    (forall v. c v => v -> o) ->
    rep z ->
    (a, [o])
  gSumEnum ::
    Aliases rep a ->
    (a -> [o] -> r) ->
    (forall v. c v => Proxy v -> o) ->
    [r]

instance
  (GSum c (left :+: right)) =>
  GSum c (D1 x (left :+: right))
  where
  gToSum (Sum s) parseBranch parseSlot1 parseSlot2 = fmap M1 <$> gToSum @c s parseBranch parseSlot1 parseSlot2
  gFromSum (Sum s) renderSlot (M1 srep) = gFromSum @c s renderSlot srep
  gSumEnum (Sum s) renderBranch renderSlot = gSumEnum @c @_ @_ @_ s renderBranch renderSlot

instance
  ( GSum c left,
    GSum c right
  ) =>
  GSum c (left :+: right)
  where
  gToSum (BranchTree aleft aright) parseBranch parseSlot1 parseSlot2 =
    (fmap L1 <$> gToSum @c @left aleft parseBranch parseSlot1 parseSlot2) ++ (fmap R1 <$> gToSum @c @right aright parseBranch parseSlot1 parseSlot2)
  gFromSum (BranchTree aleft aright) renderSlot = \case
    L1 rleft -> gFromSum @c aleft renderSlot rleft
    R1 rright -> gFromSum @c aright renderSlot rright
  gSumEnum (BranchTree aleft aright) renderBranch renderSlot = 
    gSumEnum @c aleft renderBranch renderSlot ++ gSumEnum @c aright renderBranch renderSlot


instance GSum c (C1 x U1) where
  gToSum (Branch fieldName) parseBranch parseSlot1 parseSlot2 =
    [parseBranch fieldName (ZeroSlots (M1 U1))]
  gFromSum (Branch fieldName) renderSlot _ =
    (fieldName, [])
  gSumEnum (Branch fieldName) renderBranch renderSlot = 
    [renderBranch fieldName []]

instance (c v) => GSum c (C1 x (S1 y (Rec0 v))) where
  gToSum (Branch fieldName) parseBranch parseSlot1 parseSlot2 =
    [M1 . M1. K1 <$> parseBranch fieldName (SingleSlot parseSlot1)]
  gFromSum (Branch fieldName) renderSlot (M1 (M1 (K1 slots))) =
    (fieldName, [ renderSlot slots ])
  gSumEnum (Branch fieldName) renderBranch renderSlot = 
    [renderBranch fieldName [ renderSlot (Proxy @v) ]]

instance (GSumSlots c (left :*: right)) => GSum c (C1 x (left :*: right)) where
  gToSum (Branch fieldName) parseBranch parseSlot1 parseSlot2 =
    [M1 <$> parseBranch fieldName (ManySlots (gToSumSlots @c parseSlot2))]
  gFromSum (Branch fieldName) renderSlot (M1 slots) =
    (fieldName, gFromSumSlots @c renderSlot slots)
  gSumEnum (Branch fieldName) renderBranch renderSlot = 
    [renderBranch fieldName (gSumEnumSlots @c @(left :*: right) renderSlot)]


class GSumSlots (c :: Type -> Constraint) rep where
  gToSumSlots :: Applicative m =>
    (forall v . c v => m v) ->
    m (rep z)
  gFromSumSlots :: (forall v. c v => v -> o) -> rep z -> [o]
  gSumEnumSlots :: (forall v. c v => Proxy v -> o) -> [o]

instance c v => GSumSlots c (S1 y (Rec0 v)) where
  gToSumSlots parseSlot = M1 . K1 <$> parseSlot
  gFromSumSlots renderSlot (M1 (K1 v)) = [renderSlot v]
  gSumEnumSlots renderSlot = [renderSlot (Proxy @v)]

instance
  ( GSumSlots c left,
    GSumSlots c right
  ) =>
  GSumSlots c (left :*: right)
  where
  gToSumSlots parseSlot =   
    (:*:) <$> gToSumSlots @c @left parseSlot <*> gToSumSlots @c @right parseSlot
  gFromSumSlots renderSlot (left :*: right) =
    gFromSumSlots @c renderSlot left ++ gFromSumSlots @c renderSlot right
  gSumEnumSlots renderSlot = 
    gSumEnumSlots @c @left renderSlot ++ gSumEnumSlots @c @right renderSlot