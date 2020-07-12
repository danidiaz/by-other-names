{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This package provides the general mechanism for defining field and branch
-- aliases for algebraic datatypes.
--
-- Aliases can be defined for multiple contexts (json serialization, orms...).
-- Each of those contexts is termed a 'Rubric', basically a marker datakind
-- used to namespace the aliases.
--
-- This module should only be imported if you want to define your own adapter
-- package for some new `Rubric`.
module ByOtherNames
  ( Aliases (..),
    AliasList,
    fieldAliases,
    branchAliases,
    alias,
    aliasListEnd,
    Aliased (aliases),
    module Data.Proxy,
    Symbol,
    Rubric (..),
  )
where

import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits

-- | This datatype carries the field aliases and matches the structure of the
--   generic Rep' shape.
--
--   Note that the type of the aliases is polymorphic; it depends on the
--   'Rubric'.
type Aliases :: Type -> (Type -> Type) -> Type
data Aliases a rep where
  Field :: a -> Aliases a (S1 metasel v)
  Branch :: a -> Aliases a (C1 metacons v)
  FieldTree ::
    Aliases a left ->
    Aliases a right ->
    Aliases a (left :*: right)
  BranchTree ::
    Aliases a left ->
    Aliases a right ->
    Aliases a (left :+: right)
  -- | We force the sum to contain at least two branches.
  Sum ::
    Aliases a (left :+: right) ->
    Aliases a (D1 x (left :+: right))
  Record ::
    Aliases a fields ->
    Aliases a (D1 x (C1 y fields))

-- | An intermediate datatype that makes it easier to specify the aliases.  See
-- also 'fieldAliases' and 'branchAliases'. 
type AliasList :: Type -> [Symbol] -> Type
data AliasList a names where
  Null :: AliasList a '[]
  Cons :: Proxy name -> a -> AliasList a names -> AliasList a (name : names)

-- | Add an alias to an `AliasList`.
alias :: Proxy name -> a -> AliasList a names -> AliasList a (name : names)
alias = Cons

-- | The empty `AliasList`.
aliasListEnd :: AliasList a '[]
aliasListEnd = Null

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
  parseAliasTree :: AliasList a before -> (Aliases a rep, AliasList a after)

type NamesShouldBeEqual :: Symbol -> Symbol -> Constraint
type family NamesShouldBeEqual given expected where
    NamesShouldBeEqual expected expected = ()
    NamesShouldBeEqual given expected = 
        TypeError ((Text "Expected field or branch name \"" :<>: Text expected :<>: Text "\",")
                   :$$:
                   (Text "but instead found name \"" :<>: Text given :<>: Text "\"."))

instance NamesShouldBeEqual name name' => AliasTree (name : names) (S1 ('MetaSel (Just name') x y z) v) names where
  parseAliasTree (Cons _ a rest) = (Field a, rest)

instance (AliasTree before left middle, AliasTree middle right end) => AliasTree before (left :*: right) end where
  parseAliasTree as =
    let (left, middle) = parseAliasTree @before as
        (right, end) = parseAliasTree @middle middle
     in (FieldTree left right, end)

--
instance NamesShouldBeEqual name name' => AliasTree (name : names) (C1 ('MetaCons name' fixity False) slots) names where
  parseAliasTree (Cons _ a rest) = (Branch a, rest)

instance (AliasTree before left middle, AliasTree middle right end) => AliasTree before (left :+: right) end where
  parseAliasTree as =
    let (left, middle) = parseAliasTree @before as
        (right, end) = parseAliasTree @middle middle
     in (BranchTree left right, end)

--
toAliases :: forall before a tree. AliasTree before tree '[] => AliasList a before -> Aliases a tree
toAliases names =
  let (aliases, Null) = parseAliasTree @before @tree names
   in aliases

-- | Define 'Aliases' for a record type.
fieldAliases :: forall before a tree x y. (AliasTree before tree '[]) => AliasList a before -> Aliases a (D1 x (C1 y tree))
fieldAliases = Record . toAliases @before @a @tree

-- | Define 'Aliases' for a sum type.
--
-- The sum type can only have zero or one attributes, and they can't have selectors.
branchAliases :: forall before a left right x. (AliasTree before (left :+: right) '[]) => AliasList a before -> Aliases a (D1 x (left :+: right))
branchAliases = Sum . toAliases @before @a @(left :+: right)

-- | Typeclass for datatypes @r@ that have aliases for some 'Rubric' @k@.
type Aliased :: k -> Type -> Constraint
class (Rubric k, Generic r) => Aliased k r where
  aliases :: Aliases (ForRubric k) (Rep r)

-- | Typeclass for marker datakinds used as rubrics, to classify aliases.
--
-- The associated type family `ForRubric` gives the type of the aliases.
type Rubric :: k -> Constraint
class Rubric k where
  type ForRubric k :: Type

