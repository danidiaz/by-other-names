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

-- This datatype carries the field aliases and matches the structure of the
-- generic Rep' shape.

-- It checks (for records) that field have names, but doesn't impose any
-- constraint on the names. Ditto for branches.
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
  Sum ::
    Aliases a (left :+: right) ->
    Aliases a (D1 x (left :+: right))
  Record ::
    Aliases a fields ->
    Aliases a (D1 x (C1 y fields))

type AliasList :: Type -> [Symbol] -> Type
data AliasList a names where
  Null :: AliasList a '[]
  Cons :: Proxy name -> a -> AliasList a names -> AliasList a (name : names)

alias :: Proxy name -> a -> AliasList a names -> AliasList a (name : names)
alias = Cons

aliasListEnd :: AliasList a '[]
aliasListEnd = Null

-- This typeclass converts the list-representation of aliases to the tree of
-- aliases that matches the generic Rep's shape.  
--
-- Also, quite importantly, it ensures that the field names in the list match
-- the field names in the Rep. 
type AliasTree :: [Symbol] -> (Type -> Type) -> [Symbol] -> Constraint
-- Note that we could add the functional dependency "rep after -> before", but
-- we don't want that because it would allow us to omit the field name
-- annotation when giving the aliases. We *don't* want inference there!
class AliasTree before rep after | before rep -> after where
  parseAliasTree :: AliasList a before -> (Aliases a rep, AliasList a after)

instance AliasTree (name : names) (S1 ('MetaSel (Just name) x y z) v) names where
  parseAliasTree (Cons _ a rest) = (Field a, rest)

instance (AliasTree before left middle, AliasTree middle right end) => AliasTree before (left :*: right) end where
  parseAliasTree as =
    let (left, middle) = parseAliasTree @before as
        (right, end) = parseAliasTree @middle middle
     in (FieldTree left right, end)

--
instance AliasTree (name : names) (C1 ('MetaCons name fixity False) slots) names where
  parseAliasTree (Cons _ a rest) = (Branch a, rest)

instance (AliasTree before left middle, AliasTree middle right end) => AliasTree before (left :+: right) end where
  parseAliasTree as =
    let (left, middle) = parseAliasTree @before as
        (right, end) = parseAliasTree @middle middle
     in (BranchTree left right, end)

--
toAliases :: forall before a tree . AliasTree before tree '[] => AliasList a before -> Aliases a tree
toAliases names =
  let (aliases, Null) = parseAliasTree @before @tree names
   in aliases

fieldAliases :: forall before a tree x y. (AliasTree before tree '[]) => AliasList a before -> Aliases a (D1 x (C1 y tree))
fieldAliases = Record . toAliases @before @a @tree

branchAliases :: forall before a left right x. (AliasTree before (left :+: right) '[]) => AliasList a before -> Aliases a (D1 x (left :+: right))
branchAliases = Sum . toAliases @before  @a @(left :+: right)

type Aliased :: k -> Type -> Constraint
class (Rubric k, Generic r) => Aliased k r where
  aliases :: Aliases (ForRubric k) (Rep r)

type Rubric :: k -> Constraint
class Rubric k where
  type ForRubric k :: Type

