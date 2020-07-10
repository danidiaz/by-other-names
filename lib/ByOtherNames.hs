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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module ByOtherNames
  ( Aliases(..),
    AliasList,
    fieldAliases,
    alias,
    aliasListEnd,
    Aliased (aliases),
    module Data.Proxy,
    Symbol,
    Rubric(AliasesType)
  )
where

import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits

type Aliases :: Type -> (Type -> Type) -> Type
data Aliases a rep where
  Leaf :: a -> Aliases a (S1 ('MetaSel (Just symbol) x y z) v)
  Prod ::
    Aliases a left ->
    Aliases a right ->
    Aliases a (left :*: right)
  Object ::
    Aliases a prod ->
    Aliases a (D1 x (C1 y prod))

type AliasList :: Type -> [Symbol] -> Type
data AliasList a names where
  Null :: AliasList a '[]
  Cons :: Proxy name -> a -> AliasList a names -> AliasList a (name : names)

alias :: Proxy name -> a -> AliasList a names -> AliasList a (name : names)
alias = Cons

aliasListEnd :: AliasList a '[]
aliasListEnd = Null

type AliasTree :: [Symbol] -> (Type -> Type) -> [Symbol] -> Constraint
-- Note that we could add the functional dependency "rep after -> before", but
-- we don't want that because it would allow us to omit the field name
-- annotation when giving the aliases. We *don't* want inference there!
class AliasTree before rep after | before rep -> after where 
  parseAliasTree :: AliasList a before -> (Aliases a rep, AliasList a after)

instance AliasTree (name : names) (S1 ('MetaSel (Just name) x y z) v) names where
  parseAliasTree (Cons _ a rest) = (Leaf a, rest)

instance (AliasTree before left middle, AliasTree middle right end) => AliasTree before (left :*: right) end where
  parseAliasTree as =
    let (left, middle) = parseAliasTree @before as
        (right, end) = parseAliasTree @middle middle
     in (Prod left right, end)

toAliases :: forall before tree a. AliasTree before tree '[] => AliasList a before -> Aliases a tree
toAliases names =
  let (aliases, Null) = parseAliasTree @before @tree names
   in aliases

fieldAliases :: forall before tree a x y. (AliasTree before tree '[]) => AliasList a before -> Aliases a (D1 x (C1 y tree))
fieldAliases = Object . toAliases @before @tree @a

type Aliased :: k -> Type -> Type -> Constraint
class Generic r => Aliased k a r | k -> a where
  aliases :: Proxy k -> Proxy r -> Aliases a (Rep r)

type Rubric :: k -> Constraint
class Rubric k where
    type AliasesType k :: Type

