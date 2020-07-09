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

module ByOtherNames
  ( Aliases,
    AliasList,
    aliasListBegin,
    alias,
    aliasListEnd,
    Aliased (aliases),
    module Data.Proxy,
    Symbol,
  )
where

import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits

type Aliases :: Type -> (Type -> Type) -> Type
data Aliases a rep where
  Alias :: a -> Aliases a (S1 ('MetaSel (Just symbol) x y z) v)
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
class AliasTree before rep after | before rep -> after where
  parseAliasTree :: AliasList a before -> (Aliases a rep, AliasList a after)

instance AliasTree (name : names) (S1 ('MetaSel (Just name) x y z) v) names where
  parseAliasTree (Cons _ a rest) = (Alias a, rest)

instance (AliasTree before left middle, AliasTree middle right end) => AliasTree before (left :*: right) end where
  parseAliasTree as =
    let (left, middle) = parseAliasTree @before as
        (right, end) = parseAliasTree @middle middle
     in (Prod left right, end)

toAliases :: forall before tree a. AliasTree before tree '[] => AliasList a before -> Aliases a tree
toAliases names =
  let (aliases, Null) = parseAliasTree @before @tree names
   in aliases

aliasListBegin :: forall before tree a x y. (AliasTree before tree '[]) => AliasList a before -> Aliases a (D1 x (C1 y tree))
aliasListBegin = Object . toAliases @before @tree @a

type Aliased :: k -> Type -> Type -> Constraint
class Generic r => Aliased k a r | k -> a where
  aliases :: Proxy k -> Proxy r -> Aliases a (Rep r)

