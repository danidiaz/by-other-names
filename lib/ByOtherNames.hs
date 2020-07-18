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
module ByOtherNames
  ( Aliases (..),
    AliasList,
    aliasListBegin,
    alias,
    aliasListEnd,
    Aliased (aliases),
    module Data.Proxy,
    Symbol,
    Rubric (..),
    ParsingRubric (..),
  )
where

import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import Control.Applicative

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

-- | Define the aliases for a type by listing them. 
--
-- See also 'alias' and 'aliasListEnd'.
aliasListBegin :: forall before a tree. (AliasTree before tree '[]) => AliasList a before -> Aliases a tree
aliasListBegin names =
  let (aliases, Null) = parseAliasTree @before @tree names
   in aliases

-- | The empty `AliasList`.
aliasListEnd :: AliasList a '[]
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
  parseAliasTree :: AliasList a before -> (Aliases a rep, AliasList a after)

--
instance AssertNamesAreEqual name name' => AliasTree (name : names) (S1 ('MetaSel (Just name') x y z) v) names where
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
instance AssertNamesAreEqual name name' => AliasTree (name : names) (C1 ('MetaCons name' fixity False) slots) names where
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
  aliases :: Aliases (AliasType k) (Rep r)

-- | Typeclass for marker datakinds used as rubrics, to classify aliases.
--
-- The associated type family `AliasType` gives the type of the aliases.
type Rubric :: k -> Constraint
class Rubric k where
  type AliasType k :: Type

-- 
-- extra struff for parsing
type ParsingRubric :: k -> Constraint
class Rubric k => ParsingRubric k where
    type RequiredConstraint k :: Type -> Constraint  
    type ParserType k :: Type -> Type
    parseField :: RequiredConstraint k v => AliasType k -> ParserType k v
    parseBranch :: RequiredConstraint k v => AliasType k -> ParserType k v
    parseBranch0 :: AliasType k -> ParserType k ()

type ParseableWithRubric :: k -> (Type -> Type) -> Constraint
class ParseableWithRubric k t where
  parseWithRubric :: Proxy k -> Aliases (AliasType k) t -> ParserType k (t x)

instance (ParsingRubric k, Functor (ParserType k), RequiredConstraint k v) => ParseableWithRubric k (S1 x (Rec0 v)) where
  parseWithRubric (_ :: Proxy k) (Field fieldName) = M1 . K1 <$> parseField @_ @k fieldName

instance (ParsingRubric k, Applicative (ParserType k), ParseableWithRubric k left, ParseableWithRubric k right) => ParseableWithRubric k (left :*: right) where
  parseWithRubric _ (FieldTree left right) =
    (:*:) <$> parseWithRubric (Proxy @k) left <*> parseWithRubric (Proxy @k) right

instance (ParsingRubric k, Functor (ParserType k), ParseableWithRubric k prod) => ParseableWithRubric k (D1 x (C1 y prod)) where
  parseWithRubric _ (Record prod) =
    M1 . M1 <$> parseWithRubric (Proxy @k) prod

instance (ParsingRubric k, Functor (ParserType k), RequiredConstraint k v) => ParseableWithRubric k (C1 x (S1 y (Rec0 v))) where
  parseWithRubric _ (Branch branchName) = M1 . M1 . K1 <$> parseBranch @_ @k branchName

instance (ParsingRubric k, Functor (ParserType k), RequiredConstraint k v) => ParseableWithRubric k (C1 x U1) where
  parseWithRubric _ (Branch branchName) = M1 U1 <$ parseBranch0 @_ @k branchName

instance (ParsingRubric k, Alternative (ParserType k), ParseableWithRubric k left, ParseableWithRubric k right) => ParseableWithRubric k (left :+: right) where
  parseWithRubric _ (BranchTree left right) = (L1 <$> parseWithRubric (Proxy @k) left) <|> (R1 <$> parseWithRubric (Proxy @k) right)

