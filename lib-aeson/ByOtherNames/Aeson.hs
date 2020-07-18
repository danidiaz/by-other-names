{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | A 'Rubric' for JSON serialization using Aeson, along with some helper
-- newtypes and re-exports.
--
-- Required extensions:
--
-- - DataKinds
-- - DeriveGeneric
-- - DerivingVia
-- - FlexibleInstances
-- - MultiParamTypeClasses
-- - OverloadedStrings
-- - TypeApplications
-- - ScopedTypeVariables
--
-- Example of use:
--
-- @
-- import ByOtherNames.Aeson
--   (
--     JSONRubric (JSON),
--     JSONRecord(..),
--     JSONSum(..),
--     Proxy(Proxy),
--     Aliased,
--     alias,
--     aliasListBegin
--     aliasListEnd,
--     aliases
--   )
-- import Data.Aeson
-- import Data.Aeson.Types
-- import GHC.Generics
-- import GHC.TypeLits
--
-- data Foo = Foo {aa :: Int, bb :: Bool, cc :: Char}
--   deriving (Read, Show, Eq, Generic)
--   deriving (FromJSON, ToJSON) via (JSONRecord "obj" Foo)
--
-- instance Aliased JSON Foo where
--   aliases =
--     aliasListBegin
--       $ alias (Proxy @"aa") "aax"
--       $ alias (Proxy @"bb") "bbx"
--       $ alias (Proxy @"cc") "ccx"
--       $ aliasListEnd
--
-- data Summy
--   = Aa Int
--   | Bb Bool
--   | Cc
--   deriving (Read, Show, Eq, Generic)
--   deriving (FromJSON, ToJSON) via (JSONSum "sum" Summy)
--
-- instance Aliased JSON Summy where
--   aliases =
--     aliasListBegin
--       $ alias (Proxy @"Aa") "Aax"
--       $ alias (Proxy @"Bb") "Bbx"
--       $ alias (Proxy @"Cc") "Ccx"
--       $ aliasListEnd
-- @
module ByOtherNames.Aeson
  ( -- * JSON helpers
    JSONRubric (..),
    JSONRecord (..),
    JSONSum (..),

    -- * Re-exports from ByOtherNames
    Aliased (aliases),
    aliasListBegin,
    alias,
    aliasListEnd,

    -- * Re-exports from Data.Aeson
    FromJSON,
    ToJSON,

    -- * Re-exports from Data.Proxy
    Proxy (..),
  )
where

import ByOtherNames
import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Functor.Compose
import Data.Kind
import Data.Text
import GHC.Generics
import GHC.TypeLits


-- type Parserlike :: (Type -> Type) -> Constraint
-- class (Applicative f, Alternative f) => Parserlike f where 
--     type RequiredAlias f :: Type
--     type RequiredConstraint f :: Type -> Constraint  
--     parseLeaf :: RequiredConstraint f a => RequiredAlias f -> f a

-- instance Parserlike v => Parserlike (S1 x (Rec0 v)) where
--   fieldParser (Field fieldName) = FieldParser \o -> M1 . K1 <$> explicitParseField parseJSON o fieldName

-- instance (FieldsFromJSON left, FieldsFromJSON right) => FieldsFromJSON (left :*: right) where
--   fieldParser (FieldTree left right) =
--     (:*:) <$> fieldParser left <*> fieldParser right


-- | Aliases for JSON serialization fall under this 'Rubric'.
-- The constructor 'JSON' is used as a type, with DataKinds.
data JSONRubric = JSON

-- | The aliases will be of type 'Data.Text'.
instance Rubric JSON where
  type AliasType JSON = Text


-- FromJSON
--
-- general machinery
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

-- applied to JSON
instance ParsingRubric JSON where 
  type RequiredConstraint JSON = FromJSON
  type ParserType JSON = JSONParser
  parseField fieldName = JSONParser \o ->
    explicitParseField parseJSON o fieldName
  parseBranch branchName = JSONParser \o ->
    explicitParseField parseJSON o branchName
  parseBranch0 branchName = JSONParser \o ->
    do 
        _ :: Value <- explicitParseField parseJSON o branchName
        return ()

newtype JSONParser a = JSONParser (Object -> Parser a)
  deriving (Functor, Applicative) via ((->) Object `Compose` Parser)

instance Alternative JSONParser where
    empty = JSONParser \_ -> Control.Applicative.empty
    JSONParser p1 <|> JSONParser p2 = JSONParser \o -> p1 o <|> p2 o

instance (KnownSymbol s, Aliased JSON r, Rep r ~ D1 x (C1 y prod), ParsingRubric JSON) => FromJSON (JSONRecord s r) where
  parseJSON v =
    let Record prod = aliases @JSONRubric @JSON @r
        JSONParser parser = parseWithRubric (Proxy @JSON) prod
     in JSONRecord . to <$> withObject (symbolVal (Proxy @s)) parser v

instance (KnownSymbol s, Aliased JSON r, Rep r ~ D1 x (left :+: right), ParsingRubric JSON) => FromJSON (JSONSum s r) where
  parseJSON v =
    let Sum branches = aliases @JSONRubric @JSON @r
     in JSONSum . to <$> withObject (symbolVal (Proxy @s)) (parseWithRubric (Proxy @JSON) branches) v

-- ToJSON
--
newtype FieldConverter a = FieldConverter (a -> [(Text, Value)])
  deriving newtype (Semigroup, Monoid)

type FieldsToJSON :: (Type -> Type) -> Constraint
class FieldsToJSON t where
  fieldConverter :: Aliases Text t -> FieldConverter (t x)

instance ToJSON v => FieldsToJSON (S1 x (Rec0 v)) where
  fieldConverter (Field fieldName) = FieldConverter \(M1 (K1 v)) -> [(fieldName, toJSON v)]

instance (FieldsToJSON left, FieldsToJSON right) => FieldsToJSON (left :*: right) where
  fieldConverter (FieldTree left right) =
    FieldConverter \(leftFields :*: rightFields) ->
      let FieldConverter leftConverter = fieldConverter left
          FieldConverter rightConverter = fieldConverter right
       in leftConverter leftFields ++ rightConverter rightFields

instance (Aliased JSON r, Rep r ~ D1 x (C1 y prod), FieldsToJSON prod) => ToJSON (JSONRecord s r) where
  toJSON (JSONRecord (from -> M1 (M1 a))) =
    let Record prod = aliases @JSONRubric @JSON @r
        FieldConverter fieldsToValues = fieldConverter prod
     in object (fieldsToValues a)

newtype BranchConverter a = BranchConverter (a -> Value)

type BranchesToJSON :: (Type -> Type) -> Constraint
class BranchesToJSON t where
  branchConverter :: Aliases Text t -> BranchConverter (t x)

instance ToJSON v => BranchesToJSON (C1 x (S1 y (Rec0 v))) where
  branchConverter (Branch fieldName) = BranchConverter \(M1 (M1 (K1 v))) -> object [(fieldName, toJSON v)]

instance BranchesToJSON (C1 x U1) where
  branchConverter (Branch fieldName) = BranchConverter \(M1 U1) -> object [(fieldName, Null)]

instance (BranchesToJSON left, BranchesToJSON right) => BranchesToJSON (left :+: right) where
  branchConverter (BranchTree left right) =
    BranchConverter \alternatives -> case alternatives of
      L1 leftBranch ->
        let BranchConverter leftConverter = branchConverter left
         in leftConverter leftBranch
      R1 rightBranch ->
        let BranchConverter rightConverter = branchConverter right
         in rightConverter rightBranch

instance (Aliased JSON r, Rep r ~ D1 x (left :+: right), BranchesToJSON (left :+: right)) => ToJSON (JSONSum s r) where
  toJSON (JSONSum (from -> M1 a)) =
    let Sum branches = aliases @JSONRubric @JSON @r
        BranchConverter branchesToValues = branchConverter branches
     in branchesToValues a

-- | Helper newtype for deriving 'FromJSON' and 'ToJSON' for record types,
-- using DerivingVia.
--
-- The 'Symbol' type parameter is used in parse error messages.
type JSONRecord :: Symbol -> Type -> Type
newtype JSONRecord s r = JSONRecord r

-- | Helper newtype for deriving 'FromJSON' and 'ToJSON' for sum types,
-- using DerivingVia.
--
-- The 'Symbol' type parameter is used in parse error messages.
type JSONSum :: Symbol -> Type -> Type
newtype JSONSum s r = JSONSum r
