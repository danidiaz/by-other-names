{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
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
--       $ alias (Proxy \@"aa") "aax"
--       $ alias (Proxy \@"bb") "bbx"
--       $ alias (Proxy \@"cc") "ccx"
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
--       $ alias (Proxy \@"Aa") "Aax"
--       $ alias (Proxy \@"Bb") "Bbx"
--       $ alias (Proxy \@"Cc") "Ccx"
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
import GHC.Generics
import GHC.TypeLits

-- | Aliases for JSON serialization fall under this 'Rubric'.
-- The constructor 'JSON' is used as a type, with DataKinds.
data JSONRubric = JSON

-- | The aliases will be of type 'Data.Text'.
instance Rubric JSON where
  type AliasType JSON = Key

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

--
--
newtype FieldParser a = FieldParser (Object -> Parser a)
  deriving (Functor, Applicative) via ((->) Object `Compose` Parser)

type FieldsFromJSON :: (Type -> Type) -> Constraint
class FieldsFromJSON t where
  fieldParser :: Aliases Key t -> FieldParser (t x)

instance FromJSON v => FieldsFromJSON (S1 x (Rec0 v)) where
  fieldParser (Field fieldName) = FieldParser \o -> M1 . K1 <$> explicitParseField parseJSON o fieldName

instance (FieldsFromJSON left, FieldsFromJSON right) => FieldsFromJSON (left :*: right) where
  fieldParser (FieldTree left right) =
    (:*:) <$> fieldParser left <*> fieldParser right

instance (KnownSymbol s, Aliased JSON r, Rep r ~ D1 x (C1 y prod), FieldsFromJSON prod) => FromJSON (JSONRecord s r) where
  parseJSON v =
    let Record prod = aliases @JSONRubric @JSON @r
        FieldParser parser = fieldParser prod
     in JSONRecord . to . M1 . M1 <$> withObject (symbolVal (Proxy @s)) parser v

--
--
newtype FieldConverter a = FieldConverter (a -> [(Key, Value)])
  deriving newtype (Semigroup, Monoid)

type FieldsToJSON :: (Type -> Type) -> Constraint
class FieldsToJSON t where
  fieldConverter :: Aliases Key t -> FieldConverter (t x)

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

--
--

type BranchesFromJSON :: (Type -> Type) -> Constraint
class BranchesFromJSON t where
  branchParser :: Aliases Key t -> Object -> Parser (t x)

instance BranchesFromJSON (C1 x U1) where
  branchParser (Branch fieldName) = \o ->
    do
      (_ :: Value) <- o .: fieldName
      pure $ M1 U1

instance FromJSON v => BranchesFromJSON (C1 x (S1 y (Rec0 v))) where
  branchParser (Branch fieldName) = \o ->
    do
      value <- o .: fieldName
      M1 . M1 . K1 <$> parseJSON value

instance ToProductInBranch (left :*: right) => BranchesFromJSON (C1 x (left :*: right)) where
  branchParser (Branch fieldName) = \o ->
    do
      valueList <- o .: fieldName
      (prod, _) <- fromValueList valueList
      pure (M1 prod)

instance (BranchesFromJSON left, BranchesFromJSON right) => BranchesFromJSON (left :+: right) where
  branchParser (BranchTree left right) = \o ->
    (L1 <$> branchParser left o) <|> (R1 <$> branchParser right o)

instance (KnownSymbol s, Aliased JSON r, Rep r ~ D1 x (left :+: right), BranchesFromJSON (left :+: right)) => FromJSON (JSONSum s r) where
  parseJSON v =
    let Sum branches = aliases @JSONRubric @JSON @r
     in JSONSum . to . M1 <$> withObject (symbolVal (Proxy @s)) (branchParser branches) v

--
--
newtype BranchConverter a = BranchConverter (a -> Value)

type BranchesToJSON :: (Type -> Type) -> Constraint
class BranchesToJSON t where
  branchConverter :: Aliases Key t -> BranchConverter (t x)

instance BranchesToJSON (C1 x U1) where
  branchConverter (Branch fieldName) = BranchConverter \(M1 U1) -> object [(fieldName, Null)]

instance ToJSON v => BranchesToJSON (C1 x (S1 y (Rec0 v))) where
  branchConverter (Branch fieldName) = BranchConverter \(M1 (M1 (K1 v))) -> object [(fieldName, toJSON v)]

instance FromProductInBranch (left :*: right) => BranchesToJSON (C1 x (left :*: right)) where
  branchConverter (Branch fieldName) = BranchConverter \(M1 prod) -> object [(fieldName, toJSON (toValueList prod))]

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

type FromProductInBranch :: (Type -> Type) -> Constraint
class FromProductInBranch x where
  toValueList :: x z -> [Value]

instance ToJSON v => FromProductInBranch (S1 x (Rec0 v)) where
  toValueList (M1 (K1 v)) = [toJSON v]

instance (FromProductInBranch left, FromProductInBranch right) => FromProductInBranch (left :*: right) where
  toValueList (left :*: right) = toValueList left ++ toValueList right

type ToProductInBranch :: (Type -> Type) -> Constraint
class ToProductInBranch x where
  fromValueList :: [Value] -> Parser (x z, [Value])

instance FromJSON r => ToProductInBranch (S1 x (Rec0 r)) where
  fromValueList vs = case vs of
    [] -> parseFail "not enough field values for branch"
    v : vs -> do
      r <- parseJSON v
      pure (M1 (K1 r), vs)

instance (ToProductInBranch left, ToProductInBranch right) => ToProductInBranch (left :*: right) where
  fromValueList vs0 = do
    (left, vs1) <- fromValueList vs0
    (right, vs2) <- fromValueList vs1
    pure (left :*: right, vs2)