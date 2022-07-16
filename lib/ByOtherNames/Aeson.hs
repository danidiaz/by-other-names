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
import Data.Proxy

-- | Aliases for JSON serialization fall under this 'Rubric'.
-- The constructor 'JSON' is used as a type, with DataKinds.
data JSONRubric = JSON

-- | The aliases will be of type "Data.Aeson.Key".
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

--
--
instance (Aliased JSON r, GFromSum ToJSON (Rep r)) => ToJSON (JSONSum s r) where
  toJSON (JSONSum o) =
    gFromSum @ToJSON @(Rep r) @Key @Value @Value (aliases @JSONRubric @JSON @r)
      (\key slots -> case slots of
        [] -> object [(key, Null)]
        [x] -> object [(key, toJSON x)]
        xs -> object [(key, toJSON xs)]) toJSON (from @r o)

--
--
instance (Aliased JSON r, GFromProduct ToJSON (Rep r)) => ToJSON (JSONRecord s r) where
  toJSON (JSONRecord o) =
    object $ gFromProduct @ToJSON @(Rep r) @Key (aliases @JSONRubric @JSON @r) (\a v -> (a, toJSON v)) (from @r o)