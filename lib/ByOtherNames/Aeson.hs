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
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

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
-- Example of use for a record type:
--
-- >>> :{
-- data Foo = Foo {aa :: Int, bb :: Bool, cc :: Char}
--   deriving (Read, Show, Eq, Generic)
--   deriving (FromJSON, ToJSON) via (JSONRecord "obj" Foo)
-- instance Aliased JSON Foo where
--   aliases =
--     aliasListBegin
--       $ alias @"aa" "aax"
--       $ alias @"bb" "bbx"
--       $ alias @"cc" "ccx"
--       $ aliasListEnd
-- :}
--
-- Example of use for a sum type:
--
-- >>> :{
-- data Summy
--   = Aa Int
--   | Bb Bool
--   | Cc
--   deriving (Read, Show, Eq, Generic)
--   deriving (FromJSON, ToJSON) via (JSONSum "sum" Summy)
-- instance Aliased JSON Summy where
--   aliases =
--     aliasListBegin
--       $ alias @"Aa" "Aax"
--       $ alias @"Bb" "Bbx"
--       $ alias @"Cc" "Ccx"
--       $ aliasListEnd
-- :}
--
-- Some limitations:
-- 
-- - Fields in branches of sum types can't have selectors. When there is more than one field in a branch, they are parsed as a JSON Array.
-- 
-- - For sum types, only the "object with a single key consisting in the branch tag" style of serialization is supported.
--
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
import Data.Foldable

-- | Aliases for JSON serialization fall under this 'Rubric'.
-- The constructor 'JSON' is used as a type, with DataKinds.
data JSONRubric = JSON

-- | The aliases will be of type "Data.Aeson.Key".
instance Rubric JSON where
  type AliasType JSON = Key

-- | Helper newtype for deriving 'FromJSON' and 'ToJSON' for record types,
-- using DerivingVia.
--
-- The @objectName@ type parameter of kind 'Symbol' is used in parse error messages.
type JSONRecord :: Symbol -> Type -> Type
newtype JSONRecord objectName r = JSONRecord r

-- | Helper newtype for deriving 'FromJSON' and 'ToJSON' for sum types,
-- using DerivingVia.
--
-- The 'Symbol' type parameter is used in parse error messages.
type JSONSum :: Symbol -> Type -> Type
newtype JSONSum objectName r = JSONSum r

--
--
instance (KnownSymbol objectName, Aliased JSON r, GSum FromJSON (Rep r)) => FromJSON (JSONSum objectName r) where
  parseJSON v =
    let parsers = gToSum @FromJSON (aliases @JSONRubric @JSON @r) 
          (\a -> \case 
              ZeroSlots v -> BranchParser \o -> do
                Null :: Value <- o .: a
                pure v
              SingleSlot p -> BranchParser \o -> do
                value <- o .: a
                runProductInBranchParser1 p value
              ManySlots p -> BranchParser \o -> do
                valueList <- o .: a
                (prod, _) <- runProductInBranchParser p valueList
                pure prod
            ) 
          (ProductInBranchParser1 parseJSON) 
          (ProductInBranchParser \case 
            [] -> parseFail "not enough field values for branch"
            v : vs -> do
              r <- parseJSON v
              pure (r, vs))
        parserForObject o = asum $ fmap (($ o) . runBranchParser) parsers
     in JSONSum . to <$> withObject (symbolVal (Proxy @objectName)) parserForObject v
newtype BranchParser v = BranchParser { runBranchParser :: Object -> Parser v}
  deriving stock Functor

newtype ProductInBranchParser1 v = ProductInBranchParser1 { runProductInBranchParser1 :: Value -> Parser v }
  deriving stock Functor
  deriving Applicative via (Compose ((->) Value) Parser)

newtype ProductInBranchParser v = ProductInBranchParser { runProductInBranchParser :: [Value] -> Parser (v, [Value]) }
  deriving stock Functor

instance Applicative ProductInBranchParser where
  pure v = ProductInBranchParser \vs -> pure (v, vs)
  ProductInBranchParser left <*> ProductInBranchParser right =
    ProductInBranchParser \vs0 -> do
      (f, vs1) <- left vs0
      (x, vs2) <- right vs1
      pure (f x, vs2)


--
--
instance (KnownSymbol objectName, Aliased JSON r, GRecord FromJSON (Rep r)) => FromJSON (JSONRecord objectName r) where
  parseJSON v =
    let FieldParser parser = gToRecord @FromJSON (aliases @JSONRubric @JSON @r) 
          (\fieldName -> FieldParser (\o ->explicitParseField parseJSON o fieldName))
        objectName = symbolVal (Proxy @objectName)
     in JSONRecord . to <$> withObject objectName parser v
newtype FieldParser a = FieldParser (Object -> Parser a)
  deriving (Functor, Applicative) via ((->) Object `Compose` Parser)


--
--
instance (Aliased JSON r, GSum ToJSON (Rep r)) => ToJSON (JSONSum objectName r) where
  toJSON (JSONSum o) =
    let (key, slots) = gFromSum @ToJSON @(Rep r) @Key @Value @Value (aliases @JSONRubric @JSON @r) toJSON (from @r o)
     in case slots of
          [] -> object [(key, Null)]
          [x] -> object [(key, toJSON x)]
          xs -> object [(key, toJSON xs)]

--
--
instance (Aliased JSON r, GRecord ToJSON (Rep r)) => ToJSON (JSONRecord objectName r) where
  toJSON (JSONRecord o) =
    object $ Data.Foldable.toList $ gFromRecord @ToJSON @(Rep r) @Key (aliases @JSONRubric @JSON @r) (\a v -> (a, toJSON v)) (from @r o)

-- $setup
--
-- >>> :set -XBlockArguments
-- >>> :set -XTypeApplications
-- >>> :set -XDerivingStrategies
-- >>> :set -XDerivingVia
-- >>> :set -XDataKinds
-- >>> :set -XMultiParamTypeClasses
-- >>> :set -XDeriveGeneric
-- >>> :set -XOverloadedStrings
-- >>> import ByOtherNames.Aeson
-- >>> import Data.Aeson
-- >>> import Data.Aeson.Types
-- >>> import GHC.Generics
-- >>> import GHC.TypeLits