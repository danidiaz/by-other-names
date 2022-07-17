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
instance (KnownSymbol s, Aliased JSON r, GSum FromJSON (Rep r)) => FromJSON (JSONSum s r) where
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
        parserForObject o = asum $ map (($ o) . runBranchParser) parsers
     in JSONSum . to <$> withObject (symbolVal (Proxy @s)) parserForObject v
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
instance (KnownSymbol s, Aliased JSON r, GRecord FromJSON (Rep r)) => FromJSON (JSONRecord s r) where
  parseJSON v =
    let FieldParser parser = gToRecord @FromJSON (aliases @JSONRubric @JSON @r) 
          (\fieldName -> FieldParser (\o ->explicitParseField parseJSON o fieldName))
        objectName = symbolVal (Proxy @s)
     in JSONRecord . to <$> withObject objectName parser v
newtype FieldParser a = FieldParser (Object -> Parser a)
  deriving (Functor, Applicative) via ((->) Object `Compose` Parser)


--
--
instance (Aliased JSON r, GSum ToJSON (Rep r)) => ToJSON (JSONSum s r) where
  toJSON (JSONSum o) =
    let (key, slots) = gFromSum @ToJSON @(Rep r) @Key @Value @Value (aliases @JSONRubric @JSON @r) toJSON (from @r o)
     in case slots of
          [] -> object [(key, Null)]
          [x] -> object [(key, toJSON x)]
          xs -> object [(key, toJSON xs)]

--
--
instance (Aliased JSON r, GRecord ToJSON (Rep r)) => ToJSON (JSONRecord s r) where
  toJSON (JSONRecord o) =
    object $ gFromRecord @ToJSON @(Rep r) @Key (aliases @JSONRubric @JSON @r) (\a v -> (a, toJSON v)) (from @r o)