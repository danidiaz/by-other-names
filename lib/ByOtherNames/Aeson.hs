{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
-- Some observations:
--
-- - Fields in branches of sum types can't have selectors. When there is more than one field in a branch, they are parsed as a JSON Array.
--
-- - The "object with a single key consisting in the branch tag" style of serialization is used.
--
-- Sometimes we have enum-like sum types without any fields, and want to
-- serialize them to simple JSON strings, instead of to objects. In that case,
-- we can do the following:
--
-- >>> :{
-- data Enumy
--   = Xx
--   | Yy
--   | Zz
--   deriving (Read, Show, Eq, Generic)
--   deriving (FromJSON, ToJSON) via (JSONEnum Enumy)
-- instance Aliased JSON Enumy where
--   aliases =
--     aliasListBegin
--       $ alias @"Xx" "x"
--       $ alias @"Yy" "y"
--       $ alias @"Zz" "z"
--       $ aliasListEnd
-- :}
--
--

module ByOtherNames.Aeson
  ( -- * JSON helpers
    JSONRubric (..),
    JSONRecord (..),
    JSONSum (..),
    JSONEnum (..),
    -- ** Advanced JSON helpers
    GeneralJSONRecord (..),
    GeneralJSONSum (..),
    GeneralJSONEnum (..),
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
import Data.Aeson
import Data.Aeson.Key (fromText, toText)
import Data.Aeson.Types
import Data.Foldable
import Data.Functor.Compose
import Data.Kind
import Data.Proxy
import Data.Void
import GHC.Generics
import GHC.TypeLits
import ByOtherNames.Constraint

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

deriving via (GeneralJSONRecord 'JSON objectName r) instance (KnownSymbol objectName, Aliased 'JSON r, GRecord FromJSON (Rep r)) => FromJSON (JSONRecord objectName r) 
deriving via (GeneralJSONRecord 'JSON objectName r) instance (Aliased 'JSON r, GRecord ToJSON (Rep r)) => ToJSON (JSONRecord objectName r)

-- | Helper newtype for deriving 'FromJSON' and 'ToJSON' for sum types,
-- using DerivingVia.
--
-- The 'Symbol' type parameter is used in parse error messages.
type JSONSum :: Symbol -> Type -> Type
newtype JSONSum objectName r = JSONSum r

deriving via (GeneralJSONSum 'JSON objectName r) instance (KnownSymbol objectName, Aliased 'JSON r, GSum FromJSON (Rep r)) => FromJSON (JSONSum objectName r) 
deriving via (GeneralJSONSum 'JSON objectName r) instance (Aliased 'JSON r, GSum ToJSON (Rep r)) => ToJSON (JSONSum objectName r)

-- | Helper newtype for deriving 'FromJSON' and 'ToJSON' for enum-like sum types,
-- using DerivingVia.
--
-- Each constructor is serialized to a JSON string.
type JSONEnum :: Type -> Type
newtype JSONEnum r = JSONEnum r

deriving via (GeneralJSONEnum 'JSON r) instance (Aliased 'JSON r, GSum Impossible (Rep r)) => FromJSON (JSONEnum r) 
deriving via (GeneralJSONEnum 'JSON r) instance (Aliased 'JSON r, GSum Impossible (Rep r)) => ToJSON (JSONEnum r)

newtype EnumBranchParser v = EnumBranchParser {runEnumBranchParser :: Value -> Parser v}
  deriving stock (Functor)

newtype BranchParser v = BranchParser {runBranchParser :: Object -> Parser v}
  deriving stock (Functor)

newtype ProductInBranchParser1 v = ProductInBranchParser1 {runProductInBranchParser1 :: Value -> Parser v}
  deriving stock (Functor)
  deriving (Applicative) via (Compose ((->) Value) Parser)

newtype ProductInBranchParser v = ProductInBranchParser {runProductInBranchParser :: [Value] -> Parser (v, [Value])}
  deriving stock (Functor)

instance Applicative ProductInBranchParser where
  pure v = ProductInBranchParser \vs -> pure (v, vs)
  ProductInBranchParser left <*> ProductInBranchParser right =
    ProductInBranchParser \vs0 -> do
      (f, vs1) <- left vs0
      (x, vs2) <- right vs1
      pure (f x, vs2)


--
--

newtype FieldParser a = FieldParser (Object -> Parser a)
  deriving (Functor, Applicative) via ((->) Object `Compose` Parser)

-- | A more flexible version of 'JSONSum' that lets you use any 'Rubric' whose
-- 'AliasType' is 'Data.Aeson.Key'.
-- 
-- It allows deriving 'FromJSON' and 'ToJSON' for a newtype, using the generic
-- 'Rep' and the aliases of the underlying type, but __without__ defining
-- 'FromJSON' and 'ToJSON' instances for the underlying type.
-- 
-- >>> :{
-- data Summy
--   = Aa Int
--   | Bb Bool
--   | Cc
--   deriving (Read, Show, Eq, Generic)
-- data JSONLocal
-- -- We define a local rubric type to avoid colliding "Aliased" instances over Foo.
-- instance Rubric JSONLocal where
--   type AliasType JSONLocal = Key
-- instance Aliased JSONLocal Summy where
--   aliases =
--     aliasListBegin
--       $ alias @"Aa" "Aax"
--       $ alias @"Bb" "Bbx"
--       $ alias @"Cc" "Cc1"
--       $ aliasListEnd
-- newtype SummyN = SummyN Summy
--     deriving (FromJSON, ToJSON) via (GeneralJSONSum JSONLocal "obj" Summy)
-- :}
--
--
type GeneralJSONSum :: rubric -> Symbol -> Type -> Type
newtype GeneralJSONSum rubric objectName r = GeneralJSONSum r

instance (
  KnownSymbol objectName, 
  Rubric rubric, 
  Aliased rubric r, 
  AliasType rubric ~ Key, 
  GSum FromJSON (Rep r)) => FromJSON (GeneralJSONSum rubric objectName r) where
  parseJSON v =
    let parsers =
          gToSum @FromJSON
            (aliases @_ @rubric @r)
            ( \a -> \case
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
            ( ProductInBranchParser \case
                [] -> parseFail "not enough field values for branch"
                v : vs -> do
                  r <- parseJSON v
                  pure (r, vs)
            )
        parserForObject o = asum $ fmap (($ o) . runBranchParser) parsers
     in GeneralJSONSum . to <$> withObject (symbolVal (Proxy @objectName)) parserForObject v


instance (
  Rubric rubric, 
  Aliased rubric r, 
  AliasType rubric ~ Key, 
  GSum ToJSON (Rep r)) => ToJSON (GeneralJSONSum rubric objectName r) where
  toJSON (GeneralJSONSum o) =
    let (key, slots) = gFromSum @ToJSON @(Rep r) @Key @Value @Value (aliases @_ @rubric @r) toJSON (from @r o)
     in case slots of
          [] -> object [(key, Null)]
          [x] -> object [(key, toJSON x)]
          xs -> object [(key, toJSON xs)]


-- | A more flexible version of 'JSONRecord' that lets you use any 'Rubric' whose
-- 'AliasType' is 'Data.Aeson.Key'.
-- 
-- It allows deriving 'FromJSON' and 'ToJSON' for a newtype, using the generic
-- 'Rep' and the aliases of the underlying type, but __without__ defining
-- 'FromJSON' and 'ToJSON' instances for the underlying type.
-- 
-- >>> :{
-- data Foo = Foo {aa :: Int, bb :: Bool, cc :: Char}
--   deriving (Read, Show, Eq, Generic)
-- data JSONLocal
-- -- We define a local rubric type to avoid colliding "Aliased" instances over Foo.
-- instance Rubric JSONLocal where
--   type AliasType JSONLocal = Key
-- instance Aliased JSONLocal Foo where
--   aliases =
--     aliasListBegin
--       $ alias @"aa" "aax"
--       $ alias @"bb" "bbx"
--       $ alias @"cc" "ccx"
--       $ aliasListEnd
-- newtype FooN = FooN Foo
--     deriving (FromJSON, ToJSON) via (GeneralJSONRecord JSONLocal "obj" Foo)
-- :}
--
--
type GeneralJSONRecord :: rubric -> Symbol -> Type -> Type
newtype GeneralJSONRecord rubric objectName r = GeneralJSONRecord r

instance (KnownSymbol objectName, 
  Rubric rubric, 
  Aliased rubric r, 
  AliasType rubric ~ Key, 
  GRecord FromJSON (Rep r)) 
  => FromJSON (GeneralJSONRecord rubric objectName r) where
  parseJSON v =
    let FieldParser parser =
          gToRecord @FromJSON
            (aliases @_ @rubric @r)
            (\fieldName -> FieldParser (\o -> explicitParseField parseJSON o fieldName))
        objectName = symbolVal (Proxy @objectName)
     in GeneralJSONRecord . to <$> withObject objectName parser v

instance (Rubric rubric, 
  AliasType rubric ~ Key, 
  Aliased rubric r, 
  GRecord ToJSON (Rep r)) => ToJSON (GeneralJSONRecord rubric objectName r) where
  toJSON (GeneralJSONRecord o) =
    object $ Data.Foldable.toList $ gFromRecord @ToJSON @(Rep r) @Key (aliases @_ @rubric @r) (\a v -> (a, toJSON v)) (from @r o)

-- | A more flexible version of 'JSONEnum' that lets you use any 'Rubric' whose
-- 'AliasType' is 'Data.Aeson.Key'.
-- 
-- It allows deriving 'FromJSON' and 'ToJSON' for a newtype, using the generic
-- 'Rep' and the aliases of the underlying type, but __without__ defining
-- 'FromJSON' and 'ToJSON' instances for the underlying type.
-- 
-- >>> :{
-- data Enumy
--   = Xx
--   | Yy
--   | Zz
--   deriving (Read, Show, Eq, Generic)
-- data JSONLocal
-- -- We define a local rubric type to avoid colliding "Aliased" instances over Enumy.
-- instance Rubric JSONLocal where
--   type AliasType JSONLocal = Key
-- instance Aliased JSONLocal Enumy where
--   aliases =
--     aliasListBegin
--       $ alias @"Xx" "x"
--       $ alias @"Yy" "y"
--       $ alias @"Zz" "z"
--       $ aliasListEnd
-- -- We use the underlying Enumy type in DerivingVia.
-- newtype EnumyN = EnumyN Enumy
--     deriving (FromJSON, ToJSON) via (GeneralJSONEnum JSONLocal Enumy)
-- :}
--
--
type GeneralJSONEnum :: rubric -> Type -> Type
newtype GeneralJSONEnum rubric r = GeneralJSONEnum r

--
--
instance (
  Rubric rubric, 
  AliasType rubric ~ Key, 
  Aliased rubric r, 
  GSum Impossible (Rep r)) => FromJSON (GeneralJSONEnum rubric r) where
  parseJSON v =
    let parsers =
          gToSum @Impossible
            (aliases @_ @rubric @r)
            ( \a -> \case
                ZeroSlots x -> EnumBranchParser \case
                  String a' | a == fromText a' -> pure x
                  _ -> mempty
                SingleSlot _ -> EnumBranchParser mempty
                ManySlots _ -> EnumBranchParser mempty
            )
            Proxy
            Proxy
        parserForValue v = asum $ fmap (($ v) . runEnumBranchParser) parsers
     in GeneralJSONEnum . to <$> parserForValue v

instance (
  Rubric rubric, 
  AliasType rubric ~ Key, 
  Aliased rubric r, 
  GSum Impossible (Rep r)) 
  => ToJSON (GeneralJSONEnum rubric r) where
  toJSON (GeneralJSONEnum o) =
    let (key, slots) = gFromSum @Impossible @(Rep r) @Key @Value @Value (aliases @_ @rubric @r) absurd (from @r o)
     in case slots of
          [] -> String (toText key)
          [_] -> error "never happens"
          _ -> error "never happens"

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
-- >>> :set -XTypeFamilies
-- >>> :set -XDerivingStrategies
-- >>> :set -XDerivingVia
-- >>> import ByOtherNames.Aeson
-- >>> import Data.Aeson
-- >>> import Data.Aeson.Types
-- >>> import GHC.Generics
-- >>> import GHC.TypeLits
