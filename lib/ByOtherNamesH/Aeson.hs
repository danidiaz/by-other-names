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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


module ByOtherNamesH.Aeson
  ( -- * JSON helpers
    JSONRubric (..),
    JSONRecord (..),
    FromToJSON (..),
    fromToJSON,
    -- ** Advanced JSON helpers
    GeneralJSONRecord (..),
    -- * Re-exports from ByOtherNames
    Aliased (aliases),
    aliasListBegin,
    alias,
    aliasListEnd,
    SlotList,
    singleSlot,
    slot,
    slotListEnd,
    -- * Re-exports from Data.Aeson
    FromJSON,
    ToJSON,

  )
where

import ByOtherNamesH
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
import Data.Functor.Identity
import Data.Functor.Const

-- | Aliases for JSON serialization fall under this 'Rubric'.
-- The constructor 'JSON' is used as a type, with DataKinds.
data JSONRubric = JSON

-- | The aliases will be of type "Data.Aeson.Key".
instance Rubric JSON where
  type AliasType JSON = Key
  type WrapperType JSON = FromToJSON

data FromToJSON v = FromToJSON { 
    parseJSON' :: Value -> Parser v, 
    toJSON' :: v -> Value
  }

fromToJSON :: (ToJSON v, FromJSON v) => FromToJSON v 
fromToJSON = FromToJSON { parseJSON' = parseJSON, toJSON' = toJSON}

type JSONRecord :: Symbol -> Type -> Type
newtype JSONRecord objectName r = JSONRecord r

deriving via (GeneralJSONRecord 'JSON objectName r) instance (KnownSymbol objectName, Aliased 'JSON r, GRecord (Rep r)) => FromJSON (JSONRecord objectName r) 
deriving via (GeneralJSONRecord 'JSON objectName r) instance (Aliased 'JSON r, GRecord (Rep r)) => ToJSON (JSONRecord objectName r)

type GeneralJSONRecord :: rubric -> Symbol -> Type -> Type
newtype GeneralJSONRecord rubric objectName r = GeneralJSONRecord r

instance (KnownSymbol objectName, 
  Rubric rubric, 
  Aliased rubric r, 
  AliasType rubric ~ Key, 
  WrapperType rubric ~ FromToJSON, 
  GRecord (Rep r)) 
  => FromJSON (GeneralJSONRecord rubric objectName r) where
  parseJSON v =
    let FieldParser parser =
          gToRecord 
            (aliases @_ @rubric @r)
            (\fieldName (FromToJSON {parseJSON'}) -> FieldParser (\o -> explicitParseField parseJSON' o fieldName))
        objectName = symbolVal (Proxy @objectName)
     in GeneralJSONRecord . to <$> withObject objectName parser v

newtype FieldParser a = FieldParser (Object -> Parser a)
  deriving (Functor, Applicative) via ((->) Object `Compose` Parser)

instance (Rubric rubric, 
  Aliased rubric r, 
  AliasType rubric ~ Key, 
  WrapperType rubric ~ FromToJSON, 
  GRecord (Rep r)) => ToJSON (GeneralJSONRecord rubric objectName r) where
  toJSON (GeneralJSONRecord o) = do
    let plainRecord = gFromRecord $ from @r o
        deserializers = aliases @_ @rubric @r
        combineAliases _ k = k
        combineWrappers (Identity v) (FromToJSON {toJSON'}) = Const (toJSON' v)
        eachFieldRendered = gBiliftA2RecordAliases combineAliases combineWrappers plainRecord deserializers
        Const objects = gToRecord  eachFieldRendered (\a (Const v) -> Const [(a,v)])
    object objects
