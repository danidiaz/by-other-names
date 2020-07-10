{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module ByOtherNames.Aeson
  ( 
    JSON(..),
    FromJSONRecord(..)
  )
where

import ByOtherNames
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Data.Functor.Compose
import Data.Kind
import GHC.Generics
import GHC.TypeLits

data JSON = JSON

instance Rubric 'JSON where
    type AliasesType 'JSON = Text

newtype FieldParser a = FieldParser { getFieldParser :: Object -> Parser a }
                      deriving (Functor,Applicative) via ((->) Object `Compose` Parser)

type FieldFromJSON :: (Type -> Type) -> Constraint
class FieldFromJSON t where
    fieldParser :: Aliases Text t -> FieldParser (t x)

instance FromJSON v => FieldFromJSON (S1 x (Rec0 v)) where
    fieldParser (Leaf fieldName) = FieldParser \o -> M1 . K1 <$> explicitParseField parseJSON o fieldName

instance (FieldFromJSON left, FieldFromJSON right) => FieldFromJSON (left :*: right) where
    fieldParser (Prod left right) = 
        (:*:) <$> fieldParser left <*> fieldParser right

type FromJSONRecord :: Symbol -> Type -> Type
newtype FromJSONRecord s r = FromJSONRecord r
      
instance (KnownSymbol s, Aliased 'JSON r, Rep r ~ D1 x (C1 y prod), FieldFromJSON prod) => FromJSON (FromJSONRecord s r) where
    parseJSON v = 
        let ByOtherNames.Object prod = aliases @JSON @'JSON @r 
            FieldParser parser = fieldParser prod 
         in FromJSONRecord . to . M1 . M1 <$> withObject (symbolVal (Proxy @s)) parser v



