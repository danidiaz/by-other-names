{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module ByOtherNames.Aeson
  ( JSONRubric (..),
    JSONRecord (..),
  )
where

import ByOtherNames
import Data.Aeson
import Data.Aeson.Types
import Data.Functor.Compose
import Data.Kind
import Data.Text
import GHC.Generics
import GHC.TypeLits

data JSONRubric = JSON

instance Rubric JSON where
  type ForRubric JSON = Text

type JSONRecord :: Symbol -> Type -> Type
newtype JSONRecord s r = JSONRecord r

--
--
newtype FieldParser a = FieldParser {getFieldParser :: Object -> Parser a}
  deriving (Functor, Applicative) via ((->) Object `Compose` Parser)

type FieldsFromJSON :: (Type -> Type) -> Constraint
class FieldsFromJSON t where
  fieldParser :: Aliases Text t -> FieldParser (t x)

instance FromJSON v => FieldsFromJSON (S1 x (Rec0 v)) where
  fieldParser (Leaf fieldName) = FieldParser \o -> M1 . K1 <$> explicitParseField parseJSON o fieldName

instance (FieldsFromJSON left, FieldsFromJSON right) => FieldsFromJSON (left :*: right) where
  fieldParser (Prod left right) =
    (:*:) <$> fieldParser left <*> fieldParser right


instance (KnownSymbol s, Aliased JSON r, Rep r ~ D1 x (C1 y prod), FieldsFromJSON prod) => FromJSON (JSONRecord s r) where
  parseJSON v =
    let ByOtherNames.Object prod = aliases @JSONRubric @JSON @r
        FieldParser parser = fieldParser prod
     in JSONRecord . to . M1 . M1 <$> withObject (symbolVal (Proxy @s)) parser v

--
--
newtype FieldsToValues a = FieldsToValues {getFieldsToValues :: a -> [(Text, Value)]}
  deriving newtype (Semigroup,Monoid)

type FieldsToJSON :: (Type -> Type) -> Constraint
class FieldsToJSON t where
  fieldConverter :: Aliases Text t -> FieldsToValues (t x)

instance ToJSON v => FieldsToJSON (S1 x (Rec0 v)) where
  fieldConverter (Leaf fieldName) = FieldsToValues \(M1 (K1 v)) -> [(fieldName,toJSON v)]

instance (FieldsToJSON left, FieldsToJSON right) => FieldsToJSON (left :*: right) where
  fieldConverter (Prod left right) =
     FieldsToValues \(leftFields :*: rightFields) ->  
          let FieldsToValues leftConverter = fieldConverter left 
              FieldsToValues rightConverter = fieldConverter right
           in leftConverter leftFields ++ rightConverter rightFields

instance (Aliased JSON r, Rep r ~ D1 x (C1 y prod), FieldsToJSON prod) => ToJSON (JSONRecord s r) where
  toJSON (JSONRecord (from -> M1 (M1 a))) =
    let ByOtherNames.Object prod = aliases @JSONRubric @JSON @r
        FieldsToValues fieldsToValues = fieldConverter prod
     in object (fieldsToValues a)

