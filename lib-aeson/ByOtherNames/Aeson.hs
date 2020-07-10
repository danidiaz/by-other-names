{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module ByOtherNames.Aeson
  ( module ByOtherNames,
    JSON,
  )
where

import ByOtherNames
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Data.Functor.Compose
import Data.Kind
import GHC.Generics

data JSON = JSON

instance Rubric JSON where
    type AliasesType JSON = Text


newtype FieldParser a = FieldParser (Object -> Parser a) 
                      deriving (Functor,Applicative) via ((->) Object `Compose` Parser)

type FieldFromJSON :: (Type -> Type) -> Constraint
class FieldFromJSON t where
    fieldParser :: Aliases Text t -> FieldParser (t x)

instance FromJSON v => FieldFromJSON (S1 ('MetaSel (Just symbol) x y z) (Rec0 v)) where
    fieldParser (Leaf fieldName) = FieldParser \o -> M1 . K1 <$> explicitParseField parseJSON o fieldName


