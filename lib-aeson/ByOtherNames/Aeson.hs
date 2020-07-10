{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
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

data JSON = JSON

instance Rubric JSON where
    type AliasesType JSON = Text


newtype FieldParser a = FieldFromJSON (Object -> Parser a) 
                      deriving (Functor,Applicative) via ((->) Object `Compose` Parser)

-- type FieldFromJSON :: Type -> Constraint
-- class FieldParser t where
--     fieldParser :: 
