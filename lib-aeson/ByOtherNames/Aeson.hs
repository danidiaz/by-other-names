{-# LANGUAGE TypeFamilies #-}
module ByOtherNames.Aeson
  ( module ByOtherNames,
    JSON,
  )
where

import ByOtherNames
import Data.Aeson
import Data.Text

data JSON = JSON

instance Rubric JSON where
    type Alias JSON = Text
