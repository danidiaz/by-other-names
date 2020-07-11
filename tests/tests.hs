{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import ByOtherNames
import ByOtherNames.Aeson
import Data.Aeson
import GHC.Generics
import GHC.TypeLits

data Foo = Foo {aa :: Int, bb :: Bool, cc :: Char}
  deriving (Read, Show, Generic)
  deriving (FromJSON,ToJSON) via (JSONRecord "obj" Foo)

instance Aliased JSON Foo where
  aliases =
    fieldAliases
      $ alias (Proxy @"aa") "foo"
      $ alias (Proxy @"bb") "bar"
      $ alias (Proxy @"cc") "baz"
      $ aliasListEnd
foo :: Foo
foo = Foo 0 False 'f'

data Summy = Aa Int
           | Bb Bool
           | Cc
  deriving (Read, Show, Generic)
  deriving (ToJSON) via (JSONSum Summy)

instance Aliased JSON Summy where
  aliases =
    branchAliases
      $ alias (Proxy @"Aa") "Aax"
      $ alias (Proxy @"Bb") "Bbx"
      $ alias (Proxy @"Cc") "Ccx"
      $ aliasListEnd

main :: IO ()
main = pure ()
