{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import ByOtherNamesH
import ByOtherNames.TH
import Control.Monad (forM)
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import Data.Typeable
import GHC.Generics
import GHC.TypeLits
import Test.Tasty
import Test.Tasty.HUnit


data Foo = Foo {aa :: Int, bb :: Bool, cc :: Char, dd :: String, ee :: Int}
  deriving (Read, Show, Eq, Generic)

data X

data Shower v = Shower (v -> String)

instance Rubric X where
  type AliasType X = String
  type WrapperType X = Shower 

instance Aliased X Foo where
  aliases =
    aliasListBegin
      . alias @"aa" "aax" (singleSlot (Shower show))
      . alias @"bb" "bbx" (singleSlot (Shower show))
      . alias @"cc" "ccx" (singleSlot (Shower show))
      . alias @"dd" "ddx" (singleSlot (Shower show))
      . alias @"ee" "eex" (singleSlot (Shower show))
      $ aliasListEnd

--
--
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All"
    [    ]
