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
import ByOtherNamesH.Aeson
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
  deriving stock (Read, Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (JSONRecord "obj" Foo)
instance Aliased JSON Foo where
  aliases =
    aliasListBegin
      . alias @"aa" "aax" (singleSlot fromToJSON)
      . alias @"bb" "bbx" (singleSlot fromToJSON)
      . alias @"cc" "ccx" (singleSlot fromToJSON)
      . alias @"dd" "ddx" (singleSlot fromToJSON)
      . alias @"ee" "eex" (singleSlot fromToJSON)
      $ aliasListEnd

roundtrip :: forall t. (Eq t, Show t, FromJSON t, ToJSON t) => t -> IO ()
roundtrip t =
  let reparsed = parseEither parseJSON (toJSON t)
   in case reparsed of
        Left err -> assertFailure err
        Right t' -> assertEqual "" t t'

--
--
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All"
    [ testCase "recordRoundtrip" $ roundtrip $ Foo 0 False 'f' "foo" 3
    ]
