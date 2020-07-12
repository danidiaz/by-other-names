{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import ByOtherNames.Aeson
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import GHC.TypeLits

import Test.Tasty
import Test.Tasty.HUnit  

data Foo = Foo {aa :: Int, bb :: Bool, cc :: Char, dd :: String, ee :: Int}
  deriving (Read, Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (JSONRecord "obj" Foo)

instance Aliased JSON Foo where
  aliases =
    fieldAliases
      $ alias (Proxy @"aa") "aax"
      $ alias (Proxy @"bb") "bbx"
      $ alias (Proxy @"cc") "ccx"
      $ alias (Proxy @"dd") "ddx"
      $ alias (Proxy @"ee") "eex"
      $ aliasListEnd

data Summy
  = Aa Int
  | Bb Bool
  | Cc
  | Dd Char
  | Ee Int
  deriving (Read, Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (JSONSum "sum" Summy)

roundtrip :: forall t. (Eq t, Show t, FromJSON t, ToJSON t) => t -> IO () 
roundtrip t = 
    let reparsed = parseEither parseJSON (toJSON t)
     in case reparsed of
            Left err -> assertFailure err
            Right t' -> assertEqual "" t t'

instance Aliased JSON Summy where
  aliases =
    branchAliases
      $ alias (Proxy @"Aa") "Aax"
      $ alias (Proxy @"Bb") "Bbx"
      $ alias (Proxy @"Cc") "Ccx"
      $ alias (Proxy @"Dd") "Ddx"
      $ alias (Proxy @"Ee") "Eex"
      $ aliasListEnd

data SingleField = SingleField {single :: Int}
  deriving (Read, Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (JSONRecord "sng" SingleField)

instance Aliased JSON SingleField where
  aliases =
    fieldAliases
        $ alias (Proxy @"single") "Aa"
        $ aliasListEnd

data SingleBranch = SingleBranch Int
  deriving (Read, Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (JSONRecord "sng" SingleBranch)

instance Aliased JSON SingleBranch where
  aliases =
    branchAliases
        $ alias (Proxy @"SingleBranch") "Aa"
        $ aliasListEnd

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup
    "All"
    [
        testCase "recordRoundtrip" $ roundtrip $ Foo 0 False 'f' "foo" 3,
        testCase "recordRoundtripSingle" $ roundtrip $ SingleField 3,
        testGroup "sumRoundtrip" [
            testCase "a" $ roundtrip $ Aa 5,
            testCase "b" $ roundtrip $ Bb False,
            testCase "c" $ roundtrip $ Cc,
            testCase "d" $ roundtrip $ Dd 'f',
            testCase "e" $ roundtrip $ Ee 3
        ],
        testCase "sumRoundtripSingle" $ roundtrip $ SingleBranch 3
    ]

