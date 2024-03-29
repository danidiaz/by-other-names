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

import ByOtherNames
import ByOtherNames.Aeson
  ( Aliased,
    JSONRecord (..),
    JSONRubric (JSON),
    JSONSum (..),
    JSONEnum (..),
    GeneralJSONEnum (..),
    alias,
    aliasListBegin,
    aliasListEnd,
    aliases,
  )
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
      . alias @"aa" "aax"
      . alias @"bb" "bbx"
      . alias @"cc" "ccx"
      . alias @"dd" "ddx"
      . alias @"ee" "eex"
      $ aliasListEnd

enumFoo :: [(Key, TypeRep)]
enumFoo =
  Data.Foldable.toList $
    gRecordEnum @Typeable @(Rep Foo)
      ( aliasListBegin
          . alias @"aa" "aax"
          . alias @"bb" "bbx"
          . alias @"cc" "ccx"
          . alias @"dd" "ddx"
          . alias @"ee" "eex"
          $ aliasListEnd
      )
      typeRep

expectedEnumFoo :: [(Key, TypeRep)]
expectedEnumFoo =
  [ ("aax", typeRep (Proxy @Int)),
    ("bbx", typeRep (Proxy @Bool)),
    ("ccx", typeRep (Proxy @Char)),
    ("ddx", typeRep (Proxy @String)),
    ("eex", typeRep (Proxy @Int))
  ]

data FooTH = FooTH {xa :: Int, xb :: Bool, xc :: Char, xd :: String, xe :: Int}
  deriving (Read, Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (JSONRecord "obj" FooTH)

instance Aliased JSON FooTH where
  aliases =
    [aliasList| 
    xa = "aax",
    xb = "bbx",
    xc = "ccx",
    xd = "ddx",
    xe = "eex",
  |]

data Summy
  = Aa Int
  | Bb Bool
  | Cc
  | Dd Char Bool Int
  | Ee Int
  deriving (Read, Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (JSONSum "sum" Summy)

instance Aliased JSON Summy where
  aliases =
    aliasListBegin
      . alias @"Aa" "Aax"
      . alias @"Bb" "Bbx"
      . alias @"Cc" "Ccx"
      . alias @"Dd" "Ddx"
      . alias @"Ee" "Eex"
      $ aliasListEnd

enumSummy :: [(Key, [TypeRep])]
enumSummy =
  Data.Foldable.toList $
    gSumEnum @Typeable @(Rep Summy)
      ( aliasListBegin
          . alias @"Aa" "Aax"
          . alias @"Bb" "Bbx"
          . alias @"Cc" "Ccx"
          . alias @"Dd" "Ddx"
          . alias @"Ee" "Eex"
          $ aliasListEnd
      )
      typeRep

expectedEnumSummy :: [(Key, [TypeRep])]
expectedEnumSummy =
  [ ("Aax", [typeRep (Proxy @Int)]),
    ("Bbx", [typeRep (Proxy @Bool)]),
    ("Ccx", []),
    ("Ddx", [typeRep (Proxy @Char), typeRep (Proxy @Bool), typeRep (Proxy @Int)]),
    ("Eex", [typeRep (Proxy @Int)])
  ]


data Enumy
  = Xx
  | Yy
  | Zz
  deriving (Read, Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (JSONEnum Enumy)
instance Aliased JSON Enumy where
  aliases =
    aliasListBegin
      $ alias @"Xx" "x"
      $ alias @"Yy" "y"
      $ alias @"Zz" "z"
      $ aliasListEnd


-- >>> enumSummy

roundtrip :: forall t. (Eq t, Show t, FromJSON t, ToJSON t) => t -> IO ()
roundtrip t =
  let reparsed = parseEither parseJSON (toJSON t)
   in case reparsed of
        Left err -> assertFailure err
        Right t' -> assertEqual "" t t'

data SingleField = SingleField {single :: Int}
  deriving (Read, Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (JSONRecord "sng" SingleField)

instance Aliased JSON SingleField where
  aliases =
    aliasListBegin
      . alias @"single" "Aa"
      $ aliasListEnd

-- data SingleBranch = SingleBranch Int
--   deriving (Read, Show, Eq, Generic)
--   deriving (FromJSON, ToJSON) via (JSONSum "sng" SingleBranch)

-- instance Aliased JSON SingleBranch where
--   aliases =
--     branchAliases
--         $ alias (Proxy @"SingleBranch") "Aa"
--         $ aliasListEnd

--
--
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All"
    [ testCase "recordRoundtrip" $ roundtrip $ Foo 0 False 'f' "foo" 3,
      testCase "recordRoundtripSingle" $ roundtrip $ SingleField 3,
      testGroup
        "sumRoundtrip"
        [ testCase "a" $ roundtrip $ Aa 5,
          testCase "b" $ roundtrip $ Bb False,
          testCase "c" $ roundtrip $ Cc,
          testCase "d" $ roundtrip $ Dd 'f' True 0,
          testCase "e" $ roundtrip $ Ee 3
        ],
      testGroup
        "enumRoundtrip"
        [
          testCase "x" $ roundtrip Xx,
          testCase "y" $ roundtrip Yy,
          testCase "z" $ roundtrip Zz
        ]
       ,
      testGroup
        "enums"
        [ testCase "prod typeReps" $ assertEqual "prod typeReps match" expectedEnumFoo enumFoo,
          testCase "sum typeReps" $ assertEqual "sum typeReps match" expectedEnumSummy enumSummy
        ]
    ]
