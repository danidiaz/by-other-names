{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import ByOtherNames
import ByOtherNames.Aeson
import GHC.Generics
import GHC.TypeLits
import Data.Aeson

data Foo = Foo {aa :: Int, bb :: Bool, cc :: Char} 
            deriving (Read, Show, Generic)
            deriving FromJSON via (FromJSONRecord "obj" Foo)

instance Aliased 'JSON Foo where
  aliases = 
    fieldAliases
    $ alias (Proxy @"aa") "foo"
    $ alias (Proxy @"bb") "bar"
    $ alias (Proxy @"cc") "baz"
    $ aliasListEnd

foo :: Foo
foo = Foo 0 False 'f'

main :: IO ()
main = pure ()

