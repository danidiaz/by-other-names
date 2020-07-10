{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import ByOtherNames
import GHC.Generics

data Foo = Foo {aa :: Int, bb :: Bool, cc :: Char} deriving (Read, Show, Generic)

foo :: Foo
foo = Foo 0 False 'f'

-- forgetting the name of a field causes a type error, as it should be
fooAliases :: Aliases String (Rep Foo)
fooAliases =
  fieldAliases
    $ alias (Proxy @"aa") "foo"
    $ alias (Proxy @"bb") "bar"
    $ alias (Proxy @"cc") "baz"
    $ aliasListEnd

instance Aliased () String Foo where
  aliases _ _ = fooAliases

main :: IO ()
main = pure ()
