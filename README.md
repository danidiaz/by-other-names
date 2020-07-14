(**NOTE**: see this Cabal issue https://github.com/haskell/cabal/issues/6039
for progress in handling packages with multiple public libraries.)

# by-other-names

Give aliases to record fields.

When generically deriving [aeson](http://hackage.haskell.org/package/aeson)'s
`FromJSON` and `ToJSON` instances, field names are used as the keys for the
serialized JSON. If you don't want that, another option is to write the
instances manually. Problem is, you have to repeat the field names once for
`FromJSON` and once for `ToJSON`.

I wanted an intermediate solution similar to what is provided by Go's [struct
tags](https://www.digitalocean.com/community/tutorials/how-to-use-struct-tags-in-go):
associate aliases with each field and use those aliases when
serializing/deserializing. There can be different sets of aliases for different
contexts (json, orm...). In this library, each of those possible contexts is
called a "rubric".

## How to depend on this library?

This is a Cabal package with multiple public libraries.

- **by-other-names** 

  The general mechanism for defining field aliases. 

  Only depend on this if you are developing your own adapter for some new
  rubric.

  ```
  build-depends:
    by-other-names ^>= 1.0.2.0
  ```

- **by-other-names:aeson-adapter** 

  Helpers for defining aeson's FromJSON and ToJSON instances with aliases field
  names.

  ```
  build-depends:
    by-other-names:aeson-adapter  ^>= 1.0.2.0
  ```

- **by-other-names:th** 

  Provides a quasiquoter with a simplified syntax for defining aliases.

  ```
  build-depends:
    by-other-names:th  ^>= 1.0.2.0
  ```

## How to use by-other-names:aeson-adapter?

Here are two example, one for a record and another for a sum type:

    {-# LANGUAGE DataKinds #-}
    {-# LANGUAGE DeriveGeneric #-}
    {-# LANGUAGE DerivingVia #-}
    {-# LANGUAGE FlexibleInstances #-}
    {-# LANGUAGE MultiParamTypeClasses #-}
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE TypeApplications #-}
    {-# LANGUAGE ScopedTypeVariables #-}
    import ByOtherNames.Aeson
      ( 
        JSONRubric (JSON),
        JSONRecord(..),
        JSONSum(..),
        Proxy(Proxy),
        Aliased,
        alias,
        aliasListBegin,
        aliasListEnd,
        aliases
      )
    import Data.Aeson
    import Data.Aeson.Types
    import GHC.Generics
    import GHC.TypeLits

    data Foo = Foo {aa :: Int, bb :: Bool, cc :: Char}
      deriving (Read, Show, Eq, Generic)
      deriving (FromJSON, ToJSON) via (JSONRecord "obj" Foo)

    instance Aliased JSON Foo where
      aliases =
        aliasListBegin
          $ alias (Proxy @"aa") "aax"
          $ alias (Proxy @"bb") "bbx"
          $ alias (Proxy @"cc") "ccx"
          $ aliasListEnd

    data Summy
      = Aa Int
      | Bb Bool
      | Cc
      deriving (Read, Show, Eq, Generic)
      deriving (FromJSON, ToJSON) via (JSONSum "sum" Summy)

    instance Aliased JSON Summy where
      aliases =
        aliasListBegin
          $ alias (Proxy @"Aa") "Aax"
          $ alias (Proxy @"Bb") "Bbx"
          $ alias (Proxy @"Cc") "Ccx"
          $ aliasListEnd

Notice the use of
[`-XDerivingVia`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=derivingvia#extension-DerivingVia),
and of the `JSONSum` and `JSONRecord` adapter newtypes. The
[Symbol](http://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-TypeLits.html#t:Symbol)s
that parameterize the newtypes are used in parse error messages.

There are limitations on sum types though:

- Each branch can have zero or one fields, and the field can't have a selector.

- Only the "object with a single key consisting in the branch tag" style of serialization is supported.

## How to use by-other-names:th?

That library provides a module 'ByOtherNames.TH' which exports the 'aliasList'
quasiquoter:

    import ByOtherNames.TH

    -- ...

    data FooTH = FooTH {xa :: Int, xb :: Bool, xc :: Char, xd :: String, xe :: Int}
      deriving (Read, Show, Eq, Generic)
      deriving (FromJSON, ToJSON) via (JSONRecord "obj" FooTH)

    instance Aliased JSON FooTH where
      aliases = [aliasList| 
        xa = "aax",
        xb = "bbx",
        xc = "ccx",
        xd = "ddx",
        xe = "eex",
      |]

The trailing comma is optional.

