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

```
build-depends:
  by-other-names ^>= 1.2.0.0
```

## How to use the Aeson adapter?

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
          . alias @"aa" "aax"
          . alias @"bb" "bbx"
          . alias @"cc" "ccx"
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
          . alias @"Aa" "Aax"
          . alias @"Bb" "Bbx"
          . alias @"Cc" "Ccx"
          $ aliasListEnd

Notice the use of
[`-XDerivingVia`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=derivingvia#extension-DerivingVia),
and of the `JSONSum` and `JSONRecord` adapter newtypes. The
[Symbol](http://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-TypeLits.html#t:Symbol)s
that parameterize the newtypes are used in parse error messages.

There are limitations on sum types though:

- Fields in branches of sum types can't have selectors. When there is more than one field in a branch, they are parsed as a JSON Array.

- For sum types, only the "object with a single key consisting in the branch tag" style of serialization is supported.

## How to use the quasiquoter?

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

## other packages

- [generics-sop]()

- [barbies](https://hackage.haskell.org/package/barbies)

- [higgledy](https://hackage.haskell.org/package/higgledy)

- [generic-data-surgery](https://hackage.haskell.org/package/generic-data-surgery)

- [one-liner](https://hackage.haskell.org/package/one-liner)



