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
## Other related packages

- [generics-sop](https://hackage.haskell.org/package/generics-sop)

- [barbies](https://hackage.haskell.org/package/barbies)

- [higgledy](https://hackage.haskell.org/package/higgledy)

- [generic-data-surgery](https://hackage.haskell.org/package/generic-data-surgery)

- [one-liner](https://hackage.haskell.org/package/one-liner)
