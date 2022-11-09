1.2.2.0
=======

- GeneralJSONEnum : like JSONEnum, but lets you define FromJSON/ToJSON instances
  for a newtype that use the generic representation of the wrapped type,
  *without* requiring FromJSON/ToJSON instances from the wrapped type. So it's
  different from GeneralizedNewtypeDeriving: only the generic rep of the wrapped
  type is used!

  JSONEnum instances implemented in terms of GeneralJSONEnum.

1.2.1.0
=======

- JSONEnum: FromJSON / ToJSON instances for enum-like types without fields.

1.2.0.1
=======

- A bit more documentation.

1.2.0.0
=======

- Hid the internals of the `Aliases` module in the main module.
- Various generic helpers.

1.0.1.0
=======

- deprecated fieldAliases and branchAliases in favor of aliasListBegin.

1.0.2.0
=======

- added a quasiquoter in a new public library

1.1.0.0
=======

- removed deprecated functions fieldAliases and branchAliases
- renamed ForRubric to AliasType
