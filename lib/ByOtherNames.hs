-- | This module provides the general mechanism for defining field and branch
-- aliases for algebraic datatypes.
--
-- Aliases can be defined for multiple contexts (json serialization, orms...).
-- Each of those contexts is termed a 'Rubric', basically a marker datakind
-- used to namespace the aliases.
--
-- This module should only be imported if you want to define your own adapter
-- package for some new `Rubric`. See "ByOtherNames.Aeson" for a concrete example.
module ByOtherNames
  ( -- * Aliases 
    Aliases,
    zipAliasesWith,
    AliasList,
    aliasListBegin,
    alias,
    aliasListEnd,
    -- * Rubrics
    Rubric (..),
    Aliased (aliases),
    -- * Generic helpers
    -- $helpers
    GHasDatatypeName(..),
    GHasFieldNames (..),
    GRecord (..),
    GHasBranchNames (..),
    GSum (..),
    Slots (..),
    -- * Re-exports
    Symbol,
  )
where

import ByOtherNames.Internal

{- $helpers

These generic helpers allow you to define typeclass instances for your datatypes
in one go, without having to divide your logic across auxiliary instances, or
deal with types of the generic representation like "GHC.Generics.D1".

You still need to use "GHC.Generics.Rep", @from@, and @to@. 

-}
