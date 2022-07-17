-- | This package provides the general mechanism for defining field and branch
-- aliases for algebraic datatypes.
--
-- Aliases can be defined for multiple contexts (json serialization, orms...).
-- Each of those contexts is termed a 'Rubric', basically a marker datakind
-- used to namespace the aliases.
--
-- This module should only be imported if you want to define your own adapter
-- package for some new `Rubric`.
module ByOtherNames
  ( -- * Aliases 
    Aliases,
    AliasList,
    aliasListBegin,
    alias,
    aliasListEnd,
    -- * Rubrics
    Rubric (..),
    Aliased (aliases),
    -- * Generic helpers
    GSum (..),
    Slot (..),
    GRecord (..),
    -- * Re-exports
    Symbol,
  )
where

import ByOtherNames.Internal