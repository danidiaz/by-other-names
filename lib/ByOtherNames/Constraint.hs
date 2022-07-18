{-# LANGUAGE FlexibleInstances #-}
module ByOtherNames.Constraint (Top) where

-- | A constraint that can always be satisfied.
--
-- Copied from the @sop-core@ package.
--
class Top x
instance Top x