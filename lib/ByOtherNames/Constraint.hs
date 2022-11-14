{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ByOtherNames.Constraint (Top, Impossible) where
import Data.Void (Void)

-- | A constraint that can always be satisfied.
--
-- Copied from the @sop-core@ package.
--
class Top x
instance Top x

-- | A constraint that can't be satisfied.
--
-- Mostly useful with enum-like sum types to denote that they don't have fields.
class (x ~ Void) => Impossible x
instance (x ~ Void) => Impossible x
