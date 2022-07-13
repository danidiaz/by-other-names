{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
module ByOtherNames.TypeLevel where

import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits

class DemotableAnn ka (t :: Type) | ka -> t where
    demoteAnn :: t

type GDemotableAnnsSumType :: Type -> [ (Symbol, ka) ] -> (k -> Type) -> [ (Symbol, ka) ] -> Constraint
class GDemotableAnnsSumType t (before :: [ (Symbol, ka) ]) rep (after  :: [(Symbol, ka)]) | before rep -> after where
    gdemotedAnnForBrach :: rep z -> t

instance GDemotableAnnsSumType t before (left :+: right) after => GDemotableAnnsSumType t before (D1 x (left :+: right)) after where
    gdemotedAnnForBrach (M1 sum) = gdemotedAnnForBrach @_ @_ @t @before @(left :+: right) @after sum

instance (GDemotableAnnsSumType t before left middle,
          GDemotableAnnsSumType t middle right after)
            => GDemotableAnnsSumType t before (left :+: right) after where
    gdemotedAnnForBrach = \case
      L1 leftBranch ->
        gdemotedAnnForBrach @_ @_ @t @before @left @middle leftBranch
      R1 rightBranch ->
        gdemotedAnnForBrach @_ @_ @t @middle @right @after rightBranch

instance DemotableAnn ka t => GDemotableAnnsSumType ka ('(name, t) ': after) (C1 (MetaCons name fixity b) slots) after where
    gdemotedAnnForBrach _ = demoteAnn @ka @t



-- demotedAnnForBrach :: 

-- type TypeLevelAnns :: a -> (k -> Type) -> (k -> Type)
-- type family TypeLevelAnns a rep where
--     TypeLevelAnns a (D1 x (C1 y fields)) = D1 x (C1 y (TypeLevelAnns a fields))
--     TypeLevelAnns a (D1 x (left :+: right)) = D1 x (TypeLevelAnns a left :+: TypeLevelAnns a right)
--     TypeLevelAnns a (left :*: right) = TypeLevelAnns a left :*: TypeLevelAnns a right
--     TypeLevelAnns a (left :+: right) = TypeLevelAnns a left :+: TypeLevelAnns a right
--     TypeLevelAnns a (S1 metasel v) = a
--     TypeLevelAnns a (C1 metacons v) = a

-- instance JsonP typelist rep2 rep '[]
-- before repann rep after  