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

class DemotableAnnKind ka where
    type DemotedAnnType ka :: Type
    demoteAnn :: DemotedAnnType ka

-- type GDemotableAnnsSumType :: ka -> [ '(Symbol, ka) ] -> (k -> Type) -> [ '(Symbol, ka) ] -> Constraint
class DemotableAnnKind ka => GDemotableAnnsSumType ka (before :: [ ( Symbol, ka) ]) rep (after  :: [ (Symbol, ka)]) | before rep -> after where
   gdemotedAnnForBrach :: rep z -> DemotedAnnType ka

instance GDemotableAnnsSumType ka before (left :+: right) after => GDemotableAnnsSumType ka before (D1 x (left :+: right)) after where
    gdemotedAnnForBrach (M1 sum) = gdemotedAnnForBrach @ka @before @(left :+: right) @after sum

instance (GDemotableAnnsSumType ka before left middle,
          GDemotableAnnsSumType ka middle right after)
            => GDemotableAnnsSumType ka before (left :+: right) after where
    gdemotedAnnForBrach = \case
      L1 leftBranch ->
        gdemotedAnnForBrach @ka @before @left @middle leftBranch
      R1 rightBranch ->
        gdemotedAnnForBrach @ka @middle @right @after rightBranch

instance DemotableAnnKind ka => GDemotableAnnsSumType ka ('(name, ann) ': after) (C1 (MetaCons name fixity b) slots) after where
    gdemotedAnnForBrach _ = demoteAnn @ann



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