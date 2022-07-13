{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module ByOtherNames.TypeLevel
  ( DemotedTypeForKind,
    DemotableType (..),
    GDemotableAnnsSumType (..),
  )
where

import Data.Kind
import GHC.Generics
import GHC.TypeLits

type DemotedTypeForKind :: ka -> Type
type family DemotedTypeForKind ka

class DemotableType (t :: ka) where
  demote :: DemotedTypeForKind ka

class GDemotableAnnsSumType ka (before :: [(Symbol, ka)]) rep (after :: [(Symbol, ka)]) | before rep -> after where
  gdemoteAnnForBrach :: rep z -> DemotedTypeForKind ka

instance GDemotableAnnsSumType ka before (left :+: right) after => GDemotableAnnsSumType ka before (D1 x (left :+: right)) after where
  gdemoteAnnForBrach (M1 sum) = gdemoteAnnForBrach @ka @before @(left :+: right) @after sum

instance
  ( GDemotableAnnsSumType ka before left middle,
    GDemotableAnnsSumType ka middle right after
  ) =>
  GDemotableAnnsSumType ka before (left :+: right) after
  where
  gdemoteAnnForBrach = \case
    L1 leftBranch ->
      gdemoteAnnForBrach @ka @before @left @middle leftBranch
    R1 rightBranch ->
      gdemoteAnnForBrach @ka @middle @right @after rightBranch

instance DemotableType ann => GDemotableAnnsSumType ka ('(name, ann) ': after) (C1 (MetaCons name fixity b) slots) after where
  gdemoteAnnForBrach _ = demote @ka @ann
