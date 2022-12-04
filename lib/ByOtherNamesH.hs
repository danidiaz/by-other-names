{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module ByOtherNamesH () where

import Control.Applicative
import Data.Foldable.WithIndex
import Data.Functor.WithIndex
import Data.Kind
import Data.Proxy
import Data.Traversable.WithIndex
import GHC.Generics
import GHC.TypeLits

type AliasesH :: (Type -> Type) -> Type -> (Type -> Type) -> Type
data AliasesH rep a h where
  Field :: 
    KnownSymbol fieldName => 
    a ->
    h v -> 
    AliasesH (S1 ('MetaSel ('Just fieldName) unpackedness strictness laziness) (Rec0 v)) a h
  Branch :: 
    KnownSymbol branchName => 
    a -> 
    BranchFieldsH v h ->
    AliasesH (C1 ('MetaCons branchName fixity sels) v) a h
  FieldTree ::
    AliasesH left a h ->
    AliasesH right a h ->
    AliasesH (left :*: right) a h
  BranchTree ::
    AliasesH left a h ->
    AliasesH right a h ->
    AliasesH (left :+: right) a h
  -- | We force the sum to contain at least two branches.
  Sum ::
    AliasesH (left :+: right) a h ->
    AliasesH (D1 x (left :+: right)) a h
  Record ::
    AliasesH fields a h ->
    AliasesH (D1 x (C1 y fields)) a h

type BranchFieldsH :: (Type -> Type) -> (Type -> Type) -> Type
data BranchFieldsH rep h where
  BranchFieldTree :: 
    BranchFieldsH left h ->  
    BranchFieldsH left h -> 
    BranchFieldsH (left :+: right) h
  BranchField ::
    h v ->
    BranchFieldsH (S1 ('MetaSel 'Nothing unpackedness strictness laziness) (Rec0 v)) h

data InductiveTuple :: (k -> Type) -> [k] -> Type where
  EmptyTuple  :: InductiveTuple f '[]
  ConsTuple :: f x -> InductiveTuple f xs -> InductiveTuple f (x ': xs)

-- | An intermediate datatype for specifying the aliases.  See
-- 'aliasListBegin', 'alias' and 'aliasListEnd'.
type AliasListH :: [(Symbol, [Type])] -> Type -> (Type -> Type) -> Type
data AliasListH code a h where
  EmptyAliasList :: AliasListH '[] a h
  ConsAliasList :: 
    Proxy name -> 
    a -> 
    InductiveTuple h slots -> 
    AliasListH prev a h -> AliasListH ('(name,slots) : prev) a h
