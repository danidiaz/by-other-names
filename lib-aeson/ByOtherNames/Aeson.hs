{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module ByOtherNames.Aeson
  ( JSONRubric (..),
    JSONRecord (..),
    JSONSum (..),
  )
where

import ByOtherNames
import Data.Aeson
import Data.Aeson.Types
import Data.Functor.Compose
import Data.Kind
import Data.Text
import GHC.Generics
import GHC.TypeLits

data JSONRubric = JSON

instance Rubric JSON where
  type ForRubric JSON = Text

type JSONRecord :: Symbol -> Type -> Type
newtype JSONRecord s r = JSONRecord r

type JSONSum :: Type -> Type
newtype JSONSum r = JSONSum r

--
--
newtype FieldParser a = FieldParser ( Object -> Parser a )
  deriving (Functor, Applicative) via ((->) Object `Compose` Parser)

type FieldsFromJSON :: (Type -> Type) -> Constraint
class FieldsFromJSON t where
  fieldParser :: Aliases Text t -> FieldParser (t x)

instance FromJSON v => FieldsFromJSON (S1 x (Rec0 v)) where
  fieldParser (Leaf fieldName) = FieldParser \o -> M1 . K1 <$> explicitParseField parseJSON o fieldName

instance (FieldsFromJSON left, FieldsFromJSON right) => FieldsFromJSON (left :*: right) where
  fieldParser (Prod left right) =
    (:*:) <$> fieldParser left <*> fieldParser right

instance (KnownSymbol s, Aliased JSON r, Rep r ~ D1 x (C1 y prod), FieldsFromJSON prod) => FromJSON (JSONRecord s r) where
  parseJSON v =
    let ByOtherNames.Object prod = aliases @JSONRubric @JSON @r
        FieldParser parser = fieldParser prod
     in JSONRecord . to . M1 . M1 <$> withObject (symbolVal (Proxy @s)) parser v

--
--
newtype FieldConverter a = FieldConverter ( a -> [(Text, Value)] )
  deriving newtype (Semigroup,Monoid)

type FieldsToJSON :: (Type -> Type) -> Constraint
class FieldsToJSON t where
  fieldConverter :: Aliases Text t -> FieldConverter (t x)

instance ToJSON v => FieldsToJSON (S1 x (Rec0 v)) where
  fieldConverter (Leaf fieldName) = FieldConverter \(M1 (K1 v)) -> [(fieldName,toJSON v)]

instance (FieldsToJSON left, FieldsToJSON right) => FieldsToJSON (left :*: right) where
  fieldConverter (Prod left right) =
     FieldConverter \(leftFields :*: rightFields) ->  
          let FieldConverter leftConverter = fieldConverter left 
              FieldConverter rightConverter = fieldConverter right
           in leftConverter leftFields ++ rightConverter rightFields

instance (Aliased JSON r, Rep r ~ D1 x (C1 y prod), FieldsToJSON prod) => ToJSON (JSONRecord s r) where
  toJSON (JSONRecord (from -> M1 (M1 a))) =
    let ByOtherNames.Object prod = aliases @JSONRubric @JSON @r
        FieldConverter fieldsToValues = fieldConverter prod
     in object (fieldsToValues a)

--
--
newtype BranchConverter a = BranchConverter ( a -> Value )

type BranchesToJSON :: (Type -> Type) -> Constraint
class BranchesToJSON t where
  branchConverter :: Aliases Text t -> BranchConverter (t x)

instance ToJSON v => BranchesToJSON (C1 x (S1 y (Rec0 v))) where
  branchConverter (Ctor fieldName) = BranchConverter \(M1 (M1 (K1 v))) -> object [(fieldName,toJSON v)]

instance BranchesToJSON (C1 x U1) where
  branchConverter (Ctor fieldName) = BranchConverter \(M1 U1) -> object [(fieldName,Null)]

instance (BranchesToJSON left, BranchesToJSON right) => BranchesToJSON (left :+: right) where
  branchConverter (Sum left right) =
     BranchConverter \alternatives -> case alternatives of 
        L1 leftBranch -> 
            let BranchConverter leftConverter = branchConverter left 
             in leftConverter leftBranch
        R1 rightBranch -> 
            let BranchConverter rightConverter = branchConverter right 
             in rightConverter rightBranch

instance (Aliased JSON r, Rep r ~ D1 x (left :+: right), BranchesToJSON (left :+: right)) => ToJSON (JSONSum r) where
  toJSON (JSONSum (from -> M1 a)) =
    let ByOtherNames.SumObject branches = aliases @JSONRubric @JSON @r
        BranchConverter branchesToValues = branchConverter branches
     in branchesToValues a

