{-# LANGUAGE OverloadedStrings #-}

module IR.Constraint.Error where

import qualified Common.Identifiers as Ident


data Type
  = Infinite
  | Error
  | FlexVar Ident.TVarId
  | RigidVar Ident.TVarId
  | Type Ident.TConId [Type]
  deriving (Show)


-- data Problem
--   = ArityMismatch Int Int
--   | BadRigidVar Ident.TVarId Type

data Error
  = BadExpr Type Type
  | BadPattern Type Type
  | InfiniteType Ident.Identifier Type


toErrorString :: Error -> String
toErrorString err = case err of
  BadExpr actualType expectedType ->
    "Ill-typed expression. Expected "
      ++ show expectedType
      ++ ", but got "
      ++ show actualType
  BadPattern actualType expectedType ->
    "Ill-typed pattern. Expected "
      ++ show expectedType
      ++ ", but got "
      ++ show actualType
  InfiniteType name overallType ->
    "Infinite type for variable "
      ++ show name
      ++ ", which has the infinite type: "
      ++ show overallType
