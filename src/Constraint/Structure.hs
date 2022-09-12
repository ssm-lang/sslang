{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Constraint.Structure where

import Common.Identifiers (TConId (..))
import Data.Maybe (fromJust, isJust)

data Structure a = TyConStruc TConId [a]
  deriving (Eq, Functor, Foldable, Traversable)

isLeaf :: Maybe (Structure a) -> Bool
isLeaf = isJust

projectNonLeaf :: Maybe (Structure a) -> Structure a
projectNonLeaf = fromJust
