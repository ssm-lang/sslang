{-# LANGUAGE DeriveTraversable #-}

module Constraint.Structure where

import Common.Identifiers (TConId (..))
import Data.Maybe (fromJust, isNothing)

data Structure a = TyConS TConId [a]
  deriving (Eq, Functor, Foldable, Traversable)

isLeaf :: Maybe (Structure a) -> Bool
isLeaf = isNothing

projectNonLeaf :: Maybe (Structure a) -> Structure a
projectNonLeaf = fromJust
