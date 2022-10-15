{-# LANGUAGE DeriveTraversable #-}

module Constraint.Structure where

import           Common.Identifiers             ( TConId(..) )
import           Data.Maybe                     ( fromJust
                                                , isNothing
                                                )

-- TODO: is deriving really ok here?
data Structure a = TyConS TConId [a]
  deriving (Eq, Show, Functor, Foldable, Traversable)

leaf :: Maybe (Structure a)
leaf = Nothing

isLeaf :: Maybe (Structure a) -> Bool
isLeaf = isNothing

projectNonLeaf :: Maybe (Structure a) -> Structure a
projectNonLeaf = fromJust
