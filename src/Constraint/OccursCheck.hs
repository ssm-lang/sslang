module Constraint.OccursCheck where

import qualified Constraint.ShadowMap as SM
import Constraint.SolverM (SolverM)
import Constraint.Structure (isLeaf, projectNonLeaf)
import qualified Constraint.Unifier as U
import qualified Constraint.UnionFind as U
import Constraint.Utils (throwTypeError)
import Control.Monad (unless, when)
import Control.Monad.ST.Trans (readSTRef)

newOccursCheck :: (U.Descriptor s -> SolverM s Bool) -> SolverM s (U.Variable s -> SolverM s ())
newOccursCheck isYoung = do
  table <- SM.new
  let trav v = do
        d <- U.descriptor v
        isy <- isYoung d
        when isy $ do
          let i = U.descId d
          visited <- SM.lookup table i
          case visited of
            Just False -> throwTypeError "recursive type detected"
            Just True -> return ()
            Nothing -> do
              SM.add table i False
              struc <- readSTRef $ U.descStructure d
              unless (isLeaf struc) $ mapM_ trav (projectNonLeaf struc)
              SM.replace table i True
  return trav
