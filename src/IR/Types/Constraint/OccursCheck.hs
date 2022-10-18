-- |

module IR.Types.Constraint.OccursCheck
  ( newOccursCheck
  ) where

import           Control.Monad                  ( unless
                                                , when
                                                )
import           Control.Monad.Except           ( throwError )
import qualified IR.Types.Constraint.ShadowMap as SM
import           IR.Types.Constraint.Type       ( Descriptor
                                                , Infer
                                                , UVar
                                                , getStructure
                                                , getTag
                                                , isLeaf
                                                , occursError
                                                , projectNonLeaf
                                                )
import qualified IR.Types.Constraint.UnionFind as UF

newOccursCheck
  :: (Descriptor s -> Infer s Bool) -> Infer s (UVar s -> Infer s ())
newOccursCheck isYoung = do
  table <- SM.new
  let trav v = do
        d   <- UF.descriptor v
        isy <- isYoung d
        when isy $ do
          let tag = getTag d
          visited <- SM.lookup table tag
          case visited of
            Just False -> throwError occursError
            Just True  -> return ()
            Nothing    -> do
              SM.add table tag False
              struc <- getStructure d
              unless (isLeaf struc) $ mapM_ trav (projectNonLeaf struc)
              SM.replace table tag True
  return trav
