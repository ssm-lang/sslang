module IR.Constraint.Occurs (
  occurs,
) where

import Data.Foldable (foldrM)
import IR.Constraint.Monad (TC)
import IR.Constraint.Type (
  Content (..),
  Descriptor (..),
  FlatType (..),
  Variable,
 )
import qualified IR.Constraint.UnionFind as UF


-- OCCURS

occurs :: Variable -> TC Bool
occurs var = occursHelp [] var False


occursHelp :: [Variable] -> Variable -> Bool -> TC Bool
occursHelp seen var foundCycle =
  if var `elem` seen
    then return True
    else do
      (Descriptor content _ _ _) <- UF.get var
      case content of
        FlexVar _ -> return foundCycle
        RigidVar _ -> return foundCycle
        Structure term ->
          let newSeen = var : seen
           in case term of
                TCon1 _ args -> foldrM (occursHelp newSeen) foundCycle args
        Error -> return foundCycle
