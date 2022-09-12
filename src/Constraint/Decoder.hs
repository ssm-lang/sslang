module Constraint.Decoder where

import Common.Identifiers (TVarId (..), fromString)
import Constraint.SolverM (SolverM)
import qualified Constraint.Structure as S
import qualified Constraint.Unifier as U
import qualified Constraint.UnionFind as UF
import Constraint.Utils (throwTypeError)
import Control.Monad (unless)
import Control.Monad.ST.Trans (STRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.Map as M
import qualified IR.IR as I

newtype Decoder s = Decoder (STRef s (M.Map Int I.Type))

initDecoder :: SolverM s (Decoder s)
initDecoder = Decoder <$> newSTRef M.empty

decode :: Decoder s -> U.Variable s -> SolverM s I.Type
decode dec@(Decoder ref) uv = do
  m <- readSTRef ref
  d <- UF.descriptor uv
  let i = U.descId d
  case M.lookup i m of
    Just itype -> return itype
    Nothing -> do
      struc <- readSTRef (U.descStructure d)
      a <-
        if S.isLeaf struc
          then return $ decodeId i
          else
            ( do
                istruc <- mapM (decode dec) (S.projectNonLeaf struc)
                return $ unstructure istruc
            )
      writeSTRef ref $ M.insert i a m
      return a

inject :: Int -> TVarId
inject i = fromString $ "_a" ++ show i

decodeVariable :: U.Variable s -> SolverM s TVarId
decodeVariable uv = do
  d <- UF.descriptor uv
  struc <- readSTRef (U.descStructure d)
  unless (S.isLeaf struc) $ throwTypeError "can only decode leaf structure"
  return . inject . U.descId $ d

decodeId :: Int -> I.Type
decodeId = I.TVar . inject

unstructure :: S.Structure I.Type -> I.Type
unstructure (S.TyConS tcid ts) = I.TCon tcid ts
