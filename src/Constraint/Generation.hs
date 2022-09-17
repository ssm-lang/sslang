module Constraint.Generation where

import Common.Identifiers (IsString (fromString), TVarId (..), VarId (..))
import Constraint.Datatype (AstEnv)
import Constraint.Solver
import Constraint.SolverM (SolverM)
import Constraint.Structure
import Data.List (elemIndex)
import Data.Maybe (fromJust, fromMaybe)
import IR.IR
import IR.Types.Type

genConstraints :: Program Annotations -> SolverM s (Co (Program Type))
genConstraints prog = do
  let (n, e) = head $ programDefs prog
  c1 <- exist (hastype prog e)
  let c2 = fmap (\e' -> prog {programDefs = [(n, e')]}) c1
  return c2

hastype :: Program Annotations -> Expr Annotations -> Variable -> SolverM s (Co (Expr Type))
hastype prog (Var x _) w =
  inst x w
    <$$> \((gs, t), witnesses) -> Var x (subTVars gs witnesses t)
hastype prog (Lambda x u _) w =
  exist
    ( \v1 ->
        exist
          ( \v2 ->
              do
                c1 <- w -==- TyConS (fromString "->") [v1, v2]
                c2 <- hastype prog u v2
                return (c1 ^& def (fromJust x) v1 c2)
          )
    )
    <$$> \(_, (t, e)) -> Lambda x e (Arrow t (extract e))

subTVars :: [TVarId] -> [Type] -> Type -> Type
subTVars gs ws = subTVars'
  where
    subTVars' (TCon tcid ts) = TCon tcid (map (subTVars') ts)
    subTVars' (TVar x) =
      let iopt = elemIndex x gs
       in case iopt of
            Just i -> ws !! i
            Nothing -> TVar x

-- hastype' :: AstEnv -> A.Expr -> Variable -> SolverM s (Co IExpr)
-- hastype' = undefined

-- exist :: (Variable -> SolverM s (Co IExpr)) -> SolverM s (Co IExpr)
-- exist = undefined
