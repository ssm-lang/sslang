module Constraint.Generation where

import Constraint.Datatype (AstEnv)
import Constraint.Solver
import Constraint.SolverM (SolverM)
import qualified Front.Ast as A
import qualified IR.IR as I

type IExpr = I.Expr I.Type

type AExpr = A.Expr

hastype :: AstEnv -> AExpr -> SolverM s (Co IExpr)
hastype = undefined

hastype' :: AstEnv -> A.Expr -> Variable -> SolverM s (Co IExpr)
hastype' = undefined

exist :: (Variable -> SolverM s (Co IExpr)) -> SolverM s (Co IExpr)
exist = undefined
