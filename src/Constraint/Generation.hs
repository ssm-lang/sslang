module Constraint.Generation where

import Constraint.Datatype (AstEnv)
import Constraint.Solver (Co (..))
import Constraint.SolverM (SolverM)
import qualified Front.Ast as A
import qualified IR.IR as I

hastype :: AstEnv -> A.Expr -> SolverM s (Co (I.Expr I.Type))
hastype = undefined
