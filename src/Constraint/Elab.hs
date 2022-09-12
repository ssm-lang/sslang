{-# LANGUAGE OverloadedStrings #-}

module Constraint.Elab where

import qualified Common.Compiler as Compiler
import Common.Identifiers (VarId (..))
import Constraint.Datatype
  ( AstEnv,
    emptyEnv,
  )
import Constraint.Generation (hastype)
import Constraint.Solver (solveAndElab)
import Constraint.SolverM
  ( SolverM,
    initSolverCtx,
  )
import Control.Monad.ST.Trans (runSTT)
import Control.Monad.State.Lazy (evalStateT)
import qualified Front.Ast as A
import qualified IR.IR as I

-- | Elaborate implicitly typed AST program to explicitly typed IR program
elab :: A.Program -> Compiler.Pass (I.Program I.Type)
elab aprog =
  let ast = letify aprog
      env = prepEnv aprog
      iprog = do
        iexpr <- translate env ast
        let idefs = unletify iexpr
        return $
          I.Program
            { I.programEntry = "main",
              I.cDefs = undefined, -- TODO
              I.externDecls = undefined, -- TODO
              I.programDefs = idefs,
              I.typeDefs = undefined -- TODO
            }
   in evalStateT (runSTT iprog) initSolverCtx

-- | Elaborate AST expression to IR expression
translate :: AstEnv -> A.Expr -> SolverM s (I.Expr I.Type)
translate env aexpr = do
  co <- hastype env aexpr
  solveAndElab co

-- TODO: Need to prepare type declaration env for AST program
prepEnv :: A.Program -> AstEnv
prepEnv (A.Program ds) = let (_, _, _, _) = A.getTops ds in emptyEnv

-- | Wrap AST top-level definitions into one recursive let and type-annotate [main]
letify :: A.Program -> A.Expr
letify (A.Program ds) =
  let (_, _, _, dds) = A.getTops ds
   in A.Let dds (A.Constraint (A.Id "main") mainTyp)

-- | Undo what [letify] has done after translation is done
unletify :: I.Expr I.Type -> [(VarId, I.Expr I.Type)]
unletify = undefined -- TODO

-- | Type for [main] function: &Int -> &Int -> ()
mainTyp :: A.Typ
mainTyp =
  A.TApp (A.TCon "&") (A.TCon "Int")
    `A.TArrow` (A.TApp (A.TCon "&") (A.TCon "Int") `A.TArrow` A.TCon "()")
