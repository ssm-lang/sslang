{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Remove unnecessary Par expressions from the IR

This pass detects unnecessary par expressions and then replaces them with equivalent sequential expressions.
-}
module IR.OptimizePar (
  optimizePar,
) where

import Common.Compiler
import qualified Common.Compiler as Compiler
import Control.Monad.State.Lazy (
  MonadState,
  StateT (..),
  evalStateT,
  gets,
  modify,
 )
import IR.IR (Literal (LitIntegral))
import qualified IR.IR as I


-- | Optimization Environment
data OptParCtx = OptParCtx
  { numPars :: Int
  -- ^ 'numPars' the number of par nodes in the input program's IR.
  , numBadPars :: Int
  -- ^ 'numLitInts' the number of "bad" par node in the input program's IR.
  }


-- | OptPar Monad
newtype OptParFn a = LiftFn (StateT OptParCtx Compiler.Pass a)
  deriving (Functor) via (StateT OptParCtx Compiler.Pass)
  deriving (Applicative) via (StateT OptParCtx Compiler.Pass)
  deriving (Monad) via (StateT OptParCtx Compiler.Pass)
  deriving (MonadFail) via (StateT OptParCtx Compiler.Pass)
  deriving (MonadError Compiler.Error) via (StateT OptParCtx Compiler.Pass)
  deriving (MonadState OptParCtx) via (StateT OptParCtx Compiler.Pass)


-- | Example func to delete later! Demonstrates how to extract a value from the OptParFn Monad
getNumberOfPars :: OptParFn Int
getNumberOfPars = gets numPars


-- | Example func to delete later! Demonstrates how to modify a value in the OptParFn Monad
updateNumberOfPars :: Int -> OptParFn ()
updateNumberOfPars num = do
  modify $ \st -> st{numPars = num}


-- | Run a LiftFn computation.
runLiftFn :: OptParFn a -> Compiler.Pass a
runLiftFn (LiftFn m) =
  evalStateT
    m
    OptParCtx
      { numPars = 0
      , numBadPars = 0
      }


{- | Entry-point to Par Optimization.

Maps over top level definitions, removing unnecessary pars.
-}
optimizePar :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
optimizePar p = runLiftFn $ do
  optimizedDefs <- mapM optimizeParTop $ I.programDefs p
  return $ p{I.programDefs = optimizedDefs}


-- | Given a top-level definition, detect + replace unnecessary par expressions
optimizeParTop :: (I.Binder I.Type, I.Expr I.Type) -> OptParFn (I.Binder I.Type, I.Expr I.Type)
optimizeParTop (nm, rhs) = do
  rhs' <- detectReplaceBadPar rhs
  (rhs'', _) <- countPars rhs' -- calling this so we don't get an "unused" warning
  (rhs''', _) <- countBadPars rhs'' -- calling this so we don't get an "unused" warning
  -- uncomment the line below to test countPars
  -- (_, result) <- countPars rhs
  -- _ <- fail (show nm ++ ": Number of Par Exprs: " ++ show result)
  -- uncomment the two lines below to test countBadPars
  -- (_, result') <- countBadPars rhs
  -- _ <- fail (show nm ++ ": Number of Bad Par Exprs: " ++ show result')
  return (nm, rhs''')


-- | Detect Unnecessary Par Expressions + Replace With Equivalent Sequential Expression
detectReplaceBadPar :: I.Expr I.Type -> OptParFn (I.Expr I.Type)
detectReplaceBadPar e = do
  pure e -- for now, just return the same thing (don't do anyting)


{- | 1) Count Par Nodes

Practice Exercise to Delete Later!

Traverse the IR representation of the body of a top level defintion,
and count the number of par expressions present.
Return the body unchanged, as well as the count numPars.
-}
countPars :: I.Expr I.Type -> OptParFn (I.Expr I.Type, Int)
countPars e = do
  -- currently a stub
  -- PUT YOUR IMPLEMENTATION HERE
  x <- getNumberOfPars
  updateNumberOfPars (x + 0) -- calling this so we don't get an "unused" warning
  return (e, x)


{- | 1.5) Implement IsBad Predicate

Suggested by John during Monday Meeting.
Returns true if par expr contains only instantaneous expressions as arguments.
False otherwise.
Useful for exercise 2.
-}
isBad :: I.Expr I.Type -> Bool
isBad _ = False -- currently a stub


{- | 2) Count Bad Par Nodes

Practice Exercise to Delete Later!

Traverse the IR representation of the body of a top level defintion,
and count the number of BAD par expressions present.
Use the helper predicate "isBad" in your implementation.
Return the body unchanged, as well as the count numBadPars.
-}
countBadPars :: I.Expr I.Type -> OptParFn (I.Expr I.Type, Int)
countBadPars e = do
  -- currently a stub
  let y = isBad (I.Lit (LitIntegral 5) (I.extract e)) -- calling this so we don't get an "unused" warning
  return (e, fromEnum y)
