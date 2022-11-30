{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{- | Lift lambda definitions into the global scope.

This pass is responsible for moving nested lambda definitions into the global
scope and performing necessary callsite adjustments.
-}
module IR.Simplify
  ( simplifyProgram
  ) where

-- import qualified IR.Types                      as I
import qualified Common.Compiler               as Compiler
-- import           Common.Identifiers
import qualified IR.IR                         as I

-- import           Control.Monad                  ( forM_ )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                , gets
                                                , modify
                                                -- , unless
                                                )

-- import           Data.Bifunctor                 ( first )
-- import           Data.List                      ( intersperse )
import qualified Data.Map                      as M
-- import           Data.Maybe                     ( catMaybes )
-- import qualified Data.Set                      as S

-- | Occurrence Information for each binding
data OccInfo = Dead
             | LoopBreaker
             | OnceSafe
             | MultiSafe
             | OnceUnsafe
             | MultiUnsafe

-- | Simplifier Environment
data SimplCtx = SimplCtx
  { occInfo :: M.Map I.VarId OccInfo
  {- ^ 'occInfo' maps an identifier to its occurence category -}
  , runs    :: Int
  {- ^ 'runs' stores how many times the simplifier has run so far -}
  }

-- | Simplifier Monad
newtype SimplFn a = SimplFn (StateT SimplCtx Compiler.Pass a)
  deriving Functor                      via (StateT SimplCtx Compiler.Pass)
  deriving Applicative                  via (StateT SimplCtx Compiler.Pass)
  deriving Monad                        via (StateT SimplCtx Compiler.Pass)
  deriving MonadFail                    via (StateT SimplCtx Compiler.Pass)
  deriving (MonadError Compiler.Error)  via (StateT SimplCtx Compiler.Pass)
  deriving (MonadState SimplCtx)         via (StateT SimplCtx Compiler.Pass)

-- | Run a SimplFn computation.
runSimplFn :: SimplFn a -> Compiler.Pass a
runSimplFn (SimplFn m) = evalStateT m SimplCtx { occInfo = M.empty, runs = 0 }

-- | Add a binder to occInfo with category Dead by default
addOccVar :: I.VarId -> SimplFn ()
addOccVar binder = do
  m <- gets occInfo
  let m' = case M.lookup binder m of
        Nothing -> M.insert binder Dead m
        _       -> error "Should never have never seen this binder before!"
  modify $ \st -> st { occInfo = m' }

{- | Entry-point to Simplifer.

Maps over top level definitions to create a new simplified Program
Do we ever want to inline top-level definitions?
-}
simplifyProgram :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
simplifyProgram p = runSimplFn $ do
  let defs = I.programDefs p
  simplifiedProgramDefs <- mapM simplTop defs
  return $ p { I.programDefs = simplifiedProgramDefs }

-- | Simplify a top-level definition
simplTop :: (I.VarId, I.Expr I.Type) -> SimplFn (I.VarId, I.Expr I.Type)
simplTop (v, e) = (,) v <$> simplExpr e


{- | Recursively simplify IR expressions.

Probably want more documentation here eventually.

-}
simplExpr :: I.Expr I.Type -> SimplFn (I.Expr I.Type)
simplExpr e = do
  addOccVar "test"
  return e -- what a stub.

