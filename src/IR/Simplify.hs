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
             | LoopBreaker -- TBD
             | OnceSafe
             | MultiSafe
             | OnceUnsafe
             | MultiUnsafe
             | Never

-- | Simplifier Environment
data SimplEnv = SimplEnv
  { occInfo :: M.Map I.VarId OccInfo
  {- ^ 'occInfo' maps an identifier to its occurence category -}
  , runs    :: Int
  {- ^ 'runs' stores how many times the simplifier has run so far -}
  }

-- | Simplifier Monad
newtype SimplFn a = SimplFn (StateT SimplEnv Compiler.Pass a)
  deriving Functor                      via (StateT SimplEnv Compiler.Pass)
  deriving Applicative                  via (StateT SimplEnv Compiler.Pass)
  deriving Monad                        via (StateT SimplEnv Compiler.Pass)
  deriving MonadFail                    via (StateT SimplEnv Compiler.Pass)
  deriving (MonadError Compiler.Error)  via (StateT SimplEnv Compiler.Pass)
  deriving (MonadState SimplEnv)         via (StateT SimplEnv Compiler.Pass)

-- | Run a SimplFn computation.
runSimplFn :: SimplFn a -> Compiler.Pass a
--evalStateT runs function in context and returns final value, without context
  -- take state transformer monad and an initial state, and gives the initial state to the transformer monad. (and the transformer monad holds our monadic program)
      -- runSimplFun takes in the Compiler.Pass wrapped inside a SimplFn
      -- at the moment of evalStateT taking in m and SimplCtx, m has Compiler.Pass (from do) but not SimplCtx yet
        -- but so you have m that doesn't have SimplCtx yet?? (IDK) -- look at stackoverflow post
          -- state transformer: function that takes in state and returns final value (Compiler.Pass)
  -- m is state transformer monad (result of do inside simplifyProgram)
  -- SimplCtx is initial state
runSimplFn (SimplFn m) = evalStateT m SimplEnv { occInfo = M.empty, runs = 0 } 

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
simplifyProgram p = runSimplFn $ do -- everything in do expression will know about simplifier environment
                                    -- run this do expression and give it the knowledge of my simplifier environment
  let defs = I.programDefs p
  simplifiedProgramDefs <- mapM simplTop defs
  return $ p { I.programDefs = simplifiedProgramDefs } -- this whole do expression returns a Compiler.Pass

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

{-
thoughts:
Decouple occurence analyzer from simplExpr

have occurence analyzer fill in SimplEnv and just feed it into runSimplFn; 

runSimplFn will purely care about inlining
-}