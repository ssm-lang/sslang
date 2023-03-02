{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

{- | Lift lambda definitions into the global scope.

This pass is responsible for moving nested lambda definitions into the global
scope and performing necessary callsite adjustments.
-}
module IR.LambdaLift (
  liftProgramLambdas,
) where

import qualified Common.Compiler as Compiler
import Common.Identifiers
import qualified IR.IR as I

import Control.Monad (forM, unless)
import Control.Monad.Except (MonadError (..))
import Control.Monad.State.Lazy (
  MonadState (..),
  StateT (..),
  evalStateT,
  gets,
  modify,
 )

import Data.Bifunctor (Bifunctor (..))
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Maybe (maybeToList)


binderVars :: [I.Binder I.Type] -> [(I.VarId, I.Type)]
binderVars (I.BindVar v t : bs) = (v, t) : binderVars bs
binderVars (_ : bs) = binderVars bs
binderVars [] = []


-- | Lifting Environment
data LiftCtx = LiftCtx
  { -- | 'globalScope' is a set containing top-level identifiers. All scopes,
    --  regardless of depth, have access to these identifiers.
    globalScope :: M.Map I.VarId I.Type
  , -- | 'currentScope' is a set containing the identifiers available in the
    --  current scope.
    currentScope :: M.Map I.VarId I.Type
  , -- | 'currentTrail' is a list of strings tracing the surrounding scopes in
    --   terms of language constructs and relevant identifiers. It is used for
    --   creating unique identifiers for lifted lambdas.
    currentTrail :: [Identifier]
  , -- | Free variable encounetered during the a descent. These need to be added
    --      as arguments to lifted closures, and applied at the original site of the
    --      closure.
    currentFreeVars :: M.Map I.VarId I.Type
  , -- | 'lifted' is a list of lifted lambdas created while descending into a
    --  top-level definition.
    lifted :: [(I.VarId, I.Expr I.Type)]
  , -- | 'anonCount' is a monotonically increasing counter used for creating
    --  unique identifiers for lifted lambdas.
    freshCounter :: Int
  }


-- | Lift Monad
newtype LiftFn a = LiftFn (StateT LiftCtx Compiler.Pass a)
  deriving (Functor) via (StateT LiftCtx Compiler.Pass)
  deriving (Applicative) via (StateT LiftCtx Compiler.Pass)
  deriving (Monad) via (StateT LiftCtx Compiler.Pass)
  deriving (MonadFail) via (StateT LiftCtx Compiler.Pass)
  deriving (MonadError Compiler.Error) via (StateT LiftCtx Compiler.Pass)
  deriving (MonadState LiftCtx) via (StateT LiftCtx Compiler.Pass)


-- | Run a LiftFn computation.
runLiftFn :: [(I.VarId, I.Type)] -> LiftFn a -> Compiler.Pass a
runLiftFn globals (LiftFn m) =
  evalStateT
    m
    LiftCtx
      { globalScope = M.fromList globals
      , currentScope = M.empty
      , currentFreeVars = M.empty
      , currentTrail = []
      , lifted = []
      , freshCounter = 0
      }


-- | Create a string composed of the scope trail and a variable name.
prependTrail :: Identifier -> LiftFn Identifier
prependTrail cur = do
  curTrail <- gets currentTrail
  return $ mconcat $ intersperse "_" $ reverse $ cur : curTrail


-- | Construct a fresh variable name for a new lifted lambda.
getFresh :: LiftFn Identifier
getFresh = do
  curCount <- gets freshCounter
  modify $ \st -> st{freshCounter = freshCounter st + 1}
  return $ "anon" <> fromString (show curCount)


-- | Update lift environment before entering a new scope (e.g. lambda body, match arm).
withEnclosingScope :: Maybe Identifier -> [I.Binder I.Type] -> LiftFn a -> LiftFn a
withEnclosingScope t (binderVars -> s) m = do
  (scope, trail) <- (,) <$> gets currentScope <*> gets currentTrail

  modify $ \st ->
    st
      { currentScope = M.fromList s `M.union` scope -- Add bindings to inner scope
      , currentTrail = maybeToList t <> trail -- Possibly extend trail
      }

  a <- m

  modify $ \st ->
    st -- Restore state
      { currentScope = scope
      , currentTrail = trail
      }
  return a


withLiftedScope :: Maybe Identifier -> [I.Binder I.Type] -> LiftFn a -> LiftFn (a, [(I.VarId, I.Type)])
withLiftedScope t (binderVars -> s) m = do
  (scope, trail, free) <- (,,) <$> gets currentScope <*> gets currentTrail <*> gets currentFreeVars

  modify $ \st ->
    st
      { currentFreeVars = M.empty -- Reset accounting of free variables encountered
      , currentScope = M.fromList s -- Clear the currentScope (which will not exist when the inner scope is lifted to the global scope)
      , currentTrail = maybeToList t <> trail -- Possibly extend trail
      }

  a <- m

  free' <- gets currentFreeVars
  modify $ \st ->
    st -- Restore state
      { currentFreeVars = free
      , currentScope = scope
      , currentTrail = trail
      }
  return (a, M.toList free')


-- | Store a new lifted lambda to later add to the program's top level definitions.
tellLifted :: Identifier -> I.Expr I.Type -> LiftFn ()
tellLifted name lam =
  modify $ \st -> st{lifted = (fromId name, lam) : lifted st}


-- | Context management for lifting top level lambda definitions.
extractLifted :: LiftFn [(I.VarId, I.Expr I.Type)]
extractLifted = do
  lifted' <- gets lifted
  modify $ \st -> st{lifted = []}
  return $ reverse lifted'


{- | Entry-point to lambda lifting.

Maps over top level definitions and lifts out lambda definitions to create a new
Program with the relative order of user definitions preserved.
-}
liftProgramLambdas :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
liftProgramLambdas p@I.Program{I.programDefs = defs} = runLiftFn (map (second I.extract) defs) $ do
  liftedProgramDefs <- mapM liftTop defs
  return $ p{I.programDefs = concat liftedProgramDefs}
 where
  liftTop (v, lam@I.Lambda{}) = do
    let (bs, body) = I.unfoldLambda lam
    body' <- withEnclosingScope (Just $ fromId v) bs $ liftLambdas body
    liftedLambdas <- extractLifted
    return $ liftedLambdas ++ [(v, I.foldLambda bs body')]
  liftTop topDef = return [topDef]


{- | Lifting logic for IR expressions.

As we traverse over IR expressions, we note down any bindings we encounter so
that we can detect free variables. For lambda definitions, we use free
variables to create a new top-level lifted equivalent and then adjust the
callsite by partially-applying the new lifted lambda with those free variables
from the surrounding the scope.
-}
liftLambdas :: I.Expr I.Type -> LiftFn (I.Expr I.Type)
liftLambdas n@(I.Var v t) = do
  scope <- M.union <$> gets currentScope <*> gets globalScope
  unless (v `M.member` scope) $ do
    -- @v@ appears free in the current scope; make a note of it so we know to
    -- apply it when lifting the enclosing closure.
    modify $ \st -> st{currentFreeVars = M.insert v t $ currentFreeVars st}
  return n
liftLambdas (I.App e1 e2 t) = I.App <$> liftLambdas e1 <*> liftLambdas e2 <*> pure t
liftLambdas (I.Prim p exprs t) = I.Prim p <$> mapM liftLambdas exprs <*> pure t
liftLambdas lam@(I.Lambda _ _ t) = do
  let (bs, body) = I.unfoldLambda lam
  lamName <- getFresh
  fullName <- prependTrail lamName
  (body', free') <- withLiftedScope (Just lamName) bs $ do
    liftLambdas body

  let freeActuals = map (\(v', t') -> (I.Var v' t', t')) free'
      liftedLam = I.foldLambda (map (uncurry I.BindVar) free' ++ bs) body'

  tellLifted fullName liftedLam

  return $ I.foldApp (I.Var (fromId fullName) t) freeActuals
liftLambdas (I.Let ds e t) = do
  ds' <- forM ds $ \(b@(I._binderId -> v), d) ->
    withEnclosingScope (fromId <$> v) (map fst ds) $ do
      (b,) <$> liftLambdas d
  I.Let ds' <$> liftLambdas e <*> pure t
liftLambdas (I.Match s as t) = do
  s' <- liftLambdas s
  as' <- forM as $ \(a, e) -> withEnclosingScope Nothing (I.altBinders a) $ (a,) <$> liftLambdas e
  return $ I.Match s' as' t
liftLambdas lit@I.Lit{} = return lit
liftLambdas dat@I.Data{} = return dat
liftLambdas e@I.Exception{} = return e
