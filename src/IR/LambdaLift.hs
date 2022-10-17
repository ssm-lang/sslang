{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{- | Lift lambda definitions into the global scope.

This pass is responsible for moving nested lambda definitions into the global
scope and performing necessary callsite adjustments.
-}
module IR.LambdaLift
  ( liftProgramLambdas
  ) where

import qualified Common.Compiler               as Compiler
import           Common.Identifiers
import qualified IR.IR                         as I
import           IR.MangleNames                 ( pickId )
import qualified IR.Types                      as I

import           Control.Monad                  ( forM_ )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                , gets
                                                , modify
                                                , unless
                                                )

import           Data.Bifunctor                 ( first )
import           Data.List                      ( intersperse )
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes )
import qualified Data.Set                      as S

-- | Lifting Environment
data LiftCtx = LiftCtx
  { globalScope  :: S.Set I.VarId
  {- ^ 'globalScope' is a set containing top-level identifiers. All scopes,
  regardless of depth, have access to these identifiers.
  -}
  , currentScope :: S.Set I.VarId
  {- ^ 'currentScope' is a set containing the identifiers available in the
  current scope.
  -}
  , freeTypes    :: M.Map I.VarId I.Type
  {- ^ 'freeTypes' maps an identifier for a free variable to its type. -}
  , lifted       :: [(I.VarId, I.Expr I.Type)]
  {- ^ 'lifted' is a list of lifted lambdas created while descending into a
  top-level definition.
  -}
  , trail        :: [Identifier]
  {- ^ 'trail' is a list of strings tracing the surrounding scopes in terms of
  language constructs and relevant identifiers. It is used for
  creating unique identifiers for lifted lambdas.
  -}
  , anonCount    :: Int
  {- ^ 'anonCount' is a monotonically increasing counter used for creating
  unique identifiers for lifted lambdas.
  -}
  , varNames     :: M.Map I.VarId I.VarId
  {- ^ 'varNames' keeps track of all identifiers, for the purposes of ensure
  globaly unique identifiers.
  -}
  }

-- | Lift Monad
newtype LiftFn a = LiftFn (StateT LiftCtx Compiler.Pass a)
  deriving Functor                      via (StateT LiftCtx Compiler.Pass)
  deriving Applicative                  via (StateT LiftCtx Compiler.Pass)
  deriving Monad                        via (StateT LiftCtx Compiler.Pass)
  deriving MonadFail                    via (StateT LiftCtx Compiler.Pass)
  deriving (MonadError Compiler.Error)  via (StateT LiftCtx Compiler.Pass)
  deriving (MonadState LiftCtx)         via (StateT LiftCtx Compiler.Pass)

-- | Run a LiftFn computation.
runLiftFn :: LiftFn a -> Compiler.Pass a
runLiftFn (LiftFn m) = evalStateT
  m
  LiftCtx { globalScope  = S.empty
          , currentScope = S.empty
          , freeTypes    = M.empty
          , lifted       = []
          , trail        = []
          , anonCount    = 0
          , varNames     = M.empty
          }

-- | Extract top level definition names that compose program's global scope.
populateGlobalScope :: [(I.VarId, I.Expr I.Type)] -> LiftFn ()
populateGlobalScope defs = do
  let (globalNames, _) = unzip defs
  modify $ \st -> st { globalScope = S.fromList globalNames }

-- | Check if an identifier is in the current program scope.
inCurrentScope :: I.VarId -> LiftFn Bool
inCurrentScope v = S.member v <$> gets currentScope

-- | Add an identifier to the current program scope.
addCurrentScope :: I.VarId -> LiftFn ()
addCurrentScope v =
  modify $ \st -> st { currentScope = S.insert v $ currentScope st }

-- | Create a string composed of the scope trail and a variable name.
prependTrail :: Identifier -> LiftFn Identifier
prependTrail cur = do
  curTrail <- gets trail
  return $ mconcat $ intersperse "_" $ reverse $ cur : curTrail

-- | Construct a fresh variable name for a new lifted lambda.
getFresh :: LiftFn Identifier
getFresh = do
  curCount <- gets anonCount
  vns      <- gets varNames
  let (_, i) = pickId vns $ "__anon" <> fromString (show curCount)
      l      = "__generated_lambda_name__"
  modify
    $ \st -> st { anonCount = anonCount st + 1, varNames = M.insert l i vns }
  return $ fromId i

-- | Store a new lifted lambda to later add to the program's top level definitions.
addLifted :: Identifier -> I.Expr I.Type -> LiftFn ()
addLifted name lam =
  modify $ \st -> st { lifted = (fromId name, lam) : lifted st }

-- | Register a (free variable, type) mapping for the current program scope.
addFreeVar :: I.VarId -> I.Type -> LiftFn ()
addFreeVar v t = modify $ \st -> st { freeTypes = M.insert v t $ freeTypes st }

-- | Update lift environment before entering a new scope (e.g. lambda body, match arm).
newScope :: [I.VarId] -> LiftFn ()
newScope vs = modify $ \st -> st
  { currentScope = S.union (globalScope st) (S.fromList vs)
  , freeTypes    = M.empty
  }

-- | Optionally append to the scope trail.
maybeTrailAppend :: Maybe Identifier -> [Identifier] -> LiftFn ()
maybeTrailAppend (Just scopeName) trail' =
  modify (\st -> st { trail = scopeName : trail' })
maybeTrailAppend Nothing _ = return ()

-- | Context management for lifting new scopes (restore information after lifting the body).
descend
  :: Maybe Identifier
  -> LiftFn (I.Expr I.Type)
  -> LiftFn (I.Expr I.Type, M.Map VarId I.Type)
descend scopeName body = do
  savedScope     <- gets currentScope
  savedFreeTypes <- gets freeTypes
  savedTrail     <- gets trail
  liftedBody     <- maybeTrailAppend scopeName savedTrail >> body
  freeTypesBody  <- gets freeTypes
  modify $ \st -> st
    { currentScope = savedScope
    , freeTypes    = M.union (S.foldl (flip M.delete) freeTypesBody savedScope)
                             savedFreeTypes
    , trail        = savedTrail
    }
  return (liftedBody, freeTypesBody)

-- | Context management for lifting top level lambda definitions.
extractLifted
  :: LiftFn (I.Expr I.Type, M.Map VarId I.Type)
  -> LiftFn (I.Expr I.Type, [(I.VarId, I.Expr I.Type)])
extractLifted bodyDescended = do
  (liftedBody, _) <- bodyDescended -- A top-level body shouldn't have free variables.
  newTopDefs      <- gets lifted
  modify $ \st -> st { lifted = [] }
  return (liftedBody, reverse newTopDefs)

{- | Entry-point to lambda lifting.

Maps over top level definitions and lifts out lambda definitions to create a new
Program with the relative order of user definitions preserved.
-}
liftProgramLambdas :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
liftProgramLambdas p = runLiftFn $ do
  modify $ \st -> st { varNames = I.varNames p }
  let defs = I.programDefs p
  populateGlobalScope defs
  liftedProgramDefs <- mapM liftLambdasTop defs
  vns               <- gets varNames
  return $ p { I.programDefs = concat liftedProgramDefs, I.varNames = vns }

-- | Given a top-level definition, lift out any lambda definitions.
liftLambdasTop :: (I.VarId, I.Expr I.Type) -> LiftFn [(I.VarId, I.Expr I.Type)]
liftLambdasTop (v, lam@I.Lambda{}) = do
  let (vs, body) = I.unfoldLambda lam
      vs'        = zip vs $ fst (I.unfoldArrow $ I.extract lam)
  (liftedBody, newTopDefs) <- extractLifted $ descend (Just $ fromId v) $ do
    newScope (catMaybes vs)
    liftLambdas body
  return $ newTopDefs ++ [(v, I.foldLambda vs' liftedBody)]
liftLambdasTop topDef = return [topDef]

{- | Lifting logic for IR expressions.

As we traverse over IR expressions, we note down any bindings we encounter so
that we can detect free variables. For lambda definitions, we use free
variables to create a new top-level lifted equivalent and then adjust the
callsite by partially-applying the new lifted lambda with those free variables
from the surrounding the scope.
-}
liftLambdas :: I.Expr I.Type -> LiftFn (I.Expr I.Type)
liftLambdas n@(I.Var v t) = do
  inScope <- inCurrentScope v
  unless inScope $ addFreeVar v t
  return n
liftLambdas (I.App e1 e2 t) = do
  liftedE1 <- liftLambdas e1
  liftedE2 <- liftLambdas e2
  return $ I.App liftedE1 liftedE2 t
liftLambdas (I.Prim p exprs t) = do
  liftedExprs <- mapM liftLambdas exprs
  return $ I.Prim p liftedExprs t
liftLambdas lam@(I.Lambda _ _ t) = do
  let (vs, body) = I.unfoldLambda lam
      vs'        = zip vs $ fst (I.unfoldArrow t)
  lamName                       <- getFresh
  fullName                      <- prependTrail lamName
  (liftedLamBody, lamFreeTypes) <- descend (Just lamName) $ do
    newScope (catMaybes vs)
    liftLambdas body
  let liftedLam = I.foldLambda
        (map (first Just) (M.toList lamFreeTypes) ++ vs')
        liftedLamBody
  addLifted fullName liftedLam
  return $ foldl applyFree
                 (I.Var (fromId fullName) (I.extract liftedLam))
                 (M.toList lamFreeTypes)
 where
  applyFree app (v', t') = case I.extract app of
    I.Arrow _ rt -> I.App app (I.Var v' t') rt
    _            -> app
liftLambdas (I.Let bs e t) = do
  let vs = map fst bs
  mapM_ addCurrentScope (catMaybes vs)
  liftedLetBodies <- mapM liftLambdasInLet bs
  liftedExpr      <- liftLambdas e
  return $ I.Let (zip vs liftedLetBodies) liftedExpr t
liftLambdas (I.Match s arms t) = do
  liftedMatch <- liftLambdas s
  liftedArms  <- mapM liftLambdasInArm arms
  return $ I.Match liftedMatch liftedArms t
liftLambdas lit@I.Lit{}  = return lit
liftLambdas dat@I.Data{} = return dat

-- | Entry point for traversing let bindings.
liftLambdasInLet :: (I.Binder, I.Expr I.Type) -> LiftFn (I.Expr I.Type)
liftLambdasInLet (b, expr) = do
  (liftedLetBody, bodyFrees) <- descend (fmap (fromString . ident) b)
    $ liftLambdas expr
  modify $ \st -> st { freeTypes = bodyFrees }
  return liftedLetBody

-- | Entry point for traversing the arms of match expressions.
liftLambdasInArm :: (I.Alt, I.Expr I.Type) -> LiftFn (I.Alt, I.Expr I.Type)
liftLambdasInArm (I.AltLit l, arm) = do
  (liftedArm, armFrees) <- descend Nothing $ liftLambdas arm
  modify $ \st -> st { freeTypes = armFrees }
  return (I.AltLit l, liftedArm)
liftLambdasInArm (I.AltDefault b, arm) = do
  (liftedArm, armFrees) <- descend Nothing $ do
    forM_ b addCurrentScope
    liftLambdas arm
  modify $ \st -> st { freeTypes = armFrees }
  return (I.AltDefault b, liftedArm)
liftLambdasInArm (I.AltData d bs, arm) = do
  (liftedArm, armFrees) <- descend Nothing $ do
    mapM_ addCurrentScope (catMaybes bs)
    liftLambdas arm
  modify $ \st -> st { freeTypes = armFrees }
  return (I.AltData d bs, liftedArm)
