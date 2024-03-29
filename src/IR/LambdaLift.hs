{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified IR.MangleNames as I
import qualified IR.Pretty ()
import qualified IR.Types as I

import Control.Monad (forM, forM_, unless)
import Control.Monad.Except (MonadError (..))
import Control.Monad.State.Lazy (
  MonadState (..),
  StateT (..),
  gets,
  modify,
 )

import Data.Bifunctor (Bifunctor (..))
import Data.Generics (everywhere, mkT)
import Data.List (intersperse, tails)
import Data.Map ((\\))
import qualified Data.Map as M
import Data.Maybe (mapMaybe, maybeToList)


binderVars :: [I.Binder I.Type] -> [(I.VarId, I.Type)]
binderVars (I.BindVar v t : bs) = (v, t) : binderVars bs
binderVars (_ : bs) = binderVars bs
binderVars [] = []


-- | Lifting Environment
data LiftCtx = LiftCtx
  { -- | 'globalScope' is a set containing top-level identifiers. All scopes,
    -- regardless of depth, have access to these identifiers.
    globalScope :: M.Map I.VarId I.Type
  , -- | 'currentScope' is a set containing the identifiers available in the
    -- current scope.
    currentScope :: M.Map I.VarId I.Type
  , -- | 'currentTrail' is a list of strings tracing the surrounding scopes in
    -- terms of language constructs and relevant identifiers. It is used for
    -- creating unique identifiers for lifted lambdas.
    currentTrail :: [I.VarId]
  , -- | Free variable encounetered during the a descent. These need to be added
    -- as arguments to lifted closures, and applied at the original site of the
    -- closure.
    currentFreeVars :: M.Map I.VarId I.Type
  , -- | 'lifted' is a list of lifted lambdas created while descending into a
    -- top-level definition.
    lifted :: [(I.Binder I.Type, I.Expr I.Type)]
  , -- | 'symTable' is used to generate fresh variable names, using 'I.pickId'.
    symTable :: I.SymTable I.Type
  }


-- | Lift Monad
newtype LiftFn a = LiftFn (StateT LiftCtx Compiler.Pass a)
  deriving (Functor) via (StateT LiftCtx Compiler.Pass)
  deriving (Applicative) via (StateT LiftCtx Compiler.Pass)
  deriving (Monad) via (StateT LiftCtx Compiler.Pass)
  deriving (MonadFail) via (StateT LiftCtx Compiler.Pass)
  deriving (MonadError Compiler.Error) via (StateT LiftCtx Compiler.Pass)
  deriving (MonadState LiftCtx) via (StateT LiftCtx Compiler.Pass)


-- | Unwrap the lift monad.
unLiftFn :: LiftFn a -> StateT LiftCtx Compiler.Pass a
unLiftFn (LiftFn a) = a


-- | Generate a fresh name from an origin, the context trail, and the sym table.
genName :: I.VarId -> I.Type -> LiftFn I.VarId
genName origin t = do
  trail <- gets currentTrail
  syms <- gets symTable
  let origin' = mconcat $ intersperse "_" $ reverse $ origin : trail
      name = I.pickId syms origin'
      syms' = M.insert name I.SymInfo{I.symOrigin = fromId origin, I.symType = t} syms

  modify $ \ctx -> ctx{symTable = syms'}

  return name


-- | Update lift environment before entering a new scope (e.g. non-recursive let definition, match arm).
withEnclosingScope :: Maybe I.VarId -> [I.Binder I.Type] -> LiftFn a -> LiftFn a
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


withLiftedScope :: Maybe I.VarId -> [I.Binder I.Type] -> LiftFn a -> LiftFn (a, [(I.VarId, I.Type)])
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
      { currentFreeVars = free `M.union` (free' \\ scope)
      , currentScope = scope
      , currentTrail = trail
      }
  return (a, M.toList free')


-- | Store a new lifted lambda to later add to the program's top level definitions.
tellLifted :: I.VarId -> I.Expr I.Type -> LiftFn ()
tellLifted name lam =
  modify $ \st -> st{lifted = (I.BindVar (fromId name) (I.extract lam), lam) : lifted st}


-- | Context management for liftifreshNameng top level lambda definitions.
extractLifted :: LiftFn [(I.Binder I.Type, I.Expr I.Type)]
extractLifted = do
  lifted' <- gets lifted
  modify $ \st -> st{lifted = []}
  return $ reverse lifted'


{- | Entry-point to lambda lifting.

Maps over top level definitions and lifts out lambda definitions to create a new
Program with the relative order of user definitions preserved.
-}
liftProgramLambdas :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
liftProgramLambdas p@I.Program{I.programDefs = defs, I.symTable = syms} = do
  (defs', symTable -> syms') <- runStateT (unLiftFn $ mapM liftTop defs) initCtx
  return p{I.programDefs = concat defs', I.symTable = syms'}
 where
  initCtx =
    LiftCtx
      { globalScope = M.fromList globals
      , currentScope = M.empty
      , currentFreeVars = M.empty
      , currentTrail = []
      , lifted = []
      , symTable = syms
      }

  globals = mapMaybe (extractBindVar . second I.extract) defs

  extractBindVar (I.binderToVar -> Just v, t) = Just (v, t)
  extractBindVar _ = Nothing

  liftTop (v, lam@I.Lambda{}) = do
    let (bs, body) = I.unfoldLambda lam
    body' <- withEnclosingScope (fromId <$> I.binderToVar v) bs $ liftLambdas body
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
liftLambdas lam@I.Lambda{} = do
  (lam', liftName, liftLam) <- liftLambda lam [] "__lambda"
  tellLifted liftName liftLam
  return lam'
liftLambdas (I.Let ds b t)
  | all (isLambda . snd) ds = do
    -- e.g.,  let f x = ...
    --            g y = ...
    --        ...
    -- Bindings (e.g., f, g) might be recursive and apppear inside definitions.
    let binders = map fst ds
    dsls' <- forM ds $ \(x, d) -> do
      (d', x', lam) <- liftLambda d binders $ maybe "__let_underscore" fromId $ I._binderId x
      return ((x, d'), (x', lam))

    let (xd, xl) = unzip dsls'
        xdMap = M.fromList $ mapMaybe makeMapping xd

        makeMapping :: (I.Binder I.Type, I.Expr I.Type) -> Maybe (I.VarId, I.Expr I.Type)
        makeMapping (I.BindVar x _, d') = Just (x, d')
        makeMapping _ = Nothing -- Should never be reachable, consider throwing error
        mapRecBinds :: I.Expr I.Type -> I.Expr I.Type
        mapRecBinds (I.App (I.Var x xt) args at) = case M.lookup x xdMap of
          Just d' -> I.App d' args at
          Nothing -> I.App (I.Var x xt) args at
        mapRecBinds e = e

    forM_ xl $ \(x', lam) -> do
      -- replace every instance of Var x to Var x' in lam
      let lam' = everywhere (mkT mapRecBinds) lam
      tellLifted x' lam'

    b' <- withEnclosingScope Nothing binders $ liftLambdas b
    return $ I.Let xd b' t
  | length ds == 1 = do
    -- e.g.,  let x = ...
    --        ...
    -- Binding is not recursive, so x cannot appear in definition.
    let (x, d) = head ds
    d' <- liftLambdas d
    e' <- withEnclosingScope Nothing [x] $ liftLambdas b
    return $ I.Let [(x, d')] e' t
  | otherwise = error $ "Let expressions should only bind a list of values, or a single non-value " ++ show ds
 where
  isLambda I.Lambda{} = True
  isLambda _ = False
liftLambdas (I.Match s as t) = do
  s' <- liftLambdas s
  as' <- forM as $ \(a, e) -> do
    e' <- withEnclosingScope Nothing (I.altBinders a) $ liftLambdas e
    return (a, e')
  return $ I.Match s' as' t
liftLambdas lit@I.Lit{} = return lit
liftLambdas dat@I.Data{} = return dat
liftLambdas e@I.Exception{} = return e


liftLambda :: I.Expr I.Type -> [I.Binder I.Type] -> I.VarId -> LiftFn (I.Expr I.Type, I.VarId, I.Expr I.Type)
liftLambda lam letBinds originName = do
  let (bs, body) = I.unfoldLambda lam
  (body', free) <- withLiftedScope (Just originName) (letBinds ++ bs) $ do
    liftLambdas body

  let -- Helper function to prepend arguments to a function type
      prependArrow ts t' = let (ats', rt') = I.unfoldArrow t' in I.foldArrow (ts ++ ats', rt')

      -- 'tails' of the types of free variables in lambda body
      (liftedLamArgTypes : intermediateTypes) = tails $ map snd free

      liftedLamType = prependArrow liftedLamArgTypes $ I.extract lam

      -- Construct arguments to be folded into the call site
      freeActuals = zipWith (\(v', t') ts -> (I.Var v' t', prependArrow ts (I.extract lam))) free intermediateTypes

  -- Generate a fresh name for the lifted lambda
  liftedName <- genName originName liftedLamType

  let -- Replace lambda with call to lifted top-level lambda applied to all free variables
      replacement = I.foldApp (I.Var (fromId liftedName) liftedLamType) freeActuals

      -- The lifted lambda expression, which should be added to the global list
      liftedLambda = I.foldLambda (map (uncurry I.BindVar) free ++ bs) body'

  return (replacement, liftedName, liftedLambda)
