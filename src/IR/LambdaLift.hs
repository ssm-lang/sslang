{-# LANGUAGE DerivingVia #-}
module IR.LambdaLift where

import qualified Common.Compiler               as Compiler
import           Common.Identifiers
import qualified IR.IR                         as I

import qualified IR.Types.Poly                 as Poly

import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                , gets
                                                , modify
                                                , unless
                                                )
import           Debug.Trace

import           Data.Maybe                     ( catMaybes )
import qualified Data.Set                      as S


data LiftCtx = LiftCtx
  { globalScope  :: S.Set I.VarId
  , currentScope :: S.Set I.VarId
  , currentFrees :: S.Set I.VarId
  , lifted       :: [(I.VarId, I.Expr Poly.Type)]
  , anonCount    :: Int
  }

newtype LiftFn a = LiftFn (StateT LiftCtx Compiler.Pass a)
  deriving Functor                      via (StateT LiftCtx Compiler.Pass)
  deriving Applicative                  via (StateT LiftCtx Compiler.Pass)
  deriving Monad                        via (StateT LiftCtx Compiler.Pass)
  deriving MonadFail                    via (StateT LiftCtx Compiler.Pass)
  deriving (MonadError Compiler.Error)  via (StateT LiftCtx Compiler.Pass)
  deriving (MonadState LiftCtx)         via (StateT LiftCtx Compiler.Pass)

runLiftFn :: LiftFn a -> Compiler.Pass a
runLiftFn (LiftFn m) = evalStateT
  m
  LiftCtx { globalScope  = S.empty
          , currentScope = S.empty
          , currentFrees = S.empty
          , lifted       = []
          , anonCount    = 0
          }

populateGlobalScope :: [(I.VarId, I.Expr Poly.Type)] -> LiftFn ()
populateGlobalScope defs = do
  let globalNames = map fst defs
  modify $ \st -> st { globalScope = S.fromList globalNames }

inCurrentScope :: I.VarId -> LiftFn Bool
inCurrentScope v = S.member v <$> gets currentScope

addCurrentScope :: I.VarId -> LiftFn ()
addCurrentScope v =
  modify $ \st -> st { currentScope = S.insert v $ currentScope st }

getFresh :: LiftFn String
getFresh = do
  curCount <- gets anonCount
  modify $ \st -> st { anonCount = anonCount st + 1 }
  return $ "anon" ++ show curCount

addLifted :: String -> I.Expr Poly.Type -> LiftFn ()
addLifted name lam =
  modify $ \st -> st { lifted = (I.VarId (Identifier name), lam) : lifted st }

addFreeVar :: I.VarId -> LiftFn ()
addFreeVar v =
  modify $ \st -> st { currentFrees = S.insert v $ currentFrees st }

newScope :: [I.VarId] -> LiftFn ()
newScope vs = modify $ \st -> st
  { currentScope = S.union (globalScope st) (S.fromList vs)
  , currentFrees = S.empty
  }

makeLiftedLambda
  :: [I.Binder] -> I.Expr Poly.Type -> Poly.Type -> LiftFn (I.Expr Poly.Type)
makeLiftedLambda [] body _ = return body
makeLiftedLambda vs body t = do
  liftedBody <- makeLiftedLambda (tail vs) body t
  traceM (show vs)
  return (I.Lambda (head vs) liftedBody t)

liftLambdas'
  :: (I.VarId, I.Expr Poly.Type) -> LiftFn (I.VarId, I.Expr Poly.Type)
liftLambdas' (v, lam@(I.Lambda _ _ t)) = do
  let (vs, body) = I.collectLambda lam
  newScope $ catMaybes vs
  liftedBody <- liftLambdas body
  return (v, foldl (\lam' v' -> I.Lambda v' lam' t) liftedBody vs)
liftLambdas' _ = error "Expected top-level lambda binding"

descend :: LiftFn a -> LiftFn (a, S.Set VarId)
descend lb = do
  savedScope    <- gets currentScope
  savedFrees    <- gets currentFrees
  liftedLamBody <- lb
  lamFrees      <- gets currentFrees
  modify $ \st ->
    st { currentScope = savedScope, currentFrees = savedFrees }
  return (liftedLamBody, lamFrees)

liftLambdas :: I.Expr Poly.Type -> LiftFn (I.Expr Poly.Type)
liftLambdas n@(I.Var v _) = do
  inScope <- inCurrentScope v
  unless inScope $ addFreeVar v
  return n
liftLambdas (I.App e1 e2 t) = do
  liftedE1 <- liftLambdas e1
  liftedE2 <- liftLambdas e2
  return $ I.App liftedE1 liftedE2 t
liftLambdas (I.Prim p exprs t) = do
  liftedExprs <- mapM liftLambdas exprs
  return $ I.Prim p liftedExprs t
liftLambdas lam@(I.Lambda _ _ t) = do
  let (vs, body) = I.collectLambda lam
  traceM "Lambda"
  (liftedLamBody, lamFrees) <-
    descend $ newScope (catMaybes vs) >> liftLambdas body
  liftedLam <- makeLiftedLambda (map Just (S.toList lamFrees) ++ vs)
                                liftedLamBody
                                t
  freshName <- getFresh
  addLifted freshName liftedLam
  return $ foldl (\app v -> I.App app (I.Var v t) t)
                 (I.Var (I.VarId (Identifier freshName)) t)
                 (S.toList lamFrees)

liftLambdas (I.Let bs e t) = do
  let vs    = map fst bs
      exprs = map snd bs
  traceM "Let"
  mapM_ addCurrentScope (catMaybes vs)
  liftedLetBodies <- mapM liftLambdas exprs
  liftedExpr      <- liftLambdas e
  return $ I.Let (zip vs liftedLetBodies) liftedExpr t
liftLambdas n = return n

liftProgramLambdas
  :: I.Program Poly.Type -> Compiler.Pass (I.Program Poly.Type)
liftProgramLambdas p = runLiftFn $ do
  let defs = I.programDefs p
      funs = filter isFun defs
      oths = filter (not . isFun) defs
  populateGlobalScope defs
  funsWithLiftedBodies <- mapM liftLambdas' funs
  liftedLambdas        <- gets lifted
  traceM "finished Lifting"
  return $ p { I.programDefs = oths ++ liftedLambdas ++ funsWithLiftedBodies }
 where
  isFun (_, I.Lambda{}) = True
  isFun _               = False
