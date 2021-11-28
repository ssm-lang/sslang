{-# LANGUAGE DerivingVia #-}
module IR.LambdaLift where

import qualified Common.Compiler               as Compiler
import           Common.Identifiers
import qualified IR.IR                         as I

import qualified IR.Types.Poly                 as Poly
import           IR.Types.TypeSystem            ( arrow
                                                , collectArrow
                                                , dearrow
                                                )

import           Control.Comonad                ( Comonad(..) )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                , gets
                                                , modify
                                                , unless
                                                )

import qualified Data.Bifunctor                as B
import qualified Data.Foldable                 as F
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes )
import qualified Data.Set                      as S

data LiftCtx = LiftCtx
  { globalScope  :: S.Set I.VarId
  , currentScope :: S.Set I.VarId
  , freeTypes    :: M.Map I.VarId Poly.Type
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
          , freeTypes    = M.empty
          , lifted       = []
          , anonCount    = 0
          }

populateGlobalScope :: [(I.VarId, I.Expr Poly.Type)] -> LiftFn ()
populateGlobalScope defs = do
  let (globalNames, _) = unzip defs
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

addFreeVar :: I.VarId -> Poly.Type -> LiftFn ()
addFreeVar v t = modify $ \st -> st { freeTypes = M.insert v t $ freeTypes st }

newScope :: [I.VarId] -> LiftFn ()
newScope vs = modify $ \st -> st
  { currentScope = S.union (globalScope st) (S.fromList vs)
  , freeTypes    = M.empty
  }

descend :: LiftFn a -> LiftFn (a, M.Map VarId Poly.Type)
descend body = do
  savedScope     <- gets currentScope
  savedFreeTypes <- gets freeTypes
  liftedBody     <- body
  freeTypesBody  <- gets freeTypes
  modify $ \st -> st { currentScope = savedScope, freeTypes = savedFreeTypes }
  return (liftedBody, freeTypesBody)

liftProgramLambdas
  :: I.Program Poly.Type -> Compiler.Pass (I.Program Poly.Type)
liftProgramLambdas p = runLiftFn $ do
  let defs = I.programDefs p
      funs = filter isFun defs
      oths = filter (not . isFun) defs
  populateGlobalScope defs
  funsWithLiftedBodies <- mapM liftLambdasTop funs
  liftedLambdas        <- gets lifted
  return $ p { I.programDefs = oths ++ liftedLambdas ++ funsWithLiftedBodies }
 where
  isFun (_, I.Lambda{}) = True
  isFun _               = False

liftLambdasTop
  :: (I.VarId, I.Expr Poly.Type) -> LiftFn (I.VarId, I.Expr Poly.Type)
liftLambdasTop (v, lam@(I.Lambda _ _ t)) = do
  let (vs, body) = I.collectLambda lam
      vs'        = zip vs $ fst (collectArrow t)
  newScope $ catMaybes vs
  liftedBody   <- liftLambdas body
  liftedLambda <- makeLiftedLambda vs' liftedBody
  return (v, liftedLambda)
liftLambdasTop _ = error "Expected top-level lambda binding"

liftLambdas :: I.Expr Poly.Type -> LiftFn (I.Expr Poly.Type)
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
  let (vs, body) = I.collectLambda lam
      vs'        = zip vs $ fst (collectArrow t)
  (liftedLamBody, lamFreeTypes) <-
    descend $ newScope (catMaybes vs) >> liftLambdas body
  liftedLam <- makeLiftedLambda
    (map (B.first Just) (M.toList lamFreeTypes) ++ vs')
    liftedLamBody
  freshName <- getFresh
  addLifted freshName liftedLam
  return $ foldl applyFree
                 (I.Var (I.VarId (Identifier freshName)) (extract liftedLam))
                 (M.toList lamFreeTypes)
 where
  applyFree app (v', t') = case dearrow $ extract app of
    Just (_, tr) -> I.App app (I.Var v' t') tr
    Nothing      -> app
liftLambdas (I.Let bs e t) = do
  let (vs, exprs) = unzip bs
  mapM_ addCurrentScope (catMaybes vs)
  liftedLetBodies <- mapM liftLambdas exprs
  liftedExpr      <- liftLambdas e
  return $ I.Let (zip vs liftedLetBodies) liftedExpr t
liftLambdas (I.Match s arms t) = do
  liftedMatch <- liftLambdas s
  liftedArms  <- mapM liftLambdasInArm arms
  return $ I.Match liftedMatch liftedArms t
liftLambdas lit@I.Lit{}  = return lit
liftLambdas dat@I.Data{} = return dat

liftLambdasInArm
  :: (I.Alt, I.Expr Poly.Type) -> LiftFn (I.Alt, I.Expr Poly.Type)
liftLambdasInArm (I.AltLit l, arm) = do
  (liftedArm, armFrees) <- descend $ liftLambdas arm
  modify $ \st -> st { freeTypes = armFrees }
  return (I.AltLit l, liftedArm)
liftLambdasInArm (I.AltDefault b, arm) = do
  (liftedArm, armFrees) <-
    descend $ F.forM_ b addCurrentScope >> liftLambdas arm
  modify $ \st -> st { freeTypes = armFrees }
  return (I.AltDefault b, liftedArm)
liftLambdasInArm (I.AltData d bs, arm) = do
  (liftedArm, armFrees) <-
    descend $ mapM_ addCurrentScope (catMaybes bs) >> liftLambdas arm
  modify $ \st -> st { freeTypes = armFrees }
  return (I.AltData d bs, liftedArm)

makeLiftedLambda
  :: [(I.Binder, Poly.Type)] -> I.Expr Poly.Type -> LiftFn (I.Expr Poly.Type)
makeLiftedLambda []            body = return body
makeLiftedLambda ((v, t) : vs) body = do
  liftedBody <- makeLiftedLambda vs body
  return (I.Lambda v liftedBody $ arrow t (extract liftedBody))
