{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Front.Pattern.Desugar where

import           Common.Compiler                ( Error(..)
                                                , ErrorMsg
                                                , MonadError(..)
                                                , MonadWriter
                                                , Pass(..)
                                                , Warning(..)
                                                , fromString
                                                )
import           Common.Identifiers             ( Identifiable(..)
                                                , Identifier(..)
                                                , isCons
                                                , isVar
                                                )
import           Control.Monad                  ( replicateM
                                                , unless
                                                , when
                                                )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                , asks
                                                )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                , forM
                                                , gets
                                                , modify
                                                )
import           Data.Bifunctor                 ( first )
import           Data.Foldable                  ( find )
import qualified Data.Map                      as M
import           Data.Maybe                     ( mapMaybe )
import qualified Data.Set                      as S
import qualified Front.Ast                     as A
import           Front.Pattern.Common           ( CInfo(..)
                                                , TInfo(..)
                                                , buildConsMap
                                                , buildTypeMap
                                                )
import qualified Front.Pattern.Matrix          as PM
import qualified Front.Pattern.Vector          as PV

type Equation = ([A.Pat], A.Expr)
type Arm = (A.Pat, A.Expr)

data DesugarCtx = DesugarCtx
  { typeMap   :: M.Map Identifier TInfo
  , consMap   :: M.Map Identifier CInfo
  , anonCount :: Int
  }

newtype DesugarFn a = DesugarFn (StateT DesugarCtx Pass a)
  deriving Functor                      via (StateT DesugarCtx Pass)
  deriving Applicative                  via (StateT DesugarCtx Pass)
  deriving Monad                        via (StateT DesugarCtx Pass)
  deriving MonadFail                    via (StateT DesugarCtx Pass)
  deriving (MonadError Error)  via (StateT DesugarCtx Pass)
  deriving (MonadState DesugarCtx)     via (StateT DesugarCtx Pass)

runDesugarFn :: DesugarFn a -> DesugarCtx -> Pass a
runDesugarFn (DesugarFn m) = evalStateT m

freshVar :: DesugarFn Identifier
freshVar = do
  currCount <- gets anonCount
  modify $ \ctx -> ctx { anonCount = anonCount ctx + 1 }
  return $ fromString ("anon" ++ show currCount)

buildCtx :: [A.TypeDef] -> DesugarCtx
buildCtx tds = DesugarCtx { typeMap   = buildTypeMap tds
                          , consMap   = buildConsMap tds
                          , anonCount = 0
                          }


desugarProgram :: A.Program -> Pass A.Program
desugarProgram (A.Program defs) = runDesugarFn
  (A.Program <$> desugarTopDefs defs)
  ctx
 where
  tds = mapMaybe A.getTopTypeDef defs
  ctx = buildCtx tds

desugarTopDefs :: [A.TopDef] -> DesugarFn [A.TopDef]
desugarTopDefs []                    = return []
desugarTopDefs ((A.TopDef d) : tops) = do
  d'    <- desugarDef d
  tops' <- desugarTopDefs tops
  return $ A.TopDef d' : tops'
desugarTopDefs (tt@(A.TopType _) : tops) = do
  tops' <- desugarTopDefs tops
  return $ tt : tops'

desugarDefs :: [A.Definition] -> DesugarFn [A.Definition]
desugarDefs = mapM desugarDef

-- WARN: only body is desugared
desugarDef :: A.Definition -> DesugarFn A.Definition
desugarDef (A.DefFn i ps t e) = A.DefFn i ps t <$> desugarExpr e
desugarDef (A.DefPat p e    ) = A.DefPat p <$> desugarExpr e

desugarExprs :: [A.Expr] -> DesugarFn [A.Expr]
desugarExprs = mapM desugarExpr

desugarExpr :: A.Expr -> DesugarFn A.Expr
desugarExpr e@(A.Id  _       ) = return e
desugarExpr e@(A.Lit _       ) = return e
desugarExpr (  A.Apply  e1 e2) = A.Apply <$> desugarExpr e1 <*> desugarExpr e2
desugarExpr (  A.Lambda ps e ) = A.Lambda ps <$> desugarExpr e -- WARN: patterns here are not checked
desugarExpr (A.OpRegion e opRegion) =
  A.OpRegion <$> desugarExpr e <*> desugarOpRegion opRegion
desugarExpr A.NoExpr        = return A.NoExpr
desugarExpr (A.Let   ds e ) = A.Let <$> desugarDefs ds <*> desugarExpr e
desugarExpr (A.While e1 e2) = A.While <$> desugarExpr e1 <*> desugarExpr e2
desugarExpr (A.Loop e     ) = A.Loop <$> desugarExpr e
desugarExpr (A.Par  es    ) = A.Par <$> desugarExprs es
desugarExpr (A.IfElse e1 e2 e3) =
  A.IfElse <$> desugarExpr e1 <*> desugarExpr e2 <*> desugarExpr e3
desugarExpr (A.After e1 e2 e3) =
  A.After <$> desugarExpr e1 <*> desugarExpr e2 <*> desugarExpr e3
desugarExpr (A.Assign e1 e2) = A.Assign <$> desugarExpr e1 <*> desugarExpr e2
desugarExpr (A.Constraint e typann) =
  A.Constraint <$> desugarExpr e <*> return typann
desugarExpr (A.Wait es  )    = A.Wait <$> desugarExprs es
desugarExpr (A.Seq e1 e2)    = A.Seq <$> desugarExpr e1 <*> desugarExpr e2
desugarExpr A.Break          = return A.Break
desugarExpr (A.Match e arms) = do -- INFO: the only important one
  v <- freshVar
  let arms' = map (first (: [])) arms
  singleLet v e <$> desugarMatch [v] arms' A.NoExpr -- INFO: for now, default expression is NoExpr
desugarExpr (A.Return e) = A.Return <$> desugarExpr e

desugarOpRegion :: A.OpRegion -> DesugarFn A.OpRegion
desugarOpRegion (A.NextOp i e opRegion) =
  A.NextOp i <$> desugarExpr e <*> desugarOpRegion opRegion
desugarOpRegion A.EOR = return A.EOR

desugarMatch :: [Identifier] -> [Equation] -> A.Expr -> DesugarFn A.Expr
desugarMatch [] [] def = case def of
  A.NoExpr -> throwInexhaustionError -- INFO: for now, can't handle inexhaustive patterns
  _        -> return def
desugarMatch [] (([], e) : _) _   = return e
desugarMatch [] _             _   = throwDesugarError
desugarMatch us qs            def = do
  qs'  <- desugarMatchAsAnn us qs
  qs'' <- desugarMatchWild qs'
  foldrM (desugarMatchGen us) def (partitionEqs qs'')

desugarMatchGen :: [Identifier] -> [Equation] -> A.Expr -> DesugarFn A.Expr
desugarMatchGen us qs def | isVarEq (head qs)  = desugarMatchVar us qs def
                          | isConsEq (head qs) = desugarMatchCons us qs def
                          | isLitEq (head qs)  = desugarMatchLit us qs def
                          | isTupEq (head qs)  = desugarMatchTup us qs def
                          | otherwise          = error "can't happen"
 where
  isVarEq (ps, _) = case head ps of
    A.PatId i -> isVar i
    _         -> False
  isConsEq (ps, _) = case head ps of
    A.PatId  i -> isCons i
    A.PatApp _ -> True
    _          -> False
  isLitEq (ps, _) = case head ps of
    A.PatLit _ -> True
    _          -> False
  isTupEq (ps, _) = case head ps of
    A.PatTup _ -> True
    _          -> False

desugarMatchVar :: [Identifier] -> [Equation] -> A.Expr -> DesugarFn A.Expr
desugarMatchVar [] _ _ = error "can't happen"
desugarMatchVar (u : us) qs def =
  desugarMatch us [ (ps, singleAlias v u e) | (A.PatId v : ps, e) <- qs ] def -- INFO: is this v, u order correct?

desugarMatchCons :: [Identifier] -> [Equation] -> A.Expr -> DesugarFn A.Expr
desugarMatchCons []       _  _   = error "can't happen"
desugarMatchCons (u : us) qs def = do
  cs   <- getConstructors . getCon . head $ qs
  arms <- sequence
    [ desugarMatchConsArm c (u : us) (choose c) def | c <- S.toList cs ]
  return $ A.Match (A.Id u) arms
 where
  getCon (A.PatId i : _, _) = i
  getCon ((A.PatApp (A.PatId i : _)) : _, _) = i
  getCon _ = error "can't happen"
  choose c = [ q | q <- qs, getCon q == c ]

desugarMatchConsArm
  :: Identifier -> [Identifier] -> [Equation] -> A.Expr -> DesugarFn Arm
desugarMatchConsArm _ []       _  _   = error "can't happen"
desugarMatchConsArm c (u : us) qs def = do
  k   <- getArity c
  us' <- replicateM k freshVar

  undefined -- TODO:

desugarMatchLit :: [Identifier] -> [Equation] -> A.Expr -> DesugarFn A.Expr
desugarMatchLit = undefined

desugarMatchTup :: [Identifier] -> [Equation] -> A.Expr -> DesugarFn A.Expr
desugarMatchTup = undefined

desugarMatchWild :: [Equation] -> DesugarFn [Equation]
desugarMatchWild []       = return []
desugarMatchWild (x : xs) = do
  xs' <- desugarMatchWild xs
  x'  <- desugar x
  return (x' : xs')
 where
  desugar (A.PatWildcard : rs, v) = do
    i <- freshVar
    return (A.PatId i : rs, v)
  desugar eq = return eq

desugarMatchAsAnn :: [Identifier] -> [Equation] -> DesugarFn [Equation]
desugarMatchAsAnn us@(u : _) (x : xs) = do
  xs' <- desugarMatchAsAnn us xs
  return (desugar x : xs')
 where
  desugar ((A.PatAs i r) : rs, v) = desugar (r : rs, singleAlias i u v)
  desugar ((A.PatAnn t r) : rs, v) =
    desugar (r : rs, singleAliasWithTyp u u t v)
  desugar eq = eq
desugarMatchAsAnn _ _ = return []

partitionEqs :: [Equation] -> [[Equation]]
partitionEqs []  = []
partitionEqs [x] = [[x]]
partitionEqs (x : x' : xs) | sameGroup x x' = tack x (partitionEqs (x' : xs))
                           | otherwise      = [x] : partitionEqs (x' : xs)
 where
  tack y yss = (y : head yss) : tail yss
  sameGroup (ys, _) (ys', _) = case (head ys, head ys') of
    (A.PatId  i, A.PatId i') -> isVar i == isVar i'
    (A.PatId  i, A.PatApp _) -> isCons i
    (A.PatApp _, A.PatId i ) -> isCons i
    (A.PatApp _, A.PatApp _) -> True
    (A.PatLit _, A.PatLit _) -> True
    (A.PatTup _, A.PatTup _) -> True
    _                        -> False

singleLet :: Identifier -> A.Expr -> A.Expr -> A.Expr
singleLet i e = A.Let [A.DefFn i [] A.TypNone e]

singleAlias :: Identifier -> Identifier -> A.Expr -> A.Expr
singleAlias alias i = A.Let [A.DefFn alias [] A.TypNone (A.Id i)]

singleAliasWithTyp :: Identifier -> Identifier -> A.Typ -> A.Expr -> A.Expr
singleAliasWithTyp alias i t =
  A.Let [A.DefFn alias [] (A.TypProper t) (A.Id i)]

getConstructors :: Identifier -> DesugarFn (S.Set Identifier)
getConstructors i = do
  c <- getCInfo i
  t <- getTInfo (cType c)
  return $ tCSet t

getArity :: Identifier -> DesugarFn Int
getArity i = do
  c <- getCInfo i
  return $ cArity c

getCInfo :: Identifier -> DesugarFn CInfo
getCInfo i = do
  cm <- gets $ M.lookup i . consMap
  maybe throwDesugarError return cm

getTInfo :: Identifier -> DesugarFn TInfo
getTInfo i = do
  tm <- gets $ M.lookup i . typeMap
  maybe throwDesugarError return tm

foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM _ d []       = return d
foldrM f d (x : xs) = f x =<< foldrM f d xs

throwDesugarError :: DesugarFn a
throwDesugarError = throwError $ PatternError "Can't desugar pattern match"

throwInexhaustionError :: DesugarFn a
throwInexhaustionError =
  throwError $ PatternError "Can't desugar inexhaustive pattern match"
