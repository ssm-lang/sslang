{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Front.Pattern.Desugar where

import           Common.Compiler                ( Error(..)
                                                , MonadError(..)
                                                , Pass(..)
                                                , fromString
                                                )
import           Common.Identifiers             ( Identifier(..)
                                                , isCons
                                                )
import           Control.Monad                  ( replicateM )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                , gets
                                                , modify
                                                )
import           Data.Bifunctor                 ( first )
import qualified Data.Map                      as M
import           Data.Maybe                     ( mapMaybe )
import qualified Data.Set                      as S
import qualified Front.Ast                     as A
import           Front.Pattern.Common           ( CInfo(..)
                                                , TInfo(..)
                                                , buildConsMap
                                                , buildTypeMap
                                                )

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
  return $ fromString ("__pat_anon" ++ show currCount)

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
  arms' <- mapM
    (\(p, body) -> do
      body' <- desugarExpr body
      return (p, body')
    )
    arms
  let armsForDesugar = map (first (: [])) arms'
  case e of
    A.Id v -> desugarMatch [v] armsForDesugar A.NoExpr
    _      -> do
      v <- freshVar
      singleLet v e <$> desugarMatch [v] armsForDesugar A.NoExpr -- INFO: for now, default expression is NoExpr
desugarExpr (A.Return e) = A.Return <$> desugarExpr e

desugarOpRegion :: A.OpRegion -> DesugarFn A.OpRegion
desugarOpRegion (A.NextOp i e opRegion) =
  A.NextOp i <$> desugarExpr e <*> desugarOpRegion opRegion
desugarOpRegion A.EOR = return A.EOR

desugarMatch :: [Identifier] -> [Equation] -> A.Expr -> DesugarFn A.Expr
desugarMatch [] [] def = case def of
  A.NoExpr -> throwInexhaustionError -- INFO: for now, can't handle inexhaustive patterns
  _        -> return def
desugarMatch []         (([], e) : _) _   = return e
desugarMatch []         _             _   = throwDesugarError
desugarMatch us@(_ : _) qs            def = do
  qs' <- desugarMatchAsAnn us qs >>= desugarMatchWild >>= desugarMatchIdCons
  foldrM (desugarMatchGen us) def (partitionEqs qs')

desugarMatchGen :: [Identifier] -> [Equation] -> A.Expr -> DesugarFn A.Expr
desugarMatchGen us qs def | isVarEq (head qs)  = desugarMatchVar us qs def
                          | isConsEq (head qs) = desugarMatchCons us qs def
                          | isLitEq (head qs)  = desugarMatchLit us qs def
                          | isTupEq (head qs)  = desugarMatchTup us qs def
                          | otherwise          = error "can't happen"
 where
  isVarEq (ps, _) = case head ps of
    A.PatId _ -> True
    _         -> False
  isConsEq (ps, _) = case head ps of
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

{-
Assume only having PatApp
-}
desugarMatchCons :: [Identifier] -> [Equation] -> A.Expr -> DesugarFn A.Expr
desugarMatchCons (u : us) qs def = do
  cs   <- getConstructors . getCon . head $ qs
  arms <- sequence [ desugarArm c (choose c) | c <- S.toList cs ]
  return $ A.Match (A.Id u) arms
 where
  getCon ((A.PatApp (A.PatId i : _)) : _, _) = i
  getCon _ = error "can't happen"
  choose c = [ q | q <- qs, getCon q == c ]
  desugarArm c qs' = do
    k    <- getArity c
    us'  <- replicateM k freshVar
    body <- desugarMatch
      (us' ++ us)
      [ (ps' ++ ps, e) | (A.PatApp ((A.PatId _) : ps') : ps, e) <- qs' ]
      def
    if k == 0
      then return (A.PatId c, body)
      else return (A.PatApp (A.PatId c : map A.PatId us'), body)
desugarMatchCons _ _ _ = error "can't happen"

{-
Simple pass for PatLit; no special desugaring
(Assuming that the patterns are exhaustive)
-}
desugarMatchLit :: [Identifier] -> [Equation] -> A.Expr -> DesugarFn A.Expr
desugarMatchLit (u : us) qs def = do
  arms <- sequence [ desugarArm p | (p : _, _) <- qs ]
  return $ A.Match (A.Id u) arms
 where
  desugarArm p = do
    body <- desugarMatch us [ (ps, e) | (_ : ps, e) <- qs ] def
    return (p, body)
desugarMatchLit _ _ _ = error "can't happen"

{-
Similar to PatApp, but with only one kind of constructor
-}
desugarMatchTup :: [Identifier] -> [Equation] -> A.Expr -> DesugarFn A.Expr
desugarMatchTup (u : us) qs@((A.PatTup rs : _, _) : _) def = do
  arm <- desugarArm
  return $ A.Match (A.Id u) [arm]
 where
  desugarArm = do
    let k = length rs
    us'  <- replicateM k freshVar
    body <- desugarMatch (us' ++ us)
                         [ (ps' ++ ps, e) | (A.PatTup ps' : ps, e) <- qs ]
                         def
    return (A.PatTup $ map A.PatId us', body)
desugarMatchTup _ _ _ = error "can't happen"

{-
To make life easier: transform PatWildcard into variable PatId
-}
desugarMatchWild :: [Equation] -> DesugarFn [Equation]
desugarMatchWild = mapM desugar
 where
  desugar (A.PatWildcard : rs, v) = do
    i <- freshVar
    return (A.PatId i : rs, v)
  desugar eq = return eq

{-
To make life easier: transform constructor PatId into PatApp [PatId]
-}
desugarMatchIdCons :: [Equation] -> DesugarFn [Equation]
desugarMatchIdCons []       = return []
desugarMatchIdCons (x : xs) = do
  xs' <- desugarMatchIdCons xs
  let x' = desugar x
  return (x' : xs')
 where
  desugar eq@(A.PatId i : rs, v) =
    if isCons i then (A.PatApp [A.PatId i] : rs, v) else eq
  desugar eq = eq


{-
Desugar PatAs and PatAnn into its actual pattern,
and propagate the 'as' or 'ann' part using let-bindings in the body
-}
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
    (A.PatId  _, A.PatId _ ) -> True
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
