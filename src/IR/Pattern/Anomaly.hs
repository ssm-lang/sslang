{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module IR.Pattern.Anomaly where

import Common.Compiler (
  Error (..),
  ErrorMsg,
  MonadError (..),
  MonadWriter,
  Pass (..),
  Warning (..),
  fromString,
 )
import Common.Identifiers (
  DConId (..),
  Identifiable (..),
  Identifier (..),
  TConId (..),
  isCons,
 )
import Control.Monad (
  unless,
  when,
 )
import Control.Monad.Reader (
  MonadReader (..),
  ReaderT (..),
  asks,
 )
import Data.List (
  find,
 )
import qualified Data.Map as M
import qualified Data.Set as S

import qualified IR.IR as I
import IR.Pattern.Common (
  CInfo (..),
  TInfo (..),
  buildConsMap,
  buildTypeMap,
 )
import qualified IR.Pattern.Matrix as PM
import qualified IR.Pattern.Vector as PV


data AnomalyCtx = AnomalyCtx
  { typeMap :: M.Map Identifier TInfo
  , consMap :: M.Map Identifier CInfo
  }


newtype AnomalyFn a = AnomalyFn (ReaderT AnomalyCtx Pass a)
  deriving (Functor) via (ReaderT AnomalyCtx Pass)
  deriving (Applicative) via (ReaderT AnomalyCtx Pass)
  deriving (Monad) via (ReaderT AnomalyCtx Pass)
  deriving (MonadFail) via (ReaderT AnomalyCtx Pass)
  deriving (MonadError Error) via (ReaderT AnomalyCtx Pass)
  deriving (MonadWriter [Warning]) via (ReaderT AnomalyCtx Pass)
  deriving (MonadReader AnomalyCtx) via (ReaderT AnomalyCtx Pass)


buildCtx :: [(TConId, I.TypeDef)] -> AnomalyCtx
buildCtx tds =
  AnomalyCtx{typeMap = buildTypeMap tds, consMap = buildConsMap tds}


-- | Run a AnomalyFn computation.
runAnomalyFn :: AnomalyFn a -> AnomalyCtx -> Pass a
runAnomalyFn (AnomalyFn m) = runReaderT m


checkProgram :: I.Program t -> Pass ()
checkProgram topdefs = runAnomalyFn (checkDefs ds) ctx
 where
  tds = I.typeDefs topdefs
  ds = I.programDefs topdefs
  ctx = buildCtx tds


checkDefs :: [(I.VarId, I.Expr t)] -> AnomalyFn ()
checkDefs = mapM_ (\(_, e) -> checkExpr e)


checkExprs :: [I.Expr t] -> AnomalyFn ()
checkExprs = mapM_ checkExpr


checkExpr :: I.Expr t -> AnomalyFn ()
checkExpr (I.Var _ _) = return ()
checkExpr (I.Data _ _) = return ()
checkExpr (I.Lit _ _) = return ()
checkExpr (I.App e1 e2 _) = checkExprs [e1, e2]
checkExpr (I.Let bindings e _) =
  let (_, es) = unzip bindings in checkExprs es >> checkExpr e
checkExpr (I.Lambda _ e _) = checkExpr e
checkExpr (I.Match e arms _) =
  let (ps, es) = unzip arms in checkExpr e >> checkExprs es >> checkPats ps
checkExpr (I.Prim _ es _) = checkExprs es
checkExpr (I.Exception _ _) = return ()


checkPats :: [I.Alt] -> AnomalyFn ()
checkPats ps = do
  checkUseless ps
  checkExhaustive ps


checkUseless :: [I.Alt] -> AnomalyFn ()
checkUseless ps = mapM_ (\(pm, p) -> checkUselessEach pm p) cases
 where
  checkUselessEach pm p = do
    u <- useful pm p
    unless
      u
      ( throwError $
          PatternError $
            "Pattern is useless"
              <> (fromString . show $ p)
      )
  cases =
    let vecs = map (\pat -> PV.fromList [pat]) ps
        rows = map (\vec -> PM.fromPatVec vec) vecs
        emptyMat = let r : _ = rows in PM.emptyWithCols $ PM.ncol r
        patMats =
          let accumRows = (\(ms, m) r -> (m : ms, PM.extend m r))
              (pms, _) = foldl accumRows ([], emptyMat) rows
           in reverse pms
     in zip patMats vecs


checkExhaustive :: [I.Alt] -> AnomalyFn ()
checkExhaustive ps = do
  let pm = PM.singleCol ps
  u <- useful pm (PV.singleton $ I.AltBinder Nothing)
  when
    u
    ( throwError $
        PatternError $
          "Patterns are not exhaustive"
            <> (fromString . show $ ps)
    )


useful :: PM.PatMat -> PV.PatVec -> AnomalyFn Bool
useful pm pv = do
  case compare (PM.ncol pm) 0 of
    LT -> throwMalformedError
    EQ -> baseCase
    GT -> usefulInductive pm pv
 where
  baseCase = return $ PM.nrow pm == 0


usefulInductive :: PM.PatMat -> PV.PatVec -> AnomalyFn Bool
usefulInductive pm pv =
  let wildCase = case samplePat pm of
        Nothing -> useful (PM.defaultize pm) (PV.tl pv)
        Just sample ->
          let wildCaseCons cid = do
                c <- askCInfo cid
                t <- askTInfo (cType c)
                if hasCompleteCons (tCSet t) pm
                  then
                    let f ck = do
                          ck' <- askCInfo ck
                          let arity = cArity ck'
                          useful
                            (PM.specializeCons arity ck pm)
                            (PV.specializeWild arity pv)
                     in fmap or . mapM f $ S.toList (tCSet t)
                  else useful (PM.defaultize pm) (PV.tl pv)
           in case sample of
                I.AltLit _ -> useful (PM.defaultize pm) (PV.tl pv)
                I.AltData (I.DConId i) _ -> wildCaseCons i
                I.AltBinder Nothing -> error "can't happen"
                I.AltBinder (Just (I.VarId i)) -> wildCaseCons i
      consCase cid = do
        c <- askCInfo cid
        useful (PM.specializeCons (cArity c) cid pm) (PV.specialize pv)
   in case PV.hd pv of
        I.AltBinder Nothing -> wildCase
        I.AltBinder _ -> wildCase
        I.AltData (I.DConId i) _ -> consCase i
        I.AltLit lit -> useful (PM.specializeLit lit pm) (PV.specialize pv)


samplePat :: PM.PatMat -> Maybe I.Alt
samplePat pm = case firstConsPatVec of
  Nothing -> Nothing
  Just pv -> stripPat (PV.hd pv)
 where
  firstConsPatVec = find (isConstructor . PV.hd) (PM.toList pm)
   where
    isConstructor p = case p of
      I.AltLit _ -> True
      I.AltData (I.DConId i) _ -> isCons i
      I.AltBinder Nothing -> False
      I.AltBinder (Just (I.VarId i)) -> isCons i
  stripPat p = case p of
    I.AltLit _ -> Just p
    I.AltData _ _ -> Just p
    I.AltBinder Nothing -> error "can't happen"
    I.AltBinder _ -> Just p


askCInfo :: Identifier -> AnomalyFn CInfo
askCInfo i = do
  cs <- asks $ M.lookup i . consMap
  case cs of
    Nothing ->
      throwError $
        PatternError $
          "askArity: Data constructor does not exist: "
            <> showId i
    Just cs' -> return cs'


askTInfo :: Identifier -> AnomalyFn TInfo
askTInfo i = do
  ts <- asks $ M.lookup i . typeMap
  case ts of
    Nothing ->
      throwError $
        PatternError $
          "askArity: Data constructor does not exist: "
            <> showId i
    Just ts' -> return ts'


hasCompleteCons :: S.Set Identifier -> PM.PatMat -> Bool
hasCompleteCons cset pm = S.empty == foldr removeC cset (PM.toList pm)
 where
  removeC pv cset' = removeC' (PV.hd pv) cset'
  removeC' p cset' = case p of
    I.AltData (DConId i) _ -> S.delete i cset'
    I.AltBinder b ->
      case b of
        Just (I.VarId i) -> S.delete i cset'
        _ -> cset'
    _ -> cset'


-- | Report 'Identifier' for error reporting.
showId :: Identifier -> ErrorMsg
showId s = "'" <> fromString (ident s) <> "'"


showSet :: S.Set Identifier -> ErrorMsg
showSet set = "'" <> fromString (show set) <> "'"


throwMalformedError :: AnomalyFn a
throwMalformedError = throwError $ PatternError "Pattern is not well-formed."
