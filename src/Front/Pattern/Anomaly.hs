{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Front.Pattern.Anomaly where

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
  Identifiable (..),
  Identifier (..),
  isCons,
  isVar,
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
  findIndex,
 )
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Front.Ast as A
import Front.Pattern.Common (
  CInfo (..),
  TInfo (..),
  buildConsMap,
  buildTypeMap,
 )
import qualified Front.Pattern.Matrix as PM
import qualified Front.Pattern.Vector as PV


{-
Assumptions by `useful`:
- patterns are well-typed

Assumptions checked by `useful`:
- root constructors are of the same type
- root constructors have the correct number of arguments

This weak assumption check has the following implications:
- some mal-typed patterns may pass this anomaly check

This is considered okay because:
- Pattern desugar does not alter semantics of pattern matches, even mal-typed ones
- Mal-typed patterns will be type-checked later in the IR type-inference pass
-}

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


buildCtx :: [A.TypeDef] -> AnomalyCtx
buildCtx tds =
  AnomalyCtx{typeMap = buildTypeMap tds, consMap = buildConsMap tds}


-- | Run a AnomalyFn computation.
runAnomalyFn :: AnomalyFn a -> AnomalyCtx -> Pass a
runAnomalyFn (AnomalyFn m) = runReaderT m


checkProgram :: A.Program -> Pass ()
checkProgram (A.Program topdefs) = runAnomalyFn (checkDefs ds) ctx
 where
  tds = mapMaybe A.getTopTypeDef topdefs
  ds = mapMaybe A.getTopDataDef topdefs
  ctx = buildCtx tds


checkDefs :: [A.Definition] -> AnomalyFn ()
checkDefs = mapM_ checkDef


-- WARN: only body is checked
checkDef :: A.Definition -> AnomalyFn ()
checkDef d = checkExpr $ takeBody d
 where
  takeBody (A.DefFn _ _ _ e) = e
  takeBody (A.DefPat _ e) = e


checkExprs :: [A.Expr] -> AnomalyFn ()
checkExprs = mapM_ checkExpr


checkExpr :: A.Expr -> AnomalyFn ()
checkExpr (A.Id _) = return ()
checkExpr (A.Lit _) = return ()
checkExpr (A.Apply e1 e2) = checkExprs [e1, e2]
checkExpr (A.Lambda _ e) = checkExpr e -- WARN: patterns here are not checked
checkExpr (A.OpRegion e opRegion) = checkExpr e >> checkOpRegion opRegion
checkExpr A.NoExpr = return ()
checkExpr (A.Let ds e) = checkDefs ds >> checkExpr e
checkExpr (A.While e1 e2) = checkExprs [e1, e2]
checkExpr (A.Loop e) = checkExpr e
checkExpr (A.Par es) = checkExprs es
checkExpr (A.IfElse e1 e2 e3) = checkExprs [e1, e2, e3]
checkExpr (A.After e1 e2 e3) = checkExprs [e1, e2, e3]
checkExpr (A.Assign e1 e2) = checkExprs [e1, e2]
checkExpr (A.Constraint e _) = checkExpr e
checkExpr (A.Wait es) = checkExprs es
checkExpr (A.Seq e1 e2) = checkExprs [e1, e2]
checkExpr A.Break = return ()
checkExpr (A.Match e arms) =
  let (ps, es) = unzip arms in checkExpr e >> checkExprs es >> checkPats ps
checkExpr (A.CQuote _) = return ()
checkExpr (A.CCall _ es) = mapM_ checkExpr es
checkExpr (A.ListExpr es) = mapM_ checkExpr es
checkExpr (A.Tuple es) = checkExprs es


checkOpRegion :: A.OpRegion -> AnomalyFn ()
checkOpRegion (A.NextOp _ e opRegion) = checkExpr e >> checkOpRegion opRegion
checkOpRegion A.EOR = return ()


checkPats :: [A.Pat] -> AnomalyFn ()
checkPats ps = do
  checkUselessArm ps
  checkExhaustive ps


checkUselessArm :: [A.Pat] -> AnomalyFn ()
checkUselessArm ps = do
  let n = length ps
      pms =
        [(ntrial, PM.singleCol $ take ntrial ps) | ntrial <- [0 .. (n - 1)]]
      trials = [useful (PV.singleton $ ps !! ntrial) pm | (ntrial, pm) <- pms]
  results <- sequence trials
  case findIndex not results of
    Nothing -> return ()
    Just ind ->
      throwError $
        PatternError $
          "Useless pattern: "
            <> (fromString . show $ ps !! ind)


checkExhaustive :: [A.Pat] -> AnomalyFn ()
checkExhaustive ps = do
  let pm = PM.singleCol ps
  u <- useful (PV.singleton A.PatWildcard) pm
  when
    u
    ( throwError $
        PatternError $
          "Patterns are not exhaustive"
            <> (fromString . show $ ps)
    )


useful :: PV.PatVec -> PM.PatMat -> AnomalyFn Bool
useful pv pm = do
  wellFormed pv pm -- check well-form
  case compare (PM.ncol pm) 0 of
    LT -> throwMalformedError
    EQ -> baseCase
    GT -> inductiveCase
 where
  baseCase = return $ PM.nrow pm == 0
  inductiveCase =
    let wildCase = case samplePat pm of
          Nothing -> useful (PV.tl pv) (PM.defaultize pm) -- INFO: assume that there is no uninhabited types
          Just sample ->
            let consCase i = do
                  c <- askCInfo i
                  t <- askTInfo (cType c)
                  if hasCompleteCons (tCSet t) pm
                    then
                      let f ck = do
                            ck' <- askCInfo ck
                            let arity = cArity ck'
                            useful
                              (PV.specializeWild arity pv)
                              (PM.specializeCons arity ck pm)
                       in fmap or . mapM f $ S.toList (tCSet t)
                    else useful (PV.tl pv) (PM.defaultize pm)
             in case sample of
                  A.PatLit lit ->
                    let typ = litToType lit
                     in if hasCompleteLit typ pm
                          then usefulWildLit typ pv pm
                          else useful (PV.tl pv) (PM.defaultize pm)
                  A.PatTup ps ->
                    useful
                      (PV.specializeWild (length ps) pv)
                      (PM.specializeTup (length ps) pm)
                  A.PatId i -> consCase i
                  A.PatApp ((A.PatId i) : _) -> consCase i
                  A.PatApp _ -> error "can't happen"
                  A.PatAnn _ _ -> error "can't happen"
                  A.PatAs _ _ -> error "can't happen"
                  A.PatWildcard -> error "can't happen"
     in case PV.hd pv of
          A.PatWildcard -> wildCase
          A.PatId i ->
            if isVar i
              then wildCase
              else do
                c <- askCInfo i
                useful (PV.specialize pv) (PM.specializeCons (cArity c) i pm)
          A.PatLit lit -> useful (PV.specialize pv) (PM.specializeLit lit pm)
          A.PatTup ps ->
            useful (PV.specialize pv) (PM.specializeTup (length ps) pm)
          A.PatAs _ p -> useful (p `PV.append` PV.tl pv) pm
          A.PatAnn _ p -> useful (p `PV.append` PV.tl pv) pm
          A.PatApp ((A.PatId i) : ps) ->
            useful (PV.specialize pv) (PM.specializeCons (length ps) i pm)
          A.PatApp _ -> error "can't happen"


usefulWildLit :: LitType -> PV.PatVec -> PM.PatMat -> AnomalyFn Bool
usefulWildLit LitCharT _ = error "char literals are not implemented yet" -- TODO: can only do this after char literals are implemented
usefulWildLit _ _ = error "can't happen"


samplePat :: PM.PatMat -> Maybe A.Pat
samplePat pm = case firstConsPatVec of
  Nothing -> Nothing
  Just pv -> stripPat (PV.hd pv)
 where
  firstConsPatVec = find (isConstructor . PV.hd) (PM.toList pm)
   where
    isConstructor p = case p of
      A.PatApp _ -> True
      A.PatTup _ -> True
      A.PatId i -> isCons i
      A.PatLit _ -> True
      A.PatAs _ p' -> isConstructor p'
      A.PatAnn _ p' -> isConstructor p'
      A.PatWildcard -> False
  stripPat p = case p of
    A.PatId _ -> Just p
    A.PatTup _ -> Just p
    A.PatApp _ -> Just p
    A.PatLit _ -> Just p
    A.PatAs _ p' -> stripPat p'
    A.PatAnn _ p' -> stripPat p'
    A.PatWildcard -> error "can't happen"


hasCompleteCons :: S.Set Identifier -> PM.PatMat -> Bool
hasCompleteCons cset pm = S.empty == foldr removeC cset (PM.toList pm)
 where
  removeC pv cset' = removeC' (PV.hd pv) cset'
  removeC' p cset' = case p of
    A.PatId i -> S.delete i cset'
    A.PatApp ((A.PatId i) : _) -> S.delete i cset'
    A.PatAs _ p' -> removeC' p' cset'
    A.PatAnn _ p' -> removeC' p' cset'
    _ -> cset'


hasCompleteLit :: LitType -> PM.PatMat -> Bool
hasCompleteLit LitCharT _ = error "char literals are not implemented yet" -- TODO: can only do this after char literals are implemented
hasCompleteLit LitIntT _ = False
hasCompleteLit LitRatT _ = False
hasCompleteLit LitStringT _ = False
hasCompleteLit LitEventT _ = False -- WARN: how does LitEvent work in pattern matching?


{-
Checks:
- pm and pv have same number of columns
- pm has at non-negative ncol and nrow
- root constructors are of the same type
- root constructors have the correct number of arguments
-}
wellFormed :: PV.PatVec -> PM.PatMat -> AnomalyFn ()
wellFormed pv pm
  | PM.ncol pm == PV.ncol pv = case compare (PM.ncol pm) 0 of
    LT -> throwMalformedError
    EQ -> return ()
    GT -> case compare (PM.nrow pm) 0 of
      LT -> throwMalformedError
      EQ -> return ()
      GT -> wellFormedPat (PV.hd pv) pm
  | otherwise = throwMalformedError


wellFormedPat :: A.Pat -> PM.PatMat -> AnomalyFn ()
wellFormedPat p pm = case p of
  A.PatWildcard -> wellFormedWild pm
  A.PatId i ->
    if isVar i
      then wellFormedWild pm
      else do
        c <- askCInfo i
        unless (cArity c == 0) throwMalformedError
        t <- askTInfo $ cType c
        wellFormedCons t pm
  A.PatLit lit -> wellFormedLit lit pm
  A.PatAs _ p' -> wellFormedPat p' pm
  A.PatTup ps -> wellFormedTup (length ps) pm
  A.PatApp ((A.PatId i) : ps) -> do
    unless (isCons i) throwMalformedError
    c <- askCInfo i
    unless (cArity c == length ps) throwMalformedError
    t <- askTInfo $ cType c
    wellFormedCons t pm
  A.PatApp _ -> throwMalformedError
  A.PatAnn _ p' -> wellFormedPat p' pm


wellFormedCons :: TInfo -> PM.PatMat -> AnomalyFn ()
wellFormedCons t pm
  | PM.nrow pm < 0 =
    throwMalformedError
  | PM.nrow pm == 0 =
    return ()
  | otherwise =
    let pv' = PM.hd pm
        check target = case target of
          A.PatAs _ p -> check p
          A.PatAnn _ p -> check p
          A.PatApp ((A.PatId i') : ps) -> do
            unless (isCons i') throwMalformedError
            c' <- askCInfo i'
            unless (cArity c' == length ps) throwMalformedError
            unless (cType c' == tName t) throwMalformedError
          A.PatApp _ -> throwMalformedError
          A.PatLit _ -> throwMalformedError
          A.PatTup _ -> throwMalformedError
          A.PatWildcard -> return ()
          A.PatId i' -> do
            unless (isCons i') throwMalformedError
            c' <- askCInfo i'
            unless (cArity c' == 0) throwMalformedError
            unless (cType c' == tName t) throwMalformedError
     in do
          -- check that pm has constructors from the specific type
          check (PV.hd pv')
          wellFormedCons t (PM.tl pm)


wellFormedTup :: Int -> PM.PatMat -> AnomalyFn ()
wellFormedTup n pm
  | PM.nrow pm < 0 =
    throwMalformedError
  | PM.nrow pm == 0 =
    return ()
  | otherwise =
    let pv' = PM.hd pm
        check target = case target of
          A.PatAs _ p -> check p
          A.PatAnn _ p -> check p
          A.PatApp _ -> throwMalformedError
          A.PatLit _ -> throwMalformedError
          A.PatWildcard -> return ()
          A.PatId i -> unless (isVar i) throwMalformedError
          A.PatTup ps -> unless (length ps == n) throwMalformedError
     in do
          check (PV.hd pv')
          wellFormedTup n (PM.tl pm)


wellFormedLit :: A.Literal -> PM.PatMat -> AnomalyFn ()
wellFormedLit lit pm
  | PM.nrow pm < 0 =
    throwMalformedError
  | PM.nrow pm == 0 =
    return ()
  | otherwise =
    let pv' = PM.hd pm
        check target = case target of
          A.PatAs _ p -> check p
          A.PatAnn _ p -> check p
          A.PatTup _ -> throwMalformedError
          A.PatApp _ -> throwMalformedError
          A.PatWildcard -> return ()
          A.PatId i -> unless (isVar i) throwMalformedError
          A.PatLit lit' ->
            unless (litToType lit == litToType lit') throwMalformedError
     in do
          check (PV.hd pv')
          wellFormedLit lit (PM.tl pm)


wellFormedWild :: PM.PatMat -> AnomalyFn ()
wellFormedWild pm
  | PM.nrow pm < 0 = throwMalformedError
  | PM.nrow pm == 0 = return ()
  | otherwise = let pv = PM.hd pm in wellFormed pv (PM.tl pm)


data LitType
  = LitIntT
  | LitStringT
  | LitRatT
  | LitCharT
  | LitEventT
  deriving (Eq, Show)


litToType :: A.Literal -> LitType
litToType (A.LitInt _) = LitIntT
litToType (A.LitString _) = LitStringT
litToType (A.LitRat _) = LitRatT
litToType (A.LitChar _) = LitCharT
litToType A.LitEvent = LitEventT


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


-- | Report 'Identifier' for error reporting.
showId :: Identifier -> ErrorMsg
showId s = "'" <> fromString (ident s) <> "'"


showSet :: S.Set Identifier -> ErrorMsg
showSet set = "'" <> fromString (show set) <> "'"


throwMalformedError :: AnomalyFn a
throwMalformedError = throwError $ PatternError "Pattern is not well-formed."
