{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pattern match checker
module Front.Match where

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
                                                )
import           Control.Monad                  ( guard
                                                , unless
                                                )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                , asks
                                                )
import qualified Data.Map                      as M
import           Data.Maybe                     ( mapMaybe )
import qualified Data.Set                      as S
import qualified Front.Ast                     as A

-- | Report 'Identifier' for error reporting.
showId :: Identifier -> ErrorMsg
showId s = "'" <> fromString (ident s) <> "'"

showSet :: S.Set Identifier -> ErrorMsg
showSet set = "'" <> fromString (show set) <> "'"

data PV = PV
  { getPVN :: Int
  , -- number of elements
    getPVL :: [A.Pat] -- list of patterns
  }
  deriving (Eq, Show)

data PM = PM
  { getPMM :: Int
  , -- number of rows
    getPMN :: Int
  , -- number of columns
    getPML :: [PV] -- list of PV
  }
  deriving (Eq, Show)

data MatchC = MatchC
  { cName  :: Identifier
  , cType  :: Identifier
  , cArity :: Int
  }

data MatchT = MatchT
  { tName :: Identifier
  , tCSet :: S.Set Identifier
  }

data MatchCtx = MatchCtx
  { typeMap :: M.Map Identifier MatchT
  , consMap :: M.Map Identifier MatchC
  }

newtype MatchFn a = MatchFn (ReaderT MatchCtx Pass a)
  deriving (Functor) via (ReaderT MatchCtx Pass)
  deriving (Applicative) via (ReaderT MatchCtx Pass)
  deriving (Monad) via (ReaderT MatchCtx Pass)
  deriving (MonadFail) via (ReaderT MatchCtx Pass)
  deriving (MonadError Error) via (ReaderT MatchCtx Pass)
  deriving (MonadWriter [Warning]) via (ReaderT MatchCtx Pass)
  deriving (MonadReader MatchCtx) via (ReaderT MatchCtx Pass)

-- | Run a MatchFn computation.
runMatchFn :: MatchFn a -> MatchCtx -> Pass a
runMatchFn (MatchFn m) = runReaderT m

-- | Add a list of data identifiers to the scope.
-- withTypeScope :: [(Identifier, TypInfo)] -> ScopeFn a -> ScopeFn a
-- withTypeScope is =
--   local $ \ctx -> ctx { typeMap = foldr (uncurry M.insert) (typeMap ctx) is }
checkMatches :: A.Program -> Pass ()
checkMatches (A.Program topdefs) = runMatchFn (checkDefs ds) ctx
 where
  tds = mapMaybe A.getTopTypeDef topdefs
  ds  = mapMaybe A.getTopDataDef topdefs
  ctx = buildCtx tds

buildCtx :: [A.TypeDef] -> MatchCtx
buildCtx tds = MatchCtx { typeMap = tmap, consMap = cmap }
 where
  tmap = foldr tAcc M.empty tds
  cmap = foldr cAcc M.empty tds
  tAcc td tmap' =
    let typ   = A.typeName td
        clist = map (\(A.VariantUnnamed cid _) -> cid) (A.typeVariants td)
        cset  = S.fromList clist
    in  M.insert typ (MatchT { tName = typ, tCSet = cset }) tmap'
  cAcc td cmap' =
    let typ   = A.typeName td
        clist = A.typeVariants td
        cAcc' (A.VariantUnnamed cid ts) cmap'' =
          let c = MatchC { cName = cid, cType = typ, cArity = length ts }
          in  M.insert cid c cmap''
    in  foldr cAcc' cmap' clist

checkDefs :: [A.Definition] -> MatchFn ()
checkDefs = mapM_ checkDef

-- WARN: only body is checked
checkDef :: A.Definition -> MatchFn ()
checkDef d = checkExpr $ takeBody d
 where
  takeBody (A.DefFn _ _ _ e) = e
  takeBody (A.DefPat _ e   ) = e

checkExprs :: [A.Expr] -> MatchFn ()
checkExprs = mapM_ checkExpr

checkExpr :: A.Expr -> MatchFn ()
checkExpr (A.Id  _               ) = return ()
checkExpr (A.Lit _               ) = return ()
checkExpr (A.Apply    e1 e2      ) = checkExprs [e1, e2]
checkExpr (A.Lambda   _  e       ) = checkExpr e -- WARN: patterns here are not checked
checkExpr (A.OpRegion e  opRegion) = checkExpr e >> checkOpRegion opRegion
checkExpr A.NoExpr                 = return ()
checkExpr (A.Let   ds e      )     = checkDefs ds >> checkExpr e
checkExpr (A.While e1 e2     )     = checkExprs [e1, e2]
checkExpr (A.Loop e          )     = checkExpr e
checkExpr (A.Par  es         )     = checkExprs es
checkExpr (A.IfElse e1 e2 e3 )     = checkExprs [e1, e2, e3]
checkExpr (A.After  e1 e2 e3 )     = checkExprs [e1, e2, e3]
checkExpr (A.Assign     e1 e2)     = checkExprs [e1, e2]
checkExpr (A.Constraint e  _ )     = checkExpr e
checkExpr (A.Wait es         )     = checkExprs es
checkExpr (A.Seq e1 e2       )     = checkExprs [e1, e2]
checkExpr A.Break                  = return ()
checkExpr (A.Match e arms) =
  let (ps, es) = unzip arms in checkExpr e >> checkExprs es >> checkPats ps
checkExpr (A.Return e) = checkExpr e

checkOpRegion :: A.OpRegion -> MatchFn ()
checkOpRegion (A.NextOp _ e opRegion) = checkExpr e >> checkOpRegion opRegion
checkOpRegion A.EOR                   = return ()

checkPats :: [A.Pat] -> MatchFn ()
checkPats ps =
  let pm = fromListPM ps
  in  do
        u <- useful pm (justPV A.PatWildcard)
        if u
          then throwError $ PatternError "Patterns are not exhaustive"
          else return ()

-- isExhaustive :: [A.Pat] -> MatchFn Bool
-- isExhaustive ps =
--   let pm = fromListPM ps in not $ useful pm (justPV A.PatWildcard)

useful :: PM -> PV -> MatchFn Bool
useful pm pv | getPMN pm < 0  = undefined
             | getPMN pm == 0 = baseCase
             | otherwise      = inductiveCase
 where
  baseCase = return $ getPMM pm == 0
  -- INFO: PatAs and PatAnn are trivially traversed
  inductiveCase :: MatchFn Bool
  inductiveCase =
    let wildcase = if getPMM pm == 0
          then do
            dpm <- defaultPM pm
            useful dpm (tailPV pv)
          else case cSet pm of -- BUG: this is wrong for PatLit, PatTup
            Nothing -> do
              dpm <- defaultPM pm
              useful dpm (tailPV pv)
            Just cset ->
              (do
                iscomplete <- askComplete cset
                let f ck = do
                      spm   <- specializedPMCons ck pm
                      arity <- askArity ck
                      spv   <- specializedPV arity pv
                      useful spm spv
                if iscomplete
                  then fmap or . mapM f $ S.toList cset
                  else do
                    dpm <- defaultPM pm
                    useful dpm (tailPV pv)
              )
    in
      case head (getPVL pv) of
        A.PatWildcard -> wildcase
        A.PatId i     -> if isCons i
          then do
            spm   <- specializedPMCons i pm
            arity <- askArity i
            spv   <- specializedPV arity pv
            useful spm spv
          else wildcase
        A.PatLit lit -> do
          spm <- specializedPMLit lit pm
          spv <- specializedPV 0 pv
          useful spm spv
        A.PatAs _ r -> useful pm (r `consP` tailPV pv)
        A.PatTup rs -> do
          let rlen = length rs
          spm <- specializedPMTup rlen pm
          spv <- specializedPV rlen pv
          useful spm spv
        A.PatApp ((A.PatId i) : _) -> if isCons i
          then do
            spm   <- specializedPMCons i pm
            arity <- askArity i
            spv   <- specializedPV arity pv
            useful spm spv
          else throwError
            $ PatternError "useful: can't have type variable start a PatApp"
        A.PatApp _   -> throwError $ PatternError "useful: malformed PatApp"
        A.PatAnn _ r -> useful pm (r `consP` tailPV pv)

cSet :: PM -> Maybe (S.Set Identifier)
cSet pm
  | getPMM pm <= 0 = Just S.empty
  | otherwise = do
    let h = headPM pm
        t = tailPM pm
    cset'      <- cSet t
    (valid, c) <- getc h
    guard valid
    return $ S.insert c cset'
 where
  getc :: PV -> Maybe (Bool, Identifier)
  getc pv
    | getPVN pv == 0 = Nothing
    | otherwise = case headPV pv of
      A.PatWildcard    -> Just (False, "")
      A.PatId  i       -> if isCons i then Just (True, i) else Just (False, "")
      A.PatLit _       -> Nothing
      A.PatAs _ r      -> getc $ fromListPV [r]
      A.PatTup _       -> Nothing
      A.PatApp []      -> error "can't have empty PatApp"
      A.PatApp (r : _) -> case r of
        A.PatId i -> if isCons i
          then Just (True, i)
          else error "can't have variable be the first in PatApp"
        _ ->
          error "can't have patterns other than PatId be the first in PatApp"
      A.PatAnn _ r -> getc $ fromListPV [r]




{- specialize:
  1. Cons
  2. Lit: for all kinds of lit
  3. Tup
-}
specializedPV :: Int -> PV -> MatchFn PV
specializedPV arity pv =
  let width = arity + getPVN pv - 1
  in  case head (getPVL pv) of
        A.PatWildcard ->
          let sPats = replicate arity A.PatWildcard ++ tail (getPVL pv)
          in  return $ PV width sPats
        A.PatId i -> if isCons i
          then return $ PV width $ tail (getPVL pv)
          else
            let sPats = replicate arity A.PatWildcard ++ tail (getPVL pv)
            in  return $ PV width sPats
        A.PatLit _ -> return $ PV width $ tail (getPVL pv)
        A.PatAs _ _ ->
          throwError $ PatternError "specializedPV: can't specialize PatAs"
        A.PatTup ps -> return $ PV width $ ps ++ tail (getPVL pv)
        A.PatApp ps -> return $ PV width $ tail ps ++ tail (getPVL pv)
        A.PatAnn _ _ ->
          throwError $ PatternError "specializedPV: can't specialize PatAnn"

specializedPMTup :: Int -> PM -> MatchFn PM
specializedPMTup nElts pm = do
  let width = nElts + getPMN pm - 1
      specializedRowAcc :: PV -> PM -> MatchFn PM
      specializedRowAcc pv pmAcc = case head (getPVL pv) of
        A.PatId c' -> if isCons c'
          then return pmAcc
          else do
            sVec <- specializedPV nElts pv
            return $ sVec `consPV` pmAcc

        A.PatWildcard -> do
          sVec <- specializedPV nElts pv
          return $ sVec `consPV` pmAcc
        A.PatAs _ r -> do
          let n   = getPVN pv
              row = PV n (r : tail (getPVL pv))
          rows <- specializedPMTup nElts (row `consPV` emptyPM n)
          return $ rows `appendPM` pmAcc
        A.PatAnn _ r -> do
          let n   = getPVN pv
              row = PV n (r : tail (getPVL pv))
          rows <- specializedPMTup nElts (row `consPV` emptyPM n)
          return $ rows `appendPM` pmAcc
        A.PatApp _    -> return pmAcc
        A.PatLit _    -> return pmAcc
        A.PatTup elts -> if length elts == nElts
          then
            (let sVec = PV width (elts ++ tail (getPVL pv))
             in  return $ sVec `consPV` pmAcc
            )
          else throwError
            $ PatternError "specializedPMTup: tuple size is not the same"


  foldrM specializedRowAcc (emptyPM width) (getPML pm)

specializedPMLit :: A.Literal -> PM -> MatchFn PM
specializedPMLit lit pm = do
  let width = getPMN pm - 1
      specializedRowAcc :: PV -> PM -> MatchFn PM
      specializedRowAcc pv pmAcc = case head (getPVL pv) of
        A.PatLit lit' -> if lit' == lit
          then
            (let sVec = PV width (tail (getPVL pv))
             in  return $ sVec `consPV` pmAcc
            )
          else return pmAcc

        A.PatId c' -> if isCons c'
          then return pmAcc
          else do
            sVec <- specializedPV 0 pv
            return $ sVec `consPV` pmAcc

        A.PatWildcard -> do
          sVec <- specializedPV 0 pv
          return $ sVec `consPV` pmAcc
        A.PatAs _ r -> do
          let n   = getPVN pv
              row = PV n (r : tail (getPVL pv))
          rows <- specializedPMLit lit (row `consPV` emptyPM n)
          return $ rows `appendPM` pmAcc
        A.PatAnn _ r -> do
          let n   = getPVN pv
              row = PV n (r : tail (getPVL pv))
          rows <- specializedPMLit lit (row `consPV` emptyPM n)
          return $ rows `appendPM` pmAcc
        A.PatApp _ -> return pmAcc
        A.PatTup _ -> return pmAcc


  foldrM specializedRowAcc (emptyPM width) (getPML pm)

specializedPMCons :: Identifier -> PM -> MatchFn PM
specializedPMCons c pm = do
  arity <- askArity c
  let
    width = arity + getPMN pm - 1
    specializedRowAcc :: PV -> PM -> MatchFn PM
    specializedRowAcc pv pmAcc = case head (getPVL pv) of
      A.PatId c' -> if isCons c'
        then
          (if c' == c
            then
              (let sVec = PV width (tail (getPVL pv))
               in  return $ sVec `consPV` pmAcc
              )
            else return pmAcc
          )
        else do
          sVec <- specializedPV arity pv
          return $ sVec `consPV` pmAcc

      A.PatWildcard -> do
        sVec <- specializedPV arity pv
        return $ sVec `consPV` pmAcc
      A.PatAs _ r -> do
        let n   = getPVN pv
            row = PV n (r : tail (getPVL pv))
        rows <- specializedPMCons c (row `consPV` emptyPM n)
        return $ rows `appendPM` pmAcc
      A.PatAnn _ r -> do
        let n   = getPVN pv
            row = PV n (r : tail (getPVL pv))
        rows <- specializedPMCons c (row `consPV` emptyPM n)
        return $ rows `appendPM` pmAcc
      A.PatApp [] ->
        throwError $ PatternError "specializedPMCons: PatApp is empty"
      A.PatApp (p : rs) -> case p of
        A.PatId c' -> if isCons c'
          then
            (if c' == c
              then
                (let sVec = PV width (rs ++ tail (getPVL pv))
                 in  return $ sVec `consPV` pmAcc
                )
              else return pmAcc
            )
          else undefined
        _ -> throwError
          $ PatternError "specializedPMCons: PatApp must start with a PatId"
      A.PatLit _ -> return pmAcc
      A.PatTup _ -> return pmAcc
  foldrM specializedRowAcc (emptyPM width) (getPML pm)

defaultPM :: PM -> MatchFn PM
defaultPM pm =
  let width = getPMN pm - 1
      defaultRowAcc :: PV -> PM -> MatchFn PM
      defaultRowAcc pv pmAcc = case head (getPVL pv) of
        A.PatWildcard ->
          let ps = tail $ getPVL pv in return $ PV width ps `consPV` pmAcc
        A.PatId c -> if isCons c
          then return pmAcc
          else let ps = tail $ getPVL pv in return $ PV width ps `consPV` pmAcc
        A.PatLit _  -> return pmAcc
        A.PatAs _ r -> do
          let n   = getPVN pv
              row = PV n (r : tail (getPVL pv))
          rows <- defaultPM (row `consPV` emptyPM n)
          return $ rows `appendPM` pmAcc
        A.PatTup _   -> return pmAcc
        A.PatApp _   -> return pmAcc
        A.PatAnn _ r -> do
          let n   = getPVN pv
              row = PV n (r : tail (getPVL pv))
          rows <- defaultPM (row `consPV` emptyPM n)
          return $ rows `appendPM` pmAcc
  in  foldrM defaultRowAcc (emptyPM width) (getPML pm)

askArity :: Identifier -> MatchFn Int
askArity i = do
  c <- asks $ M.lookup i . consMap
  case c of
    Nothing ->
      throwError
        $  PatternError
        $  "askArity: Data constructor does not exist: "
        <> showId i
    Just c' -> return $ cArity c'


askComplete :: S.Set Identifier -> MatchFn Bool
askComplete cset
  | S.size cset == 0 = return False
  | otherwise = do
    let cname = S.elemAt 0 cset
    c <- asks $ M.lookup cname . consMap
    case c of
      Nothing ->
        throwError
          $  PatternError
          $  "askComplete: Data constructor does not exist: "
          <> showId cname
      Just c' -> do
        let tname = cType c'
        t <- asks $ M.lookup tname . typeMap
        case t of
          Nothing ->
            throwError
              $  PatternError
              $  "Type ("
              <> showId tname
              <> ") does not exist for data constructor: "
              <> showId cname
          Just t' -> do
            let tcset = tCSet t'
            Control.Monad.unless (cset `S.isSubsetOf` tcset)
              $  throwError
              $  PatternError
              $  "Data constructors are not of the same type: "
              <> showSet cset
            return $ not (cset `S.isProperSubsetOf` tcset)

consPV :: PV -> PM -> PM
consPV pVec pm = PM (1 + getPMM pm) (getPMN pm) (pVec : getPML pm)

emptyPM :: Int -> PM
emptyPM n = PM 0 n []

appendPM :: PM -> PM -> PM
appendPM pm1 pm2 =
  let m' = getPMM pm1 + getPMM pm2
      l' = getPML pm1 ++ getPML pm2
  in  PM m' (getPMN pm1) l'

consP :: A.Pat -> PV -> PV
consP p pv = PV (1 + getPVN pv) (p : getPVL pv)

headPV :: PV -> A.Pat
headPV = head . getPVL

tailPV :: PV -> PV
tailPV pv = PV (getPVN pv - 1) (tail (getPVL pv))

headPM :: PM -> PV
headPM = head . getPML

tailPM :: PM -> PM
tailPM pm = PM (getPMM pm - 1) (getPMN pm) (tail (getPML pm))

fromListPV :: [A.Pat] -> PV
fromListPV ps = let n = length ps in PV { getPVN = n, getPVL = ps }

fromListPM :: [A.Pat] -> PM
fromListPM ps =
  let m   = length ps
      pvs = map (\p -> PV { getPVN = 1, getPVL = [p] }) ps
  in  PM { getPMM = m, getPMN = 1, getPML = pvs }

justPV :: A.Pat -> PV
justPV p = PV { getPVN = 1, getPVL = [p] }

foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM _ d []       = return d
foldrM f d (x : xs) = f x =<< foldrM f d xs

