{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Pattern match checker
module Front.Match where

import           Common.Compiler                          ( Error(..)
                                                          , ErrorMsg
                                                          , MonadError(..)
                                                          , MonadWriter
                                                          , Pass(..)
                                                          , Warning(..)
                                                          , fromString
                                                          , warn
                                                          )
import           Common.Identifiers                       ( Identifiable(..)
                                                          , Identifier(..)
                                                          )
import           Control.Monad                            ( unless
                                                          , when
                                                          )
import           Control.Monad.Reader                     ( MonadReader(..)
                                                          , ReaderT(..)
                                                          , asks
                                                          )
import qualified Data.Map                      as M
import           Data.Maybe                               ( isJust
                                                          , isNothing
                                                          , mapMaybe
                                                          )
import qualified Data.Set                      as S
import qualified Front.Ast                     as A


-- | Report 'Identifier' for error reporting.
showId :: Identifier -> ErrorMsg
showId s = "'" <> fromString (ident s) <> "'"

showSet :: S.Set Identifier -> ErrorMsg
showSet set = "'" <> fromString (show set) <> "'"

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
  deriving Functor                      via (ReaderT MatchCtx Pass)
  deriving Applicative                  via (ReaderT MatchCtx Pass)
  deriving Monad                        via (ReaderT MatchCtx Pass)
  deriving MonadFail                    via (ReaderT MatchCtx Pass)
  deriving (MonadError Error)           via (ReaderT MatchCtx Pass)
  deriving (MonadWriter [Warning])      via (ReaderT MatchCtx Pass)
  deriving (MonadReader MatchCtx)       via (ReaderT MatchCtx Pass)

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
checkExpr (A.Lambda   ps e       ) = checkExpr e >> checkPats ps
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
checkPats ps = return () -- TODO

isCompleteCSet :: S.Set Identifier -> MatchFn Bool
isCompleteCSet cset
  | S.size cset == 0 = return False
  | otherwise = do
    let cname = S.elemAt 1 cset
    c <- asks $ M.lookup cname . consMap
    case c of
      Nothing ->
        throwError
          $  PatternError
          $  "Data constructor does not exist: "
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
            unless (cset `S.isSubsetOf` tcset)
              $  throwError
              $  PatternError
              $  "Data constructors are not of the same type: "
              <> showSet cset
            return $ not (cset `S.isProperSubsetOf` tcset)

y = print (int_to_string x)
x = print "x" 1
