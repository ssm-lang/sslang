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
showSet = "'" <> fromString . show <> "'"

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
checkMatches (A.Program ds) = runMatchFn checkExhaustion ctx
  where
    tds = mapMaybe A.getTopTypeDef ds
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

checkExhaustion :: [A.Definition] -> MatchFn ()
checkExhaustion ds = do
    mapM_ checkExpr es
  where
    takeBody (A.DefFn _ _ _ e) = e
    takeBody (A.DefPat _ e   ) = e
    es = map takeBody ds

checkExpr :: A.Expr -> MatchFn ()
checkExpr (Id _) = return ()
checkExpr (Lit _) = return ()
checkExpr (Apply Expr Expr) = 
checkExpr (Lambda [Pat] Expr) = 
checkExpr (OpRegion Expr OpRegion) = 
checkExpr (NoExpr) = 
checkExpr (Let [Definition] Expr) = 
checkExpr (While Expr Expr) = 
checkExpr (Loop Expr) = 
checkExpr (Par [Expr]) = 
checkExpr (IfElse Expr Expr Expr) = 
checkExpr (After Expr Expr Expr) = 
checkExpr (Assign Expr Expr) = 
checkExpr (Constraint Expr TypAnn) = 
checkExpr (Wait [Expr]) = 
checkExpr (Seq Expr Expr) = 
checkExpr (Break) = 
checkExpr (Match Expr [(Pat, Expr)]) = 
checkExpr (Return Expr) = 

isCompleteCSet :: S.Set Identifier -> MatchFn Bool
isCompleteCSet cset
    | S.size cset == 0 = return False
    | otherwise = do
        let cname = S.elemAt 1 cset
        c <- asks $ M.lookup cname . consMap
        case c of
            Nothing -> do
                throwError
                    $  PatternError
                    $  "Data constructor does not exist: "
                    <> showId cname
            Just c' -> do
                let tname = cType c'
                t <- asks $ M.lookup tname . typeMap
                case t of
                    Nothing ->
                        do
                                throwError
                            $  PatternError
                            $  "Type ("
                            <> showId tname
                            <> ") does not exist for data constructor: "
                            <> showId cname
                    Just t' -> do
                        let tcset = tCSet t'
                        unless (cset `S.isSubsetOf` tcset) $ do
                            throwError
                                $ PatternError
                                $ "Data constructors are not of the same type: "
                                <> showSet cset
                        return $ not (cset `S.isProperSubsetOf` tcset)
