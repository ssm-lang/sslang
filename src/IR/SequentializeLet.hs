{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
module IR.SequentializeLet where

import qualified Control.Monad.Trans.UnionFind as UF
import           Data.Generics                  ( Data
                                                , Proxy
                                                , everywhereM
                                                , mkM
                                                )
import           Data.List                      ( partition )
import qualified Data.Map                      as M
import qualified Data.Set                      as S

import           Common.Compiler                ( Error
                                                , MonadError
                                                , Pass
                                                , Warning(..)
                                                , warn
                                                )
import           Control.Monad.State            ( MonadTrans(lift)
                                                , execStateT
                                                , forM
                                                , forM_
                                                , modify
                                                )
import qualified IR.IR                         as I
import           IR.Types.TypeSystem            ( TypeSystem )

type UFPass p a = (UF.UnionFindT p Pass a)

newtype UFP p a = UFP (UFPass p a)
  deriving Functor                    via (UFP p)
  deriving Applicative                via (UFP p)
  deriving Monad                      via (UFP p)
  deriving (MonadError Error)         via (UFP p)
  deriving MonadFail                  via (UFP p)

runUFP :: UFP p a -> Pass a
runUFP (UFP m) = UF.runUnionFind m

sequentializeLet :: (TypeSystem t, Data t, Data a) => Proxy t -> a -> Pass a
sequentializeLet p = everywhereM $ mkM $ sequentializeLetExpr p

sequentializeLetExpr
  :: (TypeSystem t, Data t) => Proxy t -> I.Expr t -> Pass (I.Expr t)
sequentializeLetExpr p (I.Let defs body t) = do
  let (allFuncDefs, valDefs) = partition (isFunction . snd) defs
      isFunction I.Lambda{} = True
      isFunction _          = False

      getFuncDefs :: (I.Binder, I.Expr t) -> Pass [(I.VarId, I.Expr t)]
      getFuncDefs (Just v, e) = return [(v, e)]
      getFuncDefs _           = do
        warn $ PatternWarning "Bound function to empty binder"
        return []

  funcDefs      <- concat <$> mapM getFuncDefs allFuncDefs
  splitFuncDefs <- runUFP $ do
    -- Create points out of all newly defined variables
    pointsList <- forM funcDefs $ \(d, _) -> do
      point <- UFP $ UF.fresh d
      return (d, point)

    let definedVars = S.fromList $ map fst funcDefs
        points      = M.fromList pointsList

    forM_ funcDefs $ \(def, expr) -> do
      let usedVars = definedVars `S.intersection` I.usedFreeVars p expr
      forM_ usedVars $ \used -> do
        Just u <- return $ M.lookup used points
        Just d <- return $ M.lookup def points
        UFP $ UF.union u d

    fds <- (`execStateT` M.empty) $ do
      forM_ funcDefs $ \(def, expr) -> do
        Just d <- return $ M.lookup def points
        point  <- lift $ UFP $ UF.repr d
        key    <- lift $ UFP $ UF.descriptor point
        modify
          $ M.insertWithKey (\_ new old -> old ++ new) key [(Just def, expr)]
    return $ map snd $ M.toList fds
  return $ I.makeLetChain (splitFuncDefs ++ [valDefs]) body t
sequentializeLetExpr _ e = return e
