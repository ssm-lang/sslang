{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE TupleSections #-}
module IR.SequentializeLet where

import qualified Control.Monad.Trans.UnionFind as UF
import           Data.Data
import           Data.Generics
import qualified Data.Map                      as M
import           Data.Maybe                     ( mapMaybe )
import qualified Data.Set                      as S

import qualified IR.IR                         as I
import           IR.Types.TypeSystem            ( TypeSystem )


sequentializeLet :: (TypeSystem t, Data t, Data a) => Proxy t -> a -> a
sequentializeLet p = everywhere $ mkT $ sequentializeLetExpr p

sequentializeLetExpr
  :: (TypeSystem t, Data t) => Proxy t -> I.Expr t -> I.Expr t
sequentializeLetExpr p (I.Let ds b t) = undefined
 where
  newVars = S.fromList $ mapMaybe fst ds
  dsRefs  = map dsRef ds
  dsRef (v, e) = (v, e, newVars `S.intersection` usedIdentifiers p e)

  isFunction (I.Lambda _ _ _) = True
  isFunction _                = False
sequentializeLetExpr _ e = e

usedIdentifiers
  :: (TypeSystem t, Data t) => Proxy t -> I.Expr t -> S.Set I.VarId
usedIdentifiers p = everythingWithContext mempty
                                          (<>)
                                          (mkQ (mempty, ) $ getVar p)
 where
  getVar
    :: TypeSystem t
    => Proxy t
    -> I.Expr t
    -> S.Set I.VarId
    -> (S.Set I.VarId, S.Set I.VarId)
  getVar _ (I.Var v _) s | not (v `S.member` s) = (S.singleton v, s)
  getVar _ (I.Let ds _ _) s                     = (mempty, s `S.union` newVars)
    where newVars = S.fromList $ mapMaybe fst ds
  getVar _ _ s = (mempty, s)
