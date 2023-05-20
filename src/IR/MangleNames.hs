{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- | Mangle all variable names in a program to ensure global uniqueness.

Also populate symbol table with information about those (now unique) names.

This module also exports 'pickId', which other passes can use to create fresh
variable names given some symbol table.
-}
module IR.MangleNames (mangleProgram, normalizeId, pickId) where

import qualified Common.Compiler as Compiler
import Common.Identifiers (Identifiable (..), IsString (..))
import qualified IR.IR as I

import Control.Monad (forM)
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState, StateT (..), evalStateT, gets, modify)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)


type OriginId = I.VarId
type MangledId = I.VarId


-- | Normalize an identifier (remove things like backticks etc.)
normalizeId :: Identifiable i => i -> i
normalizeId = fromString . map tr . ident
 where
  tr '\'' = '_'
  tr a = a


-- | Given a symbol table and a name, pick a new name that is globally unique.
pickId :: M.Map MangledId t -> OriginId -> MangledId
pickId globals v = if alreadyInUse v' then pick 1 else v'
 where
  v' :: MangledId
  v' = normalizeId v

  pick :: Int -> MangledId
  pick i =
    let v'' = normalizeId (v <> "__" <> fromString (show i))
     in if alreadyInUse v'' then pick $ i + 1 else v''

  alreadyInUse :: I.VarId -> Bool
  alreadyInUse = (`M.member` globals)


data MangleCtx = MangleCtx
  { localScope :: M.Map OriginId MangledId
  , globalScope :: I.SymTable I.Type
  }


-- | Mangling monad
newtype Mangle a = Mangle (StateT MangleCtx Compiler.Pass a)
  deriving (Functor) via (StateT MangleCtx Compiler.Pass)
  deriving (Applicative) via (StateT MangleCtx Compiler.Pass)
  deriving (Monad) via (StateT MangleCtx Compiler.Pass)
  deriving (MonadFail) via (StateT MangleCtx Compiler.Pass)
  deriving (MonadError Compiler.Error) via (StateT MangleCtx Compiler.Pass)
  deriving (MonadState MangleCtx) via (StateT MangleCtx Compiler.Pass)


runMangle :: Mangle a -> Compiler.Pass a
runMangle (Mangle m) =
  evalStateT m MangleCtx{localScope = M.empty, globalScope = M.empty}


withLocals :: [(OriginId, MangledId)] -> Mangle a -> Mangle a
withLocals vs m = do
  locals <- gets localScope
  modify $ \st -> st{localScope = M.fromList vs `M.union` locals}
  a <- m
  modify $ \st -> st{localScope = locals}
  return a


tellGlobal :: I.VarId -> I.SymInfo I.Type -> Mangle ()
tellGlobal v i = do
  modify $ \st -> st{globalScope = M.insert v i $ globalScope st}


withMangled :: [I.Binder I.Type] -> Mangle a -> Mangle ([I.Binder I.Type], a)
withMangled vs m = do
  vs' <- mapM pickBinder vs
  withLocals (mapMaybe toLocal vs') $ do
    a <- m
    return (map fst vs', a)
 where
  pickBinder :: I.Binder I.Type -> Mangle (I.Binder I.Type, Maybe (I.SymInfo I.Type))
  pickBinder (I.BindVar v t) = do
    globals <- gets globalScope
    let v' = pickId globals v
        info = I.SymInfo{I.symOrigin = v, I.symType = t}
    tellGlobal v' info
    return (I.BindVar v' t, Just info)
  pickBinder b = return (b, Nothing)

  toLocal :: (I.Binder I.Type, Maybe (I.SymInfo I.Type)) -> Maybe (OriginId, MangledId)
  toLocal (I.BindVar v _, Just info) = Just (I.symOrigin info, v)
  toLocal _ = Nothing


mangleProgram :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
mangleProgram p = runMangle $ do
  eds <- forM (I.externDecls p) $ \(v, t) -> do
    let info = I.SymInfo{I.symOrigin = v, I.symType = t}
    tellGlobal v info
    return (v, v)
  withLocals eds $ do
    let (tvs, tes) = unzip $ I.programDefs p
    (tvs', tes') <- withMangled tvs $ mapM mangleExpr tes
    names <- gets globalScope
    return p{I.programDefs = zip tvs' tes', I.symTable = names}


mangleExpr :: I.Expr I.Type -> Mangle (I.Expr I.Type)
mangleExpr (I.Var i t) = do
  mv <- gets (M.lookup i . localScope)
  I.Var <$> maybe err return mv <*> pure t
 where
  err = Compiler.unexpected $ "MangleNames: Could not find I.Var " <> show i
mangleExpr e@(I.Data _ _) = return e
mangleExpr e@(I.Lit _ _) = return e
mangleExpr (I.App f a t) = I.App <$> mangleExpr f <*> mangleExpr a <*> pure t
mangleExpr (I.Let (unzip -> (vs, ds)) b t) = do
  (vs', (ds', b')) <-
    withMangled vs $ (,) <$> mapM mangleExpr ds <*> mangleExpr b
  return $ I.Let (zip vs' ds') b' t
mangleExpr (I.Lambda i b t) = do
  ([i'], b') <- withMangled [i] $ mangleExpr b
  return $ I.Lambda i' b' t
mangleExpr (I.Match s as t) = do
  s' <- mangleExpr s
  as' <- mapM mangleArm as
  return $ I.Match s' as' t
mangleExpr (I.Prim p es t) = I.Prim p <$> mapM mangleExpr es <*> pure t
mangleExpr e@I.Exception{} = return e


mangleArm :: (I.Alt I.Type, I.Expr I.Type) -> Mangle (I.Alt I.Type, I.Expr I.Type)
mangleArm (alt, ex) = do
  snd
    <$> withMangled
      (I.altBinders alt)
      ((,) <$> mangleAlt alt <*> mangleExpr ex)
 where
  mangleAlt (I.AltData d bs t) = I.AltData d <$> mapM mangleAlt bs <*> pure t
  mangleAlt a@(I.AltLit _ _) = return a
  mangleAlt (I.AltBinder b) = I.AltBinder <$> lookupBinder b

  lookupBinder :: I.Binder I.Type -> Mangle (I.Binder I.Type)
  lookupBinder (I.BindVar i t) = do
    mi' <- gets $ M.lookup i . localScope
    i' <- maybe err return mi'
    return $ I.BindVar i' t
   where
    err = Compiler.unexpected $ "mangleAlt: Could not find I.Var " <> show i
  lookupBinder b = return b
