{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module IR.MangleNames (mangleProgram) where

import qualified Common.Compiler as Compiler
import Common.Identifiers (Identifiable (..), IsString (..))
import qualified IR.IR as I

import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState, StateT (..), evalStateT, gets, modify)
import Data.Bifunctor (Bifunctor (..))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as S
import Control.Monad (zipWithM)


normalize :: I.VarId -> I.VarId
normalize = fromString . map tr . ident
 where
  tr '\'' = '_'
  tr a = a


pickId :: M.Map I.VarId (I.SymInfo I.Type) -> I.VarId -> I.VarId
pickId globals v =
  let v' = normalize v
   in if alreadyInUse v'
        then pick 1
        else v'
 where
  pick :: Int -> I.VarId
  pick i =
    let v' = normalize (v <> "___" <> fromString (show i))
     in if alreadyInUse v'
           then pick $ i + 1
           else v'

  alreadyInUse :: I.VarId -> Bool
  alreadyInUse v' = M.member v' globals


data MangleCtx = MangleCtx
  { localScope :: M.Map I.VarId (I.SymInfo I.Type)
  , globalScope :: M.Map I.VarId (I.SymInfo I.Type)
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


withLocals :: [(I.VarId, I.SymInfo I.Type)] -> Mangle a -> Mangle a
withLocals vs m = do
  locals <- gets localScope
  modify $ \st -> st{localScope = M.fromList vs `M.union` locals}
  a <- m
  modify $ \st -> st{localScope = locals}
  return a


tellGlobals :: [(I.VarId, I.SymInfo I.Type)] -> Mangle ()
tellGlobals vs = do
  modify $ \st -> st{globalScope = M.fromList vs `M.union` globalScope st}


withMangled :: [I.Binder] -> Mangle a -> Mangle ([I.Binder], a)
withMangled vs m = do
  vs' <- mapM pickBinder vs
  tellGlobals $ catMaybes vs'
  withLocals (catMaybes vs') $ do
    a <- m
    return (map toBinder vs', a)
 where
  pickBinder :: I.Binder -> Mangle (Maybe (I.VarId, I.SymInfo I.Type))
  pickBinder (Just v) = do
    globals <- gets globalScope
    let v' = pickId globals v
    puts { globalScope = M.insert undefined undefined globals }
    -- return $ Just (v, I.SymInfo )

  pickBinder Nothing = return Nothing

  toBinder :: Maybe (I.VarId, I.VarId) -> I.Binder
  toBinder (Just (_, v')) = Just v'
  toBinder Nothing = Nothing


mangleProgram :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
mangleProgram p = runMangle $ do
  let eds = map (\(v, _) -> (v, v)) $ I.externDecls p
  tellGlobals eds
  withLocals eds $ do
    let (tvs, tes) = unzip $ map (first Just) $ I.programDefs p
    (tvs', tes') <- withMangled tvs $ mapM mangleExpr tes
    names <- gets globalScope
    return
      p
        { I.programDefs = zip (map fromJust tvs') tes'
        , I.symTable = names
        }


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


mangleArm :: (I.Alt, I.Expr I.Type) -> Mangle (I.Alt, I.Expr I.Type)
mangleArm (alt, ex) = do
  snd
    <$> withMangled
      (map Just $ S.toList $ I.altBinders alt)
      ((,) <$> mangleAlt alt <*> mangleExpr ex)
 where
  mangleAlt (I.AltData d bs) = I.AltData d <$> mapM mangleAlt bs
  mangleAlt a@(I.AltLit _) = return a
  mangleAlt (I.AltBinder b) = I.AltBinder <$> lookupBinder b

  lookupBinder :: I.Binder -> Mangle I.Binder
  lookupBinder Nothing = return Nothing
  lookupBinder (Just i) =
    gets localScope >>= maybe err (return . Just) . M.lookup i
   where
    err = Compiler.unexpected $ "mangleAlt: Could not find I.Var " <> show i
