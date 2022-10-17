{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module IR.MangleNames
  ( mangleProgram
  ) where

import qualified Common.Compiler               as Compiler
import           Common.Identifiers             ( Identifiable(..)
                                                , IsString(..)
                                                )
import qualified IR.IR                         as I

import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.State            ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                , gets
                                                , modify
                                                )
import           Data.Bifunctor                 ( Bifunctor(..) )
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes
                                                , fromJust
                                                )
import qualified Data.Set                      as S

data MangleCtx = MangleCtx
  { localScope  :: M.Map I.VarId I.VarId
  , globalScope :: M.Map I.VarId I.VarId
  }

-- | Mangling monad
newtype Mangle a = Mangle (StateT MangleCtx Compiler.Pass a)
  deriving Functor                      via (StateT MangleCtx Compiler.Pass)
  deriving Applicative                  via (StateT MangleCtx Compiler.Pass)
  deriving Monad                        via (StateT MangleCtx Compiler.Pass)
  deriving MonadFail                    via (StateT MangleCtx Compiler.Pass)
  deriving (MonadError Compiler.Error)  via (StateT MangleCtx Compiler.Pass)
  deriving (MonadState MangleCtx)       via (StateT MangleCtx Compiler.Pass)

runMangle :: Mangle a -> Compiler.Pass a
runMangle (Mangle m) =
  evalStateT m MangleCtx { localScope = M.empty, globalScope = M.empty }

withLocals :: [(I.VarId, I.VarId)] -> Mangle a -> Mangle a
withLocals vs m = do
  locals <- gets localScope
  modify $ \st -> st { localScope = M.fromList vs `M.union` locals }
  a <- m
  modify $ \st -> st { localScope = locals }
  return a

tellGlobals :: [(I.VarId, I.VarId)] -> Mangle ()
tellGlobals vs = do
  let vs' = map (\(a, b) -> (b, a)) vs
  modify $ \st -> st { globalScope = M.fromList vs' `M.union` globalScope st }

normalize :: I.VarId -> I.VarId
normalize = fromString . map m . ident
 where
  m '\'' = '_'
  m a    = a

pickId :: I.VarId -> Mangle (I.VarId, I.VarId)
pickId v = do
  let v' = normalize v
  inUse <- alreadyInUse v'
  if inUse then pick 1 else return (v, v')
 where
  pick :: Int -> Mangle (I.VarId, I.VarId)
  pick i = do
    let v' = normalize (v <> fromString (show i))
    inUse <- alreadyInUse v'
    if inUse then pick (i + 1) else return (v, v')

  alreadyInUse :: I.VarId -> Mangle Bool
  alreadyInUse v' = gets (M.member v' . globalScope)

withMangled :: [I.Binder] -> Mangle a -> Mangle ([I.Binder], a)
withMangled vs m = do
  vs' <- mapM pickBinder vs
  tellGlobals $ catMaybes vs'
  withLocals (catMaybes vs') $ do
    a <- m
    return (map getBinder vs', a)
 where
  pickBinder (Just v) = Just <$> pickId v
  pickBinder Nothing  = return Nothing
  getBinder (Just (_, v')) = Just v'
  getBinder Nothing        = Nothing

mangleProgram :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
mangleProgram p = runMangle $ do
  let eds = map (\(v, _) -> (v, v)) $ I.externDecls p
  tellGlobals eds
  withLocals eds $ do
    let (tvs, tes) = unzip $ map (first Just) $ I.programDefs p
    (tvs', tes') <- withMangled tvs $ mapM mangleExpr tes
    return p { I.programDefs = zip (map fromJust tvs') tes' }

mangleExpr :: I.Expr I.Type -> Mangle (I.Expr I.Type)
mangleExpr (I.Var i t) = do
  mv <- gets (M.lookup i . localScope)
  I.Var <$> maybe err return mv <*> pure t
 where
  err = Compiler.unexpected $ "MangleNames: Could not find I.Var " <> show i
mangleExpr e@(I.Data _ _                   ) = return e
mangleExpr e@(I.Lit  _ _                   ) = return e
mangleExpr (I.App f a t) = I.App <$> mangleExpr f <*> mangleExpr a <*> pure t
mangleExpr (  I.Let (unzip -> (vs, ds)) b t) = do
  (vs', (ds', b')) <-
    withMangled vs $ (,) <$> mapM mangleExpr ds <*> mangleExpr b
  return $ I.Let (zip vs' ds') b' t
mangleExpr (I.Lambda i b t) = do
  ([i'], b') <- withMangled [i] $ mangleExpr b
  return $ I.Lambda i' b' t
mangleExpr (I.Match s as t) = do
  s'  <- mangleExpr s
  as' <- mapM mangleArm as
  return $ I.Match s' as' t
mangleExpr (I.Prim p es t) = I.Prim p <$> mapM mangleExpr es <*> pure t

mangleArm :: (I.Alt, I.Expr I.Type) -> Mangle (I.Alt, I.Expr I.Type)
mangleArm (alt, ex) = do
  snd <$> withMangled (map Just $ S.toList $ I.altBinders alt)
                      ((,) <$> mangleAlt alt <*> mangleExpr ex)
 where
  mangleAlt (  I.AltData d bs) = I.AltData d <$> mapM lookupBinder bs
  mangleAlt a@(I.AltLit     _) = return a
  mangleAlt (  I.AltDefault b) = I.AltDefault <$> lookupBinder b

  lookupBinder :: I.Binder -> Mangle I.Binder
  lookupBinder Nothing = return Nothing
  lookupBinder (Just i) =
    gets localScope >>= maybe err (return . Just) . M.lookup i
   where
    err = Compiler.unexpected $ "mangleAlt: Could not find I.Var " <> show i
