{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module IR.Types.Inference
  ( inferProgram
  ) where

import           IR.IR
import qualified IR.Types.Type                 as T
import qualified IR.Types.Unification          as U
import           IR.Types.Unification           ( (=:=) )

import qualified Common.Compiler               as Compiler
import           Common.Identifiers             ( TVarId )
import           Control.Monad                  ( (<=<)
                                                , forM
                                                )
import           Data.Bifunctor                 ( Bifunctor(..) )
import           Data.Maybe                     ( catMaybes )

inferProgram :: Program [Type] -> Compiler.Pass (Program Type)
inferProgram p = U.runInfer $ do
  (bs, ds) <- unzip <$> inferDefs (map (first Just) $ programDefs p)
  bs'      <- mapM (maybe undefined return) bs
  ds'      <- mapM (mapM $ U.freeze <=< U.applyBindings . T.unScheme) ds
  return $ p { programDefs = zip bs' ds' }

checkAgainst :: [Type] -> U.Type -> U.Infer U.Type
checkAgainst _ = return

inferDefs :: [(Binder, Expr [Type])] -> U.Infer [(Binder, Expr U.Scheme)]
inferDefs (unzip -> (bs, ds)) = do
  let bs' = catMaybes bs
  ts'  <- map T.trivialScheme <$> mapM (const U.fresh) bs'
  ds'  <- U.withBindings (zip bs' ts') $ mapM inferExpr ds
  ds'' <- mapM (mapM U.generalize) ds'
  return $ zip bs ds''

inferExpr :: Expr [Type] -> U.Infer (Expr U.Type)
inferExpr (Var v ts) = do
  t <- U.instantiate =<< U.lookupBinding v
  Var v <$> checkAgainst ts t
inferExpr (Data v ts) = do
  t <- U.instantiate =<< U.lookupBinding v
  Data v <$> checkAgainst ts t
inferExpr (Lit l ts) = do
  t <- U.instantiate =<< lookupLit l
  Lit l <$> checkAgainst ts t
inferExpr (App f a ts) = do
  f' <- inferExpr f
  a' <- inferExpr a
  rt <- U.fresh
  extract f' =:= extract a' -:> rt
  App f' a' <$> checkAgainst ts rt
inferExpr (Let ds b ts) = do
  _ds' <- inferDefs ds
  b'   <- inferExpr b
  Let undefined b' <$> checkAgainst ts (extract b')
inferExpr (Lambda a b ts) = do
  at <- U.fresh
  U.withBindings ((, T.trivialScheme at) <$> catMaybes [a]) $ do
    b' <- inferExpr b
    Lambda a b' <$> checkAgainst ts (at -:> extract b')
inferExpr (Match s as ts) = do
  s'  <- inferExpr s
  rt  <- U.fresh
  as' <- forM as $ \(a, c) -> do
    -- TODO: grap scope from a
    c' <- inferExpr c
    rt =:= extract c'
    return (a, c')
  Match s' as' <$> checkAgainst ts rt
inferExpr (Prim p es ts) = do
  es' <- mapM inferExpr es
  rt  <- U.fresh
  t   <- U.instantiate =<< lookupPrim p
  t =:= U.foldArrow (map extract es', rt)
  Prim p es' <$> checkAgainst ts rt

lookupPrim :: Primitive -> U.Infer U.Scheme
lookupPrim New   = return $ T.forall ["a"] $ var "a" -:> U.Ref (var "a")
lookupPrim Dup   = return $ T.forall ["a"] $ var "a" -:> var "a"
lookupPrim Drop  = return $ T.forall ["a"] $ var "a" -:> U.Unit
lookupPrim Deref = return $ T.forall ["a"] $ U.Ref (var "a") -:> var "a"
lookupPrim Assign =
  return $ T.forall ["a"] $ U.Ref (var "a") -:> var "a" -:> U.Unit
lookupPrim After =
  return $ T.forall ["a"] $ U.Time -:> U.Ref (var "a") -:> var "a" -:> U.Unit
lookupPrim (CQuote _) = return $ T.forall ["a"] $ var "a"
lookupPrim Loop       = return $ T.forall ["a"] $ var "a" -:> U.Unit
lookupPrim Break      = return $ T.trivialScheme U.Unit -- TODO: this is should be void?
lookupPrim Now        = return $ T.trivialScheme $ U.Unit -:> U.Time
lookupPrim (PrimOp _) = return $ T.trivialScheme $ U.I32 -:> U.I32 -:> U.I32
-- TODO: ^ this should actually be returning bool for some ops
lookupPrim _          = undefined
-- lookupPrim Par = _wv
-- lookupPrim Wait = _ww
-- lookupPrim (CCall cs) = _wC
-- lookupPrim (FfiCall vi) = _wD

lookupLit :: Literal -> U.Infer U.Scheme
lookupLit (LitIntegral _) = return $ T.trivialScheme U.I32
-- TODO: ^ integral typeclasses
lookupLit LitEvent        = return $ T.trivialScheme U.Unit

var :: TVarId -> U.Type
var = U.TVar

infixr 5 -:>
(-:>) :: U.Type -> U.Type -> U.Type
(-:>) = U.Arrow
