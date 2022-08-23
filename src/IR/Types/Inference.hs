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
import           Common.Identifiers             ( IsString(..)
                                                , TVarId
                                                , showId
                                                )

import           Control.Monad                  ( (<=<)
                                                , forM
                                                )
import           Data.Bifunctor                 ( Bifunctor(..) )
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes
                                                , fromJust
                                                , mapMaybe
                                                )
import qualified Data.Set                      as S


data InferCtx = InferCtx
  { varMap  :: M.Map VarId U.Scheme
  , dconMap :: M.Map DConId U.Scheme
  }

instance U.HasFreeVars InferCtx where
  freeVars = fmap S.unions . mapM U.freeVars . M.elems . varMap

type Infer = U.InferM InferCtx

lookupBinding :: VarId -> Infer U.Scheme
lookupBinding x =
  U.ask >>= maybe (U.throwError unbound) return . M.lookup x . varMap
 where
  unbound = Compiler.TypeError $ fromString $ "Unbound variable: " <> show x

withBindings :: [(Binder, U.Scheme)] -> Infer a -> Infer a
withBindings bs = U.local
  $ \s -> s { varMap = foldr (uncurry M.insert) (varMap s) vs }
 where
  vs = mapMaybe unBind bs
  unBind (Just v, s) = Just (v, s)
  unBind _           = Nothing

lookupDCon :: DConId -> Infer U.Scheme
lookupDCon x =
  U.ask >>= maybe (U.throwError unbound) return . M.lookup x . dconMap
 where
  unbound =
    Compiler.TypeError $ fromString $ "Unbound data constructor: " <> show x

inferProgram :: Program [Type] -> Compiler.Pass (Program Type)
inferProgram p = U.runInfer emptyCtx $ do
  (bs, ds) <- unzip . fromBinders <$> inferDefs (toBinders $ programDefs p)
  ds'      <- mapM (mapM $ U.freeze <=< U.applyBindings . T.unScheme) ds
  return $ p { programDefs = zip bs ds' }
 where
  emptyCtx    = InferCtx M.empty M.empty
  toBinders   = map $ first Just
  fromBinders = map $ first fromJust

checkAgainst :: [Type] -> U.Type -> Infer U.Type
checkAgainst _ = return

inferDefs :: [(Binder, Expr [Type])] -> Infer [(Binder, Expr U.Scheme)]
inferDefs (unzip -> (bs, ds)) = do
  ts'  <- map T.trivialScheme <$> mapM (const U.fresh) (catMaybes bs)
  ds'  <- withBindings (zip bs ts') $ mapM inferExpr ds
  ds'' <- mapM (mapM U.generalize) ds'
  return $ zip bs ds''

inferExpr :: Expr [Type] -> Infer (Expr U.Type)
inferExpr (Var v ts) = do
  t <- U.instantiate =<< lookupBinding v
  Var v <$> checkAgainst ts t
inferExpr (Data v ts) = do
  t <- U.instantiate =<< lookupDCon v
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
  ds' <- inferDefs ds
  b'  <- withBindings (map (second extract) ds') $ inferExpr b
  Let (fmap (second $ fmap T.unScheme) ds') b' <$> checkAgainst ts (extract b')
inferExpr (Lambda a b ts) = do
  at <- U.fresh
  withBindings [(a, T.trivialScheme at)] $ do
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
  t   <- U.instantiate =<< lookupPrim (length es) p
  t =:= U.foldArrow (map extract es', rt)
  Prim p es' <$> checkAgainst ts rt

lookupPrim :: Int -> Primitive -> Infer U.Scheme
lookupPrim _ New   = return $ T.forall ["a"] $ var "a" -:> U.Ref (var "a")
lookupPrim _ Dup   = return $ T.forall ["a"] $ var "a" -:> var "a"
lookupPrim _ Drop  = return $ T.forall ["a"] $ var "a" -:> U.Unit
lookupPrim _ Deref = return $ T.forall ["a"] $ U.Ref (var "a") -:> var "a"
lookupPrim _ Assign =
  return $ T.forall ["a"] $ U.Ref (var "a") -:> var "a" -:> U.Unit
lookupPrim _ After =
  return $ T.forall ["a"] $ U.Time -:> U.Ref (var "a") -:> var "a" -:> U.Unit
lookupPrim _   (CQuote _) = return $ T.forall ["a"] $ var "a"
lookupPrim _   Loop       = return $ T.forall ["a"] $ var "a" -:> U.Unit
lookupPrim _   Break      = return $ T.trivialScheme U.Unit -- TODO: this is should be void?
lookupPrim _   Now        = return $ T.trivialScheme $ U.Unit -:> U.Time
lookupPrim _   (PrimOp _) = return $ T.trivialScheme $ U.I32 -:> U.I32 -:> U.I32
-- TODO: ^ this should actually be returning bool for some ops
lookupPrim len Par        = return $ T.forall tvs $ U.foldArrow (args, ret)
 where
  tvs  = take len $ map (("a" <>) . showId) [(1 :: Int) ..]
  args = map var tvs
  ret  = U.tuple args
lookupPrim len Wait = return $ T.forall tvs $ U.foldArrow (args, ret)
 where
  tvs  = take len $ map (("a" <>) . showId) [(1 :: Int) ..]
  args = map var tvs
  ret  = U.Unit
lookupPrim _ p =
  Compiler.unexpected $ "lookupPrim could not handle: " ++ show p
-- lookupPrim (CCall cs) = _wC TODO:
-- lookupPrim (FfiCall vi) = _wD TODO:

lookupLit :: Literal -> Infer U.Scheme
lookupLit (LitIntegral _) = return $ T.trivialScheme U.I32
-- TODO: ^ integral typeclasses
lookupLit LitEvent        = return $ T.trivialScheme U.Unit

var :: TVarId -> U.Type
var = U.TVar

infixr 5 -:>
(-:>) :: U.Type -> U.Type -> U.Type
(-:>) = U.Arrow
