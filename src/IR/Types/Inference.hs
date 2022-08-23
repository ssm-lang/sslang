{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module IR.Types.Inference
  ( inferProgram
  ) where

import           IR.IR
import qualified IR.Types.Type                 as T
import qualified IR.Types.Unification          as U
import           IR.Types.Unification           ( (=:=) )

import qualified Common.Compiler               as Compiler
import           Common.Identifiers             ( Identifiable(..)
                                                , Identifier(..)
                                                , IsString(..)
                                                , TVarId
                                                , fromId
                                                , showId
                                                )

import           Control.Monad                  ( (<=<)
                                                , forM
                                                , unless
                                                , when
                                                , zipWithM
                                                )
import           Data.Bifunctor                 ( Bifunctor(..) )
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes
                                                , fromJust
                                                , mapMaybe
                                                )
import qualified Data.Set                      as S

var :: TVarId -> U.Type
var = U.TVar

infixr 5 -:>
(-:>) :: U.Type -> U.Type -> U.Type
(-:>) = U.Arrow

newtype InferCtx = InferCtx
  { varMap  :: M.Map Identifier U.Scheme
  }

type Infer = U.InferM InferCtx

checkAgainst :: [Type] -> U.Type -> Infer U.Type
checkAgainst _ = return

inferProgram :: Program [Type] -> Compiler.Pass (Program Type)
inferProgram p = U.runInfer (InferCtx M.empty) $ do
  ebs <- mapM externBindings $ externDecls p
  dbs <- concat <$> mapM typedefBindings (typeDefs p)
  withBindings (ebs ++ dbs) $ do
    (bs, ds) <- unzip . fromBinders <$> inferDefs (toBinders $ programDefs p)
    ds'      <- mapM (mapM $ U.freeze <=< U.applyBindings . T.unScheme) ds
    return $ p { programDefs = zip bs ds' }
 where
  toBinders   = map $ first Just
  fromBinders = map $ first fromJust

externBindings :: (VarId, Type) -> Infer (Binder, U.Scheme)
externBindings (v, t) = do
  s <- U.generalize $ U.unfreeze t
  return (Just v, s)

typedefBindings :: (TConId, TypeDef) -> Infer [(Binder, U.Scheme)]
typedefBindings (tc, TypeDef { variants = vs, targs = tvs }) =
  forM vs $ \(dc, tv) -> do
    ts <- map U.unfreeze <$> case tv of
      VariantUnnamed ts -> return ts
      VariantNamed   _  -> Compiler.todo
        "typedefBindings cannot yet handle variants with named fields"

    let t = U.TCon tc $ map U.TVar tvs
        s = T.forall tvs $ U.foldArrow (ts, t)

    ftvs <- U.freeVars s
    unless (S.empty == ftvs) $ do
      Compiler.typeError $ unlines
        [ "Type definition contains unbound type variables:"
        , "        " <> unwords (map show $ S.toList ftvs)
        , "for type : " <> show tc
        , "from data constructor: " <> show dc
        ]

    return (Just $ fromId dc, s)

inferDefs :: [(Binder, Expr [Type])] -> Infer [(Binder, Expr U.Scheme)]
inferDefs (unzip -> (bs, ds)) = do
  ts'  <- map (T.forall []) <$> mapM (const U.fresh) (catMaybes bs)
  ds'  <- withBindings (zip bs ts') $ mapM inferExpr ds
  ds'' <- mapM (mapM U.generalize) ds'
  return $ zip bs ds''

inferExpr :: Expr [Type] -> Infer (Expr U.Type)
inferExpr (Var v ts) = do
  t <- U.instantiate =<< lookupBinding v
  Var v <$> checkAgainst ts t
inferExpr (Data v ts) = do
  t <- U.instantiate =<< lookupBinding v
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
  withBindings [(a, T.forall [] at)] $ do
    b' <- inferExpr b
    Lambda a b' <$> checkAgainst ts (at -:> extract b')
inferExpr (Match s as ts) = do
  s'  <- inferExpr s
  rt  <- U.fresh
  as' <- forM as $ \(a, c) -> do
    bs <- inferAlt (extract s') a
    withBindings bs $ do
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

inferAlt :: U.Type -> Alt -> Infer [(Binder, U.Scheme)]
inferAlt t (AltData d as) = do
  t' <- U.instantiate =<< lookupBinding d
  t =:= t'
  let (ats, _) = U.unfoldArrow t'
  when (length ats /= length as) $ do
    Compiler.typeError $ unlines
      [ "Wrong number of arguments for data constructor: " <> show d
      , "Expected: " <> show (length ats)
      , "Got: " <> show (length as)
      ]
  let as' = map AltDefault as -- TODO: remove for nested patterns, just use as
  concat <$> zipWithM inferAlt ats as'
inferAlt t (AltLit l) = do
  t' <- U.instantiate =<< lookupLit l
  t =:= t'
  return []
inferAlt t (AltDefault b) = return [(b, T.forall [] t)]

lookupPrim :: Int -> Primitive -> Infer U.Scheme
lookupPrim _ New   = return $ T.forall ["a"] $ var "a" -:> U.Ref (var "a")
lookupPrim _ Dup   = return $ T.forall ["a"] $ var "a" -:> var "a"
lookupPrim _ Drop  = return $ T.forall ["a"] $ var "a" -:> U.Unit
lookupPrim _ Deref = return $ T.forall ["a"] $ U.Ref (var "a") -:> var "a"
lookupPrim _ Assign =
  return $ T.forall ["a"] $ U.Ref (var "a") -:> var "a" -:> U.Unit
lookupPrim _ After =
  -- return $ T.forall ["a"] $ U.Time -:> U.Ref (var "a") -:> var "a" -:> U.Unit
  -- TODO: ^ use this one
  return $ T.forall ["a"] $ U.I32 -:> U.Ref (var "a") -:> var "a" -:> U.Unit
lookupPrim _   (CQuote _)  = return $ T.forall ["a"] $ var "a"
lookupPrim _   Loop        = return $ T.forall ["a"] $ var "a" -:> U.Unit
lookupPrim _   Break       = return $ T.forall [] U.Unit -- TODO: this is should be void?
lookupPrim _   Now         = return $ T.forall [] $ U.Unit -:> U.Time
lookupPrim _   (FfiCall f) = lookupBinding f
lookupPrim _   (CCall   _) = return $ T.forall ["a"] $ var "a" -- Any type
lookupPrim _   (PrimOp  _) = return $ T.forall [] $ U.I32 -:> U.I32 -:> U.I32
-- TODO: ^ this should actually be returning bool for some ops
lookupPrim len Par         = return $ T.forall tvs $ U.foldArrow (args, ret)
 where
  tvs  = take len $ map (("a" <>) . showId) [(1 :: Int) ..]
  args = map var tvs
  ret  = U.tuple args
lookupPrim len Wait = return $ T.forall tvs $ U.foldArrow (args, ret)
 where
  tvs  = take len $ map (("a" <>) . showId) [(1 :: Int) ..]
  args = map var tvs
  ret  = U.Unit

lookupLit :: Literal -> Infer U.Scheme
lookupLit (LitIntegral _) = return $ T.forall [] U.I32
-- TODO: ^ integral typeclasses
lookupLit LitEvent        = return $ T.forall [] U.Unit

instance U.HasFreeVars InferCtx where
  freeVars = fmap S.unions . mapM U.freeVars . M.elems . varMap

withBindings :: [(Binder, U.Scheme)] -> Infer a -> Infer a
withBindings bs = U.local
  $ \s -> s { varMap = foldr (uncurry M.insert) (varMap s) vs }
 where
  vs = mapMaybe unBind bs
  unBind (Just v, s) = Just (fromId v, s)
  unBind _           = Nothing

lookupBinding :: Identifiable i => i -> Infer U.Scheme
lookupBinding x =
  U.ask >>= maybe (U.throwError unbound) return . M.lookup (fromId x) . varMap
 where
  unbound = Compiler.TypeError $ fromString $ "Unbound variable: " <> show x
