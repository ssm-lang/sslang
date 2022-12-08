{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Hindley-Milner-Damas type inference with union-find.
module IR.Types.Inference
  ( inferProgram
  ) where

import           IR.IR
import           IR.SegmentLets                 ( segmentDefs )
import qualified IR.Types.Type                 as T
import qualified IR.Types.Unification          as U
import           IR.Types.Unification           ( (<:=)
                                                , (=:=)
                                                )

import qualified Common.Compiler               as Compiler
import           Common.Identifiers             ( HasFreeVars(..)
                                                , Identifiable(..)
                                                , Identifier(..)
                                                , IsString(..)
                                                , fromId
                                                , showId
                                                )

import           Control.Monad                  ( (<=<)
                                                , forM
                                                , forM_
                                                , unless
                                                , when
                                                , zipWithM
                                                , zipWithM_
                                                )
import           Data.Bifunctor                 ( Bifunctor(..) )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust
                                                , mapMaybe
                                                )
import qualified Data.Set                      as S

-- | Helper notation for constructing arrow types.
(-:>) :: U.Type -> U.Type -> U.Type
(-:>) = U.Arrow
infixr 5 -:>

-- | State maintained during type inference.
data InferCtx = InferCtx
  { varEnv  :: M.Map Identifier U.Scheme  -- ^ mapping from vars to schemes
  , kindEnv :: M.Map TConId T.Kind        -- ^ mapping from tcons to kinds
  }

instance U.HasFreeUVars InferCtx where
  freeUVars = fmap S.unions . mapM U.freeUVars . M.elems . varEnv

-- | Inference monad, specialized to use 'InferCtx' as the state.
type Infer = U.InferM InferCtx

-- | Perform an 'Infer' monad, with the environmnt extended by some bindings.
withBindings :: [(Binder, U.Scheme)] -> Infer a -> Infer a
withBindings bs = U.local
  $ \s -> s { varEnv = foldr (uncurry M.insert) (varEnv s) vs }
 where
  vs = mapMaybe unBind bs
  unBind (Just v, s) = Just (fromId v, s)
  unBind _           = Nothing

-- | Look up the 'U.Scheme' a data identifier in the variable environment.
lookupBinding :: Identifiable i => i -> Infer U.Scheme
lookupBinding x =
    do
    U.asks (M.lookup (fromId x) . varEnv) >>= maybe (U.throwError unbound) return
 where
  unbound = Compiler.TypeError $ fromString $ "Unbound variable: " <> show x

-- | Look up the 'T.Kind' of a type constructor in the kind environment.
lookupKind :: TConId -> Infer T.Kind
lookupKind tcon =
  U.asks (M.lookup tcon . kindEnv) >>= maybe (U.throwError missing) return
 where
  missing =
    Compiler.TypeError
      $  fromString
      $  "Could not find type constructor: "
      <> show tcon

-- | Infer the types of every node in an optionally annotated program.
inferProgram :: Program Annotations -> Compiler.Pass (Program Type)
inferProgram p = U.runInfer initCtx $ do
  ebs <- mapM externBindings $ externDecls p
  dbs <- concat <$> mapM typedefBindings (typeDefs p)
  withBindings (ebs ++ dbs) $ do
    (bs, ds) <- unzip . map (first fromJust) . fst <$> withDefs
      (map (first Just) $ programDefs p)
      (return ())
    ds' <- mapM
      (mapM $ U.freeze <=< U.applyBindings . T.unScheme <=< U.generalize)
      ds
    return $ p { programDefs = zip bs ds' }
 where
  -- | Obtain the kind of each type constructor.
  kenv = M.fromList $ map (second $ length . targs) $ typeDefs p

  -- | Construct the inintial inference context.
  initCtx =
    InferCtx { varEnv = M.empty, kindEnv = T.builtinKinds `M.union` kenv }

-- | Create bindings for the extern symbols.
externBindings :: (VarId, Type) -> Infer (Binder, U.Scheme)
externBindings (v, t) = do
  s <- U.generalize $ U.unfreeze t
  return (Just v, s)

-- | Create bindings for the data constructors of each type definition.
typedefBindings :: (TConId, TypeDef) -> Infer [(Binder, U.Scheme)]
typedefBindings (tc, TypeDef { variants = vs, targs = tvs }) =
  forM vs $ \(dc, tv) -> do
    ts <- map U.unfreeze <$> case tv of
      VariantUnnamed ts -> return ts
      VariantNamed   _  -> Compiler.todo
        "typedefBindings cannot yet handle variants with named fields"

    let t = U.TCon tc $ map U.TVar tvs
        s = T.forall tvs $ U.foldArrow (ts, t)

    ftvs <- U.freeUVars s
    unless (S.empty == ftvs) $ do
      Compiler.typeError $ unlines
        [ "Type definition contains unbound type variables:"
        , "        " <> unwords (map show $ S.toList ftvs)
        , "for type : " <> show tc
        , "from data constructor: " <> show dc
        ]

    return (Just $ fromId dc, s)

-- | Perform inference, with the environment extended by a list of bindings.
withDefs
  :: [(Binder, Expr Annotations)]
  -> Infer a
  -> Infer ([(Binder, Expr U.Type)], a)
withDefs defs m = go $ segmentDefs defs
 where
  go :: [[(Binder, Expr Annotations)]] -> Infer ([(Binder, Expr U.Type)], _)
  go ((unzip -> (bs, es)) : dss) = do
    -- Create fresh unification variable for each binder
    ts <- mapM (const U.fresh) bs

    -- Create empty scheme for each binder's unification variable
    let ss = T.forall [] <$> ts

    -- Infer definitions with those schemes in scope
    es' <- withBindings (zip bs ss) $ mapM inferExpr es

    -- Unify unification variables with the inferred type of each definition
    forM_ (zip ts es') $ \(t, extract -> t') -> t =:= t'

    ss' <- forM es' $ \e -> if isValue e
      then U.generalize $ extract e
      else return $ T.forall [] $ extract e

    first (zip bs es' ++) <$> withBindings (zip bs ss') (go dss)
  go _ = ([], ) <$> m

-- | Type inference rules for type-annotated expressions.
inferExpr :: Expr Annotations -> Infer (Expr U.Type)
inferExpr (Var v ts) = do
  t <- U.instantiate =<< lookupBinding v
  Var v <$> checkAgainst ts t
inferExpr (Data v ts) = do
  t <- U.instantiate =<< lookupBinding v
  Data v <$> checkAgainst ts t
inferExpr (Lit l ts) = do
  t <- U.instantiate =<< inferLit l
  Lit l <$> checkAgainst ts t
inferExpr (App f a ts) = do
  f' <- inferExpr f
  a' <- inferExpr a
  rt <- U.fresh
  extract f' =:= extract a' -:> rt
  App f' a' <$> checkAgainst ts rt
inferExpr (Let ds e ts) = do
  (ds', e') <- withDefs ds $ inferExpr e
  Let ds' e' <$> checkAgainst ts (extract e')
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
  t   <- U.instantiate =<< inferPrim (length es) p
  t =:= U.foldArrow (map extract es', rt)
  Prim p es' <$> checkAgainst ts rt

-- | Obtain bindings for a pattern match arm, according to the scrutinee's type.
inferAlt :: U.Type -> Alt -> Infer [(Binder, U.Scheme)]
inferAlt t (AltData d as) = do
  (ats, t') <- U.unfoldArrow <$> (U.instantiate =<< lookupBinding d)
  t =:= t'
  when (length ats /= length as) $ do
    Compiler.typeError $ unlines
      [ "Wrong number of arguments for data constructor: " <> show d
      , "Expected: " <> show (length ats)
      , "Got: " <> show (length as)
      ]
  let as' = map AltDefault as -- TODO: remove for nested patterns, just use as
  concat <$> zipWithM inferAlt ats as'
inferAlt t (AltLit l) = do
  t' <- U.instantiate =<< inferLit l
  t =:= t'
  return []
inferAlt t (AltDefault b) = return [(b, T.forall [] t)]

-- | Type inference rules for primitives of a given arity.
inferPrim :: Int -> Primitive -> Infer U.Scheme
inferPrim _ New   = return $ T.forall ["a"] $ U.TVar "a" -:> U.Ref (U.TVar "a")
inferPrim _ Dup   = return $ T.forall ["a"] $ U.TVar "a" -:> U.TVar "a"
inferPrim _ Drop  = return $ T.forall ["a"] $ U.TVar "a" -:> U.Unit
inferPrim _ Deref = return $ T.forall ["a"] $ U.Ref (U.TVar "a") -:> U.TVar "a"
inferPrim _ Assign =
  return $ T.forall ["a"] $ U.Ref (U.TVar "a") -:> U.TVar "a" -:> U.Unit
inferPrim _ After =
  -- return $ T.forall ["a"] $ U.Time -:> U.Ref (U.TVar "a") -:> U.TVar "a" -:> U.Unit
  -- TODO: ^ use this one
  return
    $   T.forall ["a"]
    $   U.I32
    -:> U.Ref (U.TVar "a")
    -:> U.TVar "a"
    -:> U.Unit
-- inferPrim _   Now         = return $ T.forall [] $ U.Unit -:> U.Time
  -- TODO: ^ use this one
inferPrim _   Now         = return $ T.forall [] $ U.Unit -:> U.I32
inferPrim _   (CQuote _)  = return $ T.forall ["a"] $ U.TVar "a"
inferPrim _   Loop        = return $ T.forall ["a"] $ U.TVar "a" -:> U.Unit
inferPrim _   Break       = return $ T.forall [] U.Unit -- TODO: this is should be void?
inferPrim _   (FfiCall f) = lookupBinding f
inferPrim _   (CCall   _) = return $ T.forall ["a"] $ U.TVar "a" -- Any type
inferPrim _   (PrimOp  _) = return $ T.forall [] $ U.I32 -:> U.I32 -:> U.I32
-- TODO: ^ this should actually be returning bool for some ops
inferPrim len Par         = return $ T.forall tvs $ U.foldArrow (args, ret)
 where
  tvs  = take len $ map (("a" <>) . showId) [(1 :: Int) ..]
  args = map U.TVar tvs
  ret  = U.tuple args
inferPrim len Wait = return $ T.forall tvs $ U.foldArrow (args, ret)
 where
  tvs  = take len $ map (("a" <>) . showId) [(1 :: Int) ..]
  args = map U.TVar tvs
  ret  = U.Unit

-- | Type inference rules for literals.
inferLit :: Literal -> Infer U.Scheme
inferLit LitEvent        = return $ T.forall [] U.Unit
inferLit (LitIntegral _) = return $ T.forall [] U.I32
-- TODO: ^ integral typeclasses

-- | Check an inferred type (RHS) against some annotations (LHS).
checkAgainst :: Annotations -> U.Type -> Infer U.Type
checkAgainst anns u = do
  check (reverse $ T.unAnnotations anns) u
 where
  check []       t = return t
  check (a : as) t = do
    t' <- unravelAnnotation t a
    checkKind t
    checkKind t'
    checksOut <- t <:= t'
    unless checksOut $ do
      Compiler.typeError $ unlines
        [ "Type annotation is too general:"
        , "Annotation: " ++ show a
        , "Actual type: " ++ show t
        , "Instantiated ann: " ++ show t'
        ]
    t =:= t'    -- Now that the annotation is valid, actually unify it
    check as t' -- Pass on the more general annotation type

-- | Checks the kind of all type constructors in a type.
checkKind :: U.Type -> Infer ()
checkKind (U.TCon tcon ts) = do
  k <- lookupKind tcon
  unless (k == length ts) $ do
    U.throwError $ wrongKind k $ length ts
  mapM_ checkKind ts
 where
  wrongKind expected actual = Compiler.TypeError $ fromString $ unlines
    [ "Wrong kind for type: " <> show tcon
    , "Expected: " <> fmtKind expected
    , "Actual: " <> fmtKind actual
    ]
  fmtKind k = concat $ "*" : replicate k " -> *"
checkKind _ = return ()

{- | Unravel an annotation into a 'Type' according to some inferred 'Type'.

This is necessary because not all annotations are well-formed types; instead,
they may be annotations lifted from annotations in function definitions and
patterns, where data constructor patterns need to be looked up and unpacked.
-}
unravelAnnotation :: U.Type -> Annotation -> Infer U.Type
unravelAnnotation u (T.AnnType (U.unfreeze -> t)) = do
  t' <- rewriteHoles u t
  U.instantiate $ T.forall (S.toList $ freeVars t') t'
unravelAnnotation (U.unfoldArrow -> (us, u)) (T.AnnArrows as a) =
  curry U.foldArrow
    <$> zipWithM unravelAnnotation us as
    <*> unravelAnnotation (U.foldArrow (drop (length as) us, u)) a
unravelAnnotation u (T.AnnDCon dc as) = do
  (ts, t) <- U.unfoldArrow <$> (U.instantiate =<< lookupBinding dc)
  t =:= u
  when (length ts /= length as) $ do
    Compiler.typeError $ fromString $ unlines
      [ "Wrong number of arguments in pattern for constructor " <> show dc
      , "Expected: " ++ show (length ts)
      , "Got: " ++ show (length as)
      ]
  zipWithM_ unravelAnnotation ts as
  return t

-- | Rewrite each 'Type' 'Hole' in the RHS according to the LHS.
rewriteHoles :: U.Type -> U.Type -> Infer U.Type
rewriteHoles u U.Hole = return u
rewriteHoles (U.TCon ud us) (U.TCon td ts)
  | ud == td = U.TCon ud <$> zipWithM rewriteHoles us ts
  | otherwise = Compiler.typeError $ unlines
    [ "Type constructor mismatch in annotation: "
    , "Annotated: " <> show td
    , "Actual: " <> show ud
    ]
rewriteHoles _ u = return u
