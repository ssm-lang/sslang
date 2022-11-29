{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module IR.DesugarPattern
    ( desugarPattern
    ) where

import           Codegen.Typegen                ( TConInfo(typeScrut) )
import           Common.Compiler               as Compiler
import           Common.Compiler                ( Error(..)
                                                , MonadError(..)
                                                , Pass(..)
                                                , fromString
                                                )
import           Common.Identifiers             ( Identifier(..)
                                                , isCons
                                                )
import           Control.Monad                  ( replicateM )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                , gets
                                                , modify
                                                )
import           Data.Bifunctor                 ( Bifunctor(second)
                                                , first
                                                )
import qualified Data.Map                      as M
import           Data.Maybe                     ( isJust
                                                , isNothing
                                                , mapMaybe
                                                )
import qualified Data.Set                      as S
import qualified IR.IR                         as I

type Equation = ([I.Alt], I.Expr I.Type)

data CInfo = CInfo
    { cName    :: I.DConId
    , cType    :: I.TConId
    , argsType :: [I.Type]
    , cArity   :: Int
    }
    deriving (Eq, Show)

data TInfo = TInfo
    { tName :: I.TConId
    , tCSet :: S.Set I.DConId
    }
    deriving (Eq, Show)

data DesugarCtx = DesugarCtx
    { typeMap   :: M.Map I.TConId TInfo
    , consMap   :: M.Map I.DConId CInfo
    , anonCount :: Int
    }

{-
I.TypeDef = { variants :: [(DConId, TypeVariant)]
  , targs    :: [TVarId]
  }
TypeVariant = VariantUnnamed [Type] 
-}

buildTypeMap :: [(I.TConId, I.TypeDef)] -> M.Map I.TConId TInfo
buildTypeMap = foldr tAcc M.empty
  where
    tAcc td tmap' =
        let typ   = fst td
            clist = map fst (I.variants $ snd td)
            cset  = S.fromList clist
        in  M.insert typ (TInfo { tName = typ, tCSet = cset }) tmap'

buildConsMap :: [(I.TConId, I.TypeDef)] -> M.Map I.DConId CInfo
buildConsMap args = foldr buildConsMap' M.empty args'
    where args' = map (second I.variants) args

buildConsMap'
    :: (I.TConId, [(I.DConId, I.TypeVariant)])
    -> M.Map I.DConId CInfo
    -> M.Map I.DConId CInfo
buildConsMap' arg m = foldr cAcc m (snd arg)
  where
    typ = fst arg
    getTypes (_, I.VariantUnnamed typs) = typs
    getTypes _                          = error "cannot happen"
    cAcc cd cmap' = M.insert cid c cmap'
      where
        cid       = fst cd
        argsType' = getTypes cd
        c         = CInfo { cName    = cid
                          , cType    = typ
                          , argsType = argsType'
                          , cArity   = length argsType'
                          }


newtype DesugarFn a = DesugarFn (StateT DesugarCtx Compiler.Pass a)
  deriving Functor                      via (StateT DesugarCtx Compiler.Pass)
  deriving Applicative                  via (StateT DesugarCtx Compiler.Pass)
  deriving Monad                        via (StateT DesugarCtx Compiler.Pass)
  deriving MonadFail                    via (StateT DesugarCtx Compiler.Pass)
  deriving (MonadError Error)  via (StateT DesugarCtx Compiler.Pass)
  deriving (MonadState DesugarCtx)     via (StateT DesugarCtx Compiler.Pass)


runDesugarFn :: DesugarFn a -> DesugarCtx -> Compiler.Pass a
runDesugarFn (DesugarFn m) = evalStateT m


freshVar :: DesugarFn Identifier
freshVar = do
    currCount <- gets anonCount
    modify $ \ctx -> ctx { anonCount = anonCount ctx + 1 }
    return $ fromString ("__pat_anon" ++ show currCount)


buildCtx :: [(I.TConId, I.TypeDef)] -> DesugarCtx
buildCtx tds = DesugarCtx { anonCount = 0
                          , typeMap   = buildTypeMap tds
                          , consMap   = buildConsMap tds
                          }


desugarPattern :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
desugarPattern p@I.Program { I.programDefs = defs, I.typeDefs = tds } =
    runDesugarFn
        (do
            defs' <- desugarExprsDefs defs
            return $ p { I.programDefs = defs' }
        )
        ctx
    where ctx = buildCtx tds

desugarExprsDefs
    :: [(I.VarId, I.Expr I.Type)] -> DesugarFn [(I.VarId, I.Expr I.Type)]
desugarExprsDefs defs = do
    es' <- mapM desugarExpr es
    return $ zip vs es'
    where (vs, es) = unzip defs
-- desugarExprs = mapM (Data.Bifunctor.second desugarExpr)


desugarExpr :: I.Expr I.Type -> DesugarFn (I.Expr I.Type)
desugarExpr e@(I.Var  _ _) = return e
desugarExpr e@(I.Data _ _) = return e
desugarExpr e@(I.Lit  _ _) = return e
desugarExpr (I.App e1 e2 t) =
    I.App <$> desugarExpr e1 <*> desugarExpr e2 <*> do
        return t
desugarExpr (I.Let bins e t) =
    I.Let
        <$> (do
                let (bs, es) = unzip bins
                es' <- mapM desugarExpr es
                return $ zip bs es'
            )
        -- <$> mapM (Data.Bifunctor.second desugarExpr) bins
        <*> desugarExpr e
        <*> (do
                return t
            )
desugarExpr (I.Lambda b e t) = I.Lambda b <$> desugarExpr e <*> do
    return t
desugarExpr (I.Prim p es t) = I.Prim p <$> mapM desugarExpr es <*> do
    return t
desugarExpr (I.Match e arms t) = do
    arms' <- mapM
        (\(a, body) -> do
            body' <- desugarExpr body
            return (a, body')
        )
        arms
    let armsForDesugar = map (first (: [])) arms'
    -- case e of
    --     I.Var v t' -> desugarMatch [I.Var v t'] armsForDesugar (I.NoExpr t) -- should this be t?? considering put t back?
    --     _         -> do
    --         v <- I.VarId <$> freshVar
    -- singleLet v e <$> desugarMatch [I.Var v (I.extract e)] armsForDesugar (I.NoExpr t) <*> do return t-- INFO: for now, default expression is NoExpr

    desugarMatch [e] armsForDesugar (I.NoExpr t)
desugarExpr e@(I.NoExpr _) = return e

desugarMatch
    :: [I.Expr I.Type]
    -> [Equation]
    -> I.Expr I.Type
    -> DesugarFn (I.Expr I.Type)
desugarMatch [] [] def = case def of
    I.NoExpr _ -> error "can't happen" -- INFO: for now, can't handle inexhaustive patterns
    _          -> return def
desugarMatch []         (([], e) : _) _   = return e
desugarMatch []         _             _   = error "can't happen"
desugarMatch us@(_ : _) qs            def = do
    foldrM (desugarMatchGen us) def (partitionEqs qs)

desugarMatchGen
    :: [I.Expr I.Type]
    -> [Equation]
    -> I.Expr I.Type
    -> DesugarFn (I.Expr I.Type)
desugarMatchGen us qs def | isVarEq (head qs)  = desugarMatchVar us qs def
                          | isConsEq (head qs) = desugarMatchCons us qs def
                          | isLitEq (head qs)  = desugarMatchLit us qs def
                        --   | isTupEq (head qs)  = desugaratchTup us qs def
                          | isWildEq (head qs) = desugarMatchWild us qs def
                          | otherwise          = error "can't happen"
  where
    isWildEq (as, _) = case head as of
        I.AltDefault b -> isNothing b
        _              -> False
    isVarEq (as, _) = case head as of
        I.AltDefault b -> isJust b
        _              -> False
    isConsEq (as, _) = case head as of
        I.AltData _ _ -> True
        _             -> False
    isLitEq (as, _) = case head as of
        I.AltLit _ -> True
        _          -> False


desugarMatchVar
    :: [I.Expr I.Type]
    -> [Equation]
    -> I.Expr I.Type
    -> DesugarFn (I.Expr I.Type)
desugarMatchVar []       _  _   = error "can't happen"
desugarMatchVar (u : us) qs def = desugarMatch
    us
    [ (ps, singleAlias v u e) | (I.AltDefault v : ps, e) <- qs ]
    def

{-
To make life easier: transform PatWildcard into variable PatId
-}
desugarMatchWild
    :: [I.Expr I.Type]
    -> [Equation]
    -> I.Expr I.Type
    -> DesugarFn (I.Expr I.Type)
desugarMatchWild [] _ _ = error "can't happen"
desugarMatchWild (_ : us) qs def =
    desugarMatch us [ (ps, e) | (_ : ps, e) <- qs ] def

-- desugarMatchWild :: [Equation] -> DesugarFn [Equation]
-- desugarMatchWild = mapM desugar
--  where
--   desugar (A.PatWildcard : rs, v) = do
--     i <- freshVar
--     return (A.PatId i : rs, v)
--   desugar eq = return eq

{-
Assume only having PatApp
-}
desugarMatchCons
    :: [I.Expr I.Type]
    -> [Equation]
    -> I.Expr I.Type
    -> DesugarFn (I.Expr I.Type)
desugarMatchCons (u : us) qs def = do
    -- cs   <- getConstructors . getCon . head $ qs

    -- arms <- sequence [ desugarArm c (choose c) | c <- S.toList cs ]
    arms <- mapM desugarArm qs
    return $ I.Match u arms (I.extract def)
  where
    -- getCon ((A.PatApp (A.PatId i : _)) : _, _) = i
    -- getCon _ = error "can't happen"
    -- choose c = [ q | q <- qs, getCon q == c ]
    desugarArm qs' = do
        alt <- head (fst head qs')
        k   <- case alt of
            I.AltData _ lst -> lst
            _               -> error "cannot happen"
        us'  <- mapM (\i -> I.Var i t) (replicateM k freshVar)
        body <- desugarMatch (us' ++ us)
                             [ (ps' ++ ps, e) | ((_ : ps') : ps, e) <- qs' ]
                             def
        -- if k == 0
        --     then return (I.AltData , body)

        return (alt : map I.AltData us', body)
desugarMatchCons _ _ _ = error "can't happen"

{-
Simple pass for PatLit; no special desugaring
(Assuming that the patterns are exhaustive)
-}
desugarMatchLit
    :: [I.Expr I.Type]
    -> [Equation]
    -> I.Expr I.Type
    -> DesugarFn (I.Expr I.Type)
desugarMatchLit _ _ _ = error "TODO"
-- desugarMatchLit (u : us) qs def = do
--     arms <- sequence [ desugarArm p ps e | (p : ps, e) <- qs ]
--     let arms' = arms ++ [(A.PatWildcard, def)] -- WARN: assuming that PatLit is never exhaustive, adding a default case
--     return $ A.Match (A.Id u) arms'
--   where
--     desugarArm p ps e = do
--         body <- desugarMatch us [(ps, e)] def
--         return (p, body)
-- desugarMatchLit _ _ _ = error "can't happen"

{-
Similar to PatApp, but with only one kind of constructor
-}
-- desugarMatchTup :: [Identifier] -> [Equation] -> A.Expr -> DesugarFn A.Expr
-- desugarMatchTup (u : us) qs@((A.PatTup rs : _, _) : _) def = do
--     arm <- desugarArm
--     return $ A.Match (A.Id u) [arm]
--   where
--     desugarArm = do
--         let k = length rs
--         us'  <- replicateM k freshVar
--         body <- desugarMatch
--             (us' ++ us)
--             [ (ps' ++ ps, e) | (A.PatTup ps' : ps, e) <- qs ]
--             def
--         return (A.PatTup $ map A.PatId us', body)
-- desugarMatchTup _ _ _ = error "can't happen"

{-
To make life easier: transform constructor PatId into PatApp [PatId]
-}
-- desugarMatchIdCons :: [Equation] -> DesugarFn [Equation]
-- desugarMatchIdCons []       = return []
-- desugarMatchIdCons (x : xs) = do
--     xs' <- desugarMatchIdCons xs
--     let x' = desugar x
--     return (x' : xs')
--   where
--     desugar eq@(A.PatId i : rs, v) =
--         if isCons i then (A.PatApp [A.PatId i] : rs, v) else eq
--     desugar eq = eq


{-
Desugar PatAs and PatAnn into its actual pattern,
and propagate the 'as' or 'ann' part using let-bindings in the body
-}
-- desugarMatchAsAnn :: [Identifier] -> [Equation] -> DesugarFn [Equation]
-- desugarMatchAsAnn us@(u : _) (x : xs) = do
--     xs' <- desugarMatchAsAnn us xs
--     return (desugar x : xs')
--   where
--     desugar ((A.PatAs i r) : rs, v) = desugar (r : rs, singleAlias i u v)
--     desugar ((A.PatAnn t r) : rs, v) =
--         desugar (r : rs, singleAliasWithTyp u u t v)
--     desugar eq = eq
-- desugarMatchAsAnn _ _ = return []

partitionEqs :: [Equation] -> [[Equation]]
partitionEqs []  = []
partitionEqs [x] = [[x]]
partitionEqs (x : x' : xs) | sameGroup x x' = tack x (partitionEqs (x' : xs))
                           | otherwise      = [x] : partitionEqs (x' : xs)
  where
    tack y yss = (y : head yss) : tail yss
    sameGroup (as, _) (as', _) = case (head as, head as') of
        (I.AltLit _, I.AltLit _) -> True
        (I.AltDefault _, I.AltDefault _) -> True
        (I.AltData dc _, I.AltData dc' _) -> dc == dc'
        _ -> False


singleLet
    :: I.VarId -> I.Expr I.Type -> I.Expr I.Type -> I.Type -> I.Expr I.Type
singleLet i e = I.Let [(Just i, e)]

singleAlias :: I.Binder -> I.Expr I.Type -> I.Expr I.Type -> I.Expr I.Type
singleAlias alias i e = I.Let [(alias, i)] e (I.extract e) -- what's t here?

-- singleAlias :: Identifier -> Identifier -> A.Expr -> A.Expr
-- singleAlias alias i = A.Let [A.DefFn alias [] A.TypNone (A.Id i)]

-- singleAliasWithTyp :: Identifier -> Identifier -> A.Typ -> A.Expr -> A.Expr
-- singleAliasWithTyp alias i t =
--     A.Let [A.DefFn alias [] (A.TypProper t) (A.Id i)]

-- getConstructors :: Identifier -> DesugarFn (S.Set Identifier)
-- getConstructors i = do
--     c <- getCInfo i
--     t <- getTInfo (cType c)
--     return $ tCSet t

-- getArity :: Identifier -> DesugarFn Int
-- getArity i = do
--     c <- getCInfo i
--     return $ cArity c

-- getCInfo :: Identifier -> DesugarFn CInfo
-- getCInfo i = do
--     cm <- gets $ M.lookup i . consMap
--     maybe throwDesugarError return cm

-- getTInfo :: Identifier -> DesugarFn TInfo
-- getTInfo i = do
--     tm <- gets $ M.lookup i . typeMap
--     maybe throwDesugarError return tm

foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM _ d []       = return d
foldrM f d (x : xs) = f x =<< foldrM f d xs
