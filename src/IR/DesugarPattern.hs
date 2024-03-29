{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module IR.DesugarPattern (
  desugarPattern,
) where

import Common.Compiler as Compiler (
  Error (PatternError),
  MonadError (throwError),
  Pass,
  fromString,
 )
import qualified IR.IR as I
import qualified IR.MangleNames as I

import Common.Pretty (Pretty (..))
import Control.Monad (forM)
import Control.Monad.State.Lazy (
  MonadState,
  StateT (..),
  gets,
  modify,
 )
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (foldrM)
import qualified Data.Map as M
import qualified Data.Set as S


type Equation = ([I.Alt I.Type], I.Expr I.Type)


data CInfo = CInfo
  { cName :: I.DConId
  , cType :: I.TConId
  , argsType :: [I.Type]
  }
  deriving (Eq, Show)


data TInfo = TInfo
  { tName :: I.TConId
  , tCSet :: S.Set I.DConId
  }
  deriving (Eq, Show)


data DesugarCtx = DesugarCtx
  { typeMap :: M.Map I.TConId TInfo
  , consMap :: M.Map I.DConId CInfo
  , symTable :: I.SymTable I.Type
  }


newtype DesugarFn a = DesugarFn (StateT DesugarCtx Compiler.Pass a)
  deriving (Functor) via (StateT DesugarCtx Compiler.Pass)
  deriving (Applicative) via (StateT DesugarCtx Compiler.Pass)
  deriving (Monad) via (StateT DesugarCtx Compiler.Pass)
  deriving (MonadFail) via (StateT DesugarCtx Compiler.Pass)
  deriving (MonadError Error) via (StateT DesugarCtx Compiler.Pass)
  deriving (MonadState DesugarCtx) via (StateT DesugarCtx Compiler.Pass)


unDesugarFn :: DesugarFn a -> StateT DesugarCtx Compiler.Pass a
unDesugarFn (DesugarFn a) = a


freshVar :: I.Type -> DesugarFn I.VarId
freshVar t = do
  syms <- gets symTable
  let origin = "__anonymous_pattern"
      name = I.pickId syms origin
      syms' = M.insert name I.SymInfo{I.symOrigin = origin, I.symType = t} syms
  modify $ \ctx -> ctx{symTable = syms'}
  return name


-- TODO: this should be a pattern synonym
unreachableExpr :: I.Type -> I.Expr I.Type
unreachableExpr = I.Exception $ I.ExceptDefault $ I.LitIntegral 0


desugarPattern :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
desugarPattern p@I.Program{I.programDefs = defs, I.typeDefs = tds, I.symTable = syms} = do
  (defs', symTable -> syms') <- runStateT (unDesugarFn $ mapM desugarExprsDefs defs) initCtx
  return $ p{I.programDefs = defs', I.symTable = syms'}
 where
  desugarExprsDefs (vs, es) = (vs,) <$> desugarExpr es
  initCtx = DesugarCtx{typeMap = buildTypeMap tds, consMap = buildConsMap tds, symTable = syms}


desugarExpr :: I.Expr I.Type -> DesugarFn (I.Expr I.Type)
desugarExpr (I.App e1 e2 t) = I.App <$> desugarExpr e1 <*> desugarExpr e2 <*> pure t
desugarExpr (I.Let (unzip -> (bs, es)) e t) =
  I.Let <$> (zip bs <$> mapM desugarExpr es) <*> desugarExpr e <*> pure t
desugarExpr (I.Lambda b e t) = I.Lambda b <$> desugarExpr e <*> pure t
desugarExpr (I.Prim p es t) = I.Prim p <$> mapM desugarExpr es <*> pure t
desugarExpr (I.Match e arms t) = do
  eqns <- forM arms $ \(a, body) -> do
    -- Generate an 'Equation' for each arm
    body' <- desugarExpr body
    return ([a], body')
  case e of
    I.Var _ _ -> desugarMatch [e] eqns (unreachableExpr t) -- TODO: add let alias
    _ -> do
      -- Bind scrutinee to a variable before threading it through desugarMatch
      let et = I.extract e
      var <- freshVar et
      I.Let [(I.BindVar var et, e)]
        <$> desugarMatch [I.Var var et] eqns (unreachableExpr t)
        <*> pure et
desugarExpr e = return e


desugarMatch ::
  [I.Expr I.Type] ->
  [Equation] ->
  I.Expr I.Type -> -- Default expression the 'I.Match' should return
  DesugarFn (I.Expr I.Type)
desugarMatch [] [] def = return def
desugarMatch [] (([], e) : _) _ = return e
desugarMatch [] _ _ = error "can't happen 1"
desugarMatch us eqs def = foldrM desugarMatchGen def $ partitionEqs eqs
 where
  desugarMatchGen qs@(q : _)
    | isVarEq q = desugarMatchVar us qs
    | isConsEq q = desugarMatchCons us qs
    | isLitEq q = desugarMatchLit us qs
    | isWildEq q = desugarMatchWild us qs
  desugarMatchGen _ = error "can't happen 2"

  partitionEqs :: [Equation] -> [[Equation]]
  partitionEqs [] = []
  partitionEqs [x] = [[x]]
  partitionEqs (x : x' : xs)
    | sameGroup x x' = tack x (partitionEqs (x' : xs))
    | otherwise = [x] : partitionEqs (x' : xs)

  tack y yss = (y : head yss) : tail yss

  sameGroup (I.AltLit{} : _, _) (I.AltLit{} : _, _) = True
  sameGroup (I.AltBinder{} : _, _) (I.AltBinder{} : _, _) = True
  sameGroup (I.AltData{} : _, _) (I.AltData{} : _, _) = True
  sameGroup _ _ = False

  isWildEq (I.AltBinder I.BindAnon{} : _, _) = True
  isWildEq _ = False
  isVarEq (I.AltBinder I.BindVar{} : _, _) = True
  isVarEq _ = False
  isConsEq (I.AltData{} : _, _) = True
  isConsEq _ = False
  isLitEq (I.AltLit{} : _, _) = True
  isLitEq _ = False


desugarMatchVar :: [I.Expr I.Type] -> [Equation] -> I.Expr I.Type -> DesugarFn (I.Expr I.Type)
desugarMatchVar (u : us) qs def =
  -- Create a let-bound alias for 'u' named 'v' in each equation
  let qs' = [(ps, I.Let [(v, u)] e $ I.extract e) | (I.AltBinder v : ps, e) <- qs]
   in desugarMatch us qs' def
desugarMatchVar [] _ _ = error "can't happen 3"


desugarMatchWild :: [I.Expr I.Type] -> [Equation] -> I.Expr I.Type -> DesugarFn (I.Expr I.Type)
desugarMatchWild (_ : us) qs def =
  -- Discard first 'I.Alt' from each equation
  let qs' = [(ps, e) | (_ : ps, e) <- qs]
   in desugarMatch us qs' def
desugarMatchWild [] _ _ = error "can't happen 4"


desugarMatchCons :: [I.Expr I.Type] -> [Equation] -> I.Expr I.Type -> DesugarFn (I.Expr I.Type)
desugarMatchCons (u : us) qs@(q : _) def = do
  cs <- getConstructors $ getCon q
  arms <- sequence [desugarArm dcon (getTyp q) (sameConsAs dcon) | dcon <- S.toList cs]
  return $ I.Match u arms (I.extract def)
 where
  getCon ((I.AltData dcon _ _) : _, _) = dcon
  getCon _ = error "can't happen 5"
  getTyp ((I.AltData _ _ t) : _, _) = t
  getTyp _ = error "no no no 555"

  sameConsAs c = filter ((== c) . getCon) qs

  desugarArm :: I.DConId -> I.Type -> [Equation] -> DesugarFn (I.Alt I.Type, I.Expr I.Type)
  desugarArm dcon dconTyp qs' = do
    argsTyps <- argsType <$> getCInfo dcon

    (unzip -> (bs', us')) <- forM argsTyps $ \argTyp -> do
      name <- freshVar argTyp
      return (I.AltBinder $ I.BindVar name argTyp, I.Var name argTyp)

    body <-
      if null qs'
        then do
          -- We're done desugaring this equation, just use default body
          return def
        else do
          -- Recursively generate body from remaining equations
          let qs'' = [(as' ++ as, e) | ((I.AltData _ as' _) : as, e) <- qs']
          desugarMatch (us' ++ us) qs'' def

    return (I.AltData dcon bs' dconTyp, body)
desugarMatchCons _ _ _ = error "can't happen 7"


desugarMatchLit :: [I.Expr I.Type] -> [Equation] -> I.Expr I.Type -> DesugarFn (I.Expr I.Type)
desugarMatchLit (u : us) qs def = do
  arms <- sequence [(a,) <$> desugarMatch us [(as, e)] def | (a : as, e) <- qs]
  let defAlt = I.AltBinder $ I.BindAnon $ I.extract u
  return $ I.Match u (arms ++ [(defAlt, def)]) (I.extract def)
--                            ^^^^^^^^^^^
-- WARN: we assume that PatLit is never exhaustive, so we add a default case
desugarMatchLit _ _ _ = error "can't happen 8"


getConstructors :: I.DConId -> DesugarFn (S.Set I.DConId)
getConstructors dcon = do
  c <- getCInfo dcon
  t <- getTInfo (cType c)
  return $ tCSet t


getCInfo :: I.DConId -> DesugarFn CInfo
getCInfo dcon = gets (M.lookup dcon . consMap) >>= maybe (desugarError dcon) return


getTInfo :: I.TConId -> DesugarFn TInfo
getTInfo tcon = gets (M.lookup tcon . typeMap) >>= maybe (desugarError tcon) return


desugarError :: Pretty i => i -> DesugarFn a
desugarError i = throwError $ PatternError $ "Unknown identifier: " <> fromString (show $ pretty i)


buildTypeMap :: [(I.TConId, I.TypeDef)] -> M.Map I.TConId TInfo
buildTypeMap = foldr build M.empty
 where
  build (tcon, map fst . I.variants -> dcons) =
    M.insert tcon (TInfo{tName = tcon, tCSet = S.fromList dcons})


buildConsMap :: [(I.TConId, I.TypeDef)] -> M.Map I.DConId CInfo
buildConsMap = foldr (build . second I.variants) M.empty
 where
  variantTypes (I.VariantUnnamed typs) = typs
  variantTypes _ = error "VariantNamed aren't supported (yet?)"
  build (tcon, variants) m = foldr (build' tcon) m variants
  build' tcon (dcon, typs) =
    M.insert dcon $
      CInfo
        { cName = dcon
        , cType = tcon
        , argsType = variantTypes typs
        }
