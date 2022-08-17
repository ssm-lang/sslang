{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
-- | Infer types for optionally annotated program.
--
-- For now, this module implements a very crude form of pure, syntax-directed type
-- inference where type annotations can be locally and immediately inferred,
-- without the help of a type environment/context.
module IR.TypeChecker where

import qualified Common.Compiler as Compiler
import Common.Identifiers (Binder)
import Control.Comonad (Comonad (..))
import Control.Monad.Except (MonadError (..))
import Control.Monad.State.Lazy
  ( MonadState,
    StateT (..),
    evalStateT,
    get,
    gets,
    modify,
  )
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.String (IsString (fromString))
import qualified IR.IR as I
import qualified IR.Types.Annotated as Ann
import qualified IR.Types.Classes as Classes
import IR.Types.TypeSystem
  ( Builtin (..),
    TypeDef (TypeDef),
    TypeVariant (VariantNamed, VariantUnnamed),
    targs,
    dearrow,
    deref,
    int,
    ref,
    tuple,
    unit,
    variants,
  )

-- | Typing Environment
data TypeCtx = TypeCtx
  { varMap :: M.Map I.VarId Classes.Type,
    dConMap :: M.Map I.DConId Classes.Type
  }

-- Inference Monad
newtype InferFn a = InferFn (StateT TypeCtx Compiler.Pass a)
  deriving (Functor) via (StateT TypeCtx Compiler.Pass)
  deriving (Applicative) via (StateT TypeCtx Compiler.Pass)
  deriving (Monad) via (StateT TypeCtx Compiler.Pass)
  deriving (MonadFail) via (StateT TypeCtx Compiler.Pass)
  deriving (MonadError Compiler.Error) via (StateT TypeCtx Compiler.Pass)
  deriving (MonadState TypeCtx) via (StateT TypeCtx Compiler.Pass)

-- | Run a InferFn computation.
runInferFn :: InferFn a -> Compiler.Pass a
runInferFn (InferFn m) =
  evalStateT
    m
    TypeCtx
      { varMap = M.empty,
        dConMap = M.empty
      }

-- | Insert a variable ID and its type into current typing context.
insertVar :: I.VarId -> Classes.Type -> InferFn ()
insertVar v t = modify $ \st -> st {varMap = M.insert v t $ varMap st}

-- | Replace the current typing context with a new one.
replaceCtx :: TypeCtx -> InferFn ()
replaceCtx ctx = modify $ const ctx

-- | Look up the type of a variable in the current typing context given its variable ID.
lookupVar :: I.VarId -> InferFn (Maybe Classes.Type)
lookupVar v = M.lookup v <$> gets varMap

-- | Look up the type of a data constructor in the typing context.
lookupDCon :: I.DConId -> InferFn (Maybe Classes.Type)
lookupDCon d = M.lookup d <$> gets dConMap

-- | Infer all the program defs of the given program.
inferProgram :: I.Program Ann.Type -> Compiler.Pass (I.Program Classes.Type)
inferProgram p = runInferFn $ do
  typeDefs' <- inferADT $ I.typeDefs p
  externs' <- inferExterns $ I.externDecls p
  defs' <- inferProgramDefs $ I.programDefs p
  return $ I.Program { I.programDefs = defs'
                     , I.typeDefs = typeDefs'
                     , I.externDecls = externs'
                     , I.programEntry = I.programEntry p
                     , I.cDefs = I.cDefs p 
                     }

{-| Pass all program typeDefs through the typechecker

Change Ann.Type to Classes.Type
For each ADT, save ('DConId, 'TConId) key-value pairs in typing context for future use
-}
inferADT :: [(I.TConId, TypeDef Ann.Type)] -> InferFn [(I.TConId, TypeDef Classes.Type)]
inferADT [] = pure []
inferADT adts@((tconId, h) : tl) = do
  mapM_ insertADT adts -- save ADTs in typing context
  h' <- inferTypeDef h
  tl' <- inferADT tl
  return ((tconId, h') : tl')
  where
    insertADT :: (I.TConId, TypeDef Ann.Type) -> InferFn ()
    insertADT (tcon, TypeDef {variants = vars}) = mapM_ (insertDCon tcon.fst) vars
      where
      insertDCon :: I.TConId -> I.DConId ->  InferFn ()
      insertDCon t d = modify $ \st -> st {dConMap = M.insert d (Classes.TCon t []) $ dConMap st}
    inferTypeDef :: TypeDef Ann.Type -> InferFn (TypeDef Classes.Type)
    inferTypeDef TypeDef {variants = vars, targs = ar} =
      case vars of
        [] -> pure TypeDef {variants = [], targs = ar}
        ((dconId, h2) : tl2) -> do
          h2' <- inferTypeVariant h2
          TypeDef {variants = tl2'} <- inferTypeDef TypeDef {variants = tl2, targs = ar}
          return TypeDef {variants = (dconId, h2') : tl2', targs = ar}
    inferTypeVariant :: TypeVariant Ann.Type -> InferFn (TypeVariant Classes.Type)
    inferTypeVariant (VariantUnnamed []) = do return (VariantUnnamed [])
    inferTypeVariant (VariantUnnamed (h3 : tl3)) = do
      h3' <- anns2Class h3
      (VariantUnnamed tl3') <- inferTypeVariant (VariantUnnamed tl3)
      return (VariantUnnamed (h3' : tl3'))
    inferTypeVariant (VariantNamed []) = do return (VariantNamed [])
    inferTypeVariant (VariantNamed [(varId, t)]) = do
      t' <- anns2Class t
      return (VariantNamed [(varId, t')])
    inferTypeVariant (VariantNamed ((varId, h4) : tl4)) = do
      h4' <- anns2Class h4
      (VariantNamed tl4') <- inferTypeVariant (VariantNamed tl4)
      return (VariantNamed ((varId, h4') : tl4'))

inferExterns :: [(I.VarId, Ann.Type)] -> InferFn [(I.VarId, Classes.Type)]
inferExterns = mapM inferExtern
  where inferExtern (i, t) = do
          t' <- anns2Class t
          insertVar i t'
          return (i, t')

-- | Top level inference.
--
-- This will infer the (VarId, Expr) pair sequentially and add the infered types
-- into the type context so that these info can be used when infering the following
-- expressions.
inferProgramDefs :: [(I.VarId, I.Expr Ann.Type)] -> InferFn [(I.VarId, I.Expr Classes.Type)]
inferProgramDefs [] = return []
inferProgramDefs ((v, e):xs) = do
  e' <- inferExpr e
  insertVar v (extract e')
  xs' <- inferProgramDefs xs
  return $ (v, e'):xs'

-- | Infer a single expression @e@.
--
-- This function will also infer any nested expressions inside @e@ and all
-- expressions in the return value will have Classes.Type as its type. Error will
-- be thrown if the type annotation is in inconsistent with the true true or the
-- expression is known.
inferExpr :: I.Expr Ann.Type -> InferFn (I.Expr Classes.Type)
inferExpr (I.Var v t) = do
  record <- lookupVar v
  t' <- anns2Class t
  case record of
    Nothing -> do
      insertVar v t'
      return $ I.Var v t'
    Just t'' | t'' == t' -> return $ I.Var v t'
    Just t'' -> throwError $ Compiler.TypeError $ fromString $
      "Var expression has inconsistent type annotaation: " ++ show t' ++ ", expected " ++ show t''
inferExpr e@(I.Lambda v b t) = do
  b' <- withNewScope $ withVty e v t >> inferExpr b
  t' <- anns2Class t
  return $ I.Lambda v b' t'
inferExpr (I.Let vs b _) = do
  (vs', b') <- withNewScope $ localInferExpr vs b
  return $ I.Let vs' b' (extract b')
  where
    localInferExpr bs body = do
      bs' <- mapM fn bs
      body' <- inferExpr body
      return (bs', body')
      where
        fn (binding, e) = do
          e' <- inferExpr e
          case binding of
            Just vid -> insertVar vid (extract e')
            Nothing -> return ()
          return (binding, e')
inferExpr e@I.Prim {} = inferPrim e
inferExpr (I.Lit I.LitEvent _) = return $ I.Lit I.LitEvent unit
inferExpr (I.Lit i@(I.LitIntegral _) _) = return $ I.Lit i (int 32)
inferExpr e@(I.Data d _) = do
  tcon <- lookupDCon d
  case tcon of
    Nothing -> throwError $ Compiler.TypeError $ fromString $ "Unable to type ADT expression: " ++ show e
    Just t' -> return $ I.Data d t'
inferExpr (I.App a@(I.Data _ _) b _) = do
    a' <- inferExpr a
    b' <- inferExpr b
    return $ I.App a' b' (extract a')
inferExpr e@(I.App a b _) = do
  a' <- inferExpr a
  b' <- inferExpr b
  case dearrow $ extract a' of
    Just (t1, t2) ->
      if t1 == extract a'
        then throwError $ Compiler.TypeError $ fromString $ "App expression has inconsistent type: " ++ show e
        else return $ I.App a' b' t2
    Nothing -> throwError $ Compiler.TypeError $ fromString $ "Unable to type App expression: " ++ show e
inferExpr e = throwError $ Compiler.TypeError $ fromString $ "Unable to type unknown expression: " ++ show e

-- | Infer the type of a Primitive expression.
inferPrim :: I.Expr Ann.Type -> InferFn (I.Expr Classes.Type)
inferPrim (I.Prim I.New [e] _) = do
  e' <- inferExpr e
  return $ I.Prim I.New [e'] $ ref $ extract e'
inferPrim (I.Prim I.Deref [e] _) = do
  e' <- inferExpr e
  return $ I.Prim I.Deref [e'] $ fromJust $ deref $ extract e'
inferPrim (I.Prim I.Dup [e] _) = do
  e' <- inferExpr e
  return $ I.Prim I.Dup [e'] $ extract e'
inferPrim e@(I.Prim I.Assign [lhs, rhs] _) = do
  lhs' <- inferExpr lhs
  rhs' <- inferExpr rhs
  let rrty = ref $ extract rhs'
  if extract lhs' == rrty
    then do
      case lhs' of
        I.Var v _ -> insertVar v rrty
        _ -> return ()
      return $ I.Prim I.Assign [rrty <$ lhs', rhs'] unit
    else throwError $ Compiler.TypeError $ fromString $ "Assign expression has inconsistent type: " ++ show e
inferPrim e@(I.Prim I.After [del, lhs, rhs] _) = do
  del' <- inferExpr del
  rhs' <- inferExpr rhs
  lhs' <- inferExpr lhs
  let rrty = ref $ extract rhs'
  if extract del' == int 32 && extract lhs' == rrty
    then do
      case lhs' of
        I.Var v _ -> insertVar v rrty
        _ -> return ()
      return $ I.Prim I.After [(\_ -> int 32) <$> del', rrty <$ lhs', rhs'] unit
    else throwError $ Compiler.TypeError $ fromString $ "After expression has inconsistent type: " ++ show e
inferPrim (I.Prim (I.PrimOp o@I.PrimAdd) [e1, e2] _) = inferPrimBinop o e1 e2
inferPrim (I.Prim (I.PrimOp o@I.PrimSub) [e1, e2] _) = inferPrimBinop o e1 e2
inferPrim (I.Prim (I.PrimOp o@I.PrimMul) [e1, e2] _) = inferPrimBinop o e1 e2
inferPrim (I.Prim (I.PrimOp o@I.PrimDiv) [e1, e2] _) = inferPrimBinop o e1 e2
inferPrim (I.Prim (I.PrimOp o@I.PrimMod) [e1, e2] _) = inferPrimBinop o e1 e2
inferPrim (I.Prim (I.PrimOp o@I.PrimEq) [e1, e2] _) = inferPrimBinop o e1 e2
inferPrim (I.Prim (I.PrimOp o@I.PrimNeq) [e1, e2] _) = inferPrimBinop o e1 e2
inferPrim (I.Prim (I.PrimOp o@I.PrimGt) [e1, e2] _) = inferPrimBinop o e1 e2
inferPrim (I.Prim (I.PrimOp o@I.PrimLt) [e1, e2] _) = inferPrimBinop o e1 e2
inferPrim (I.Prim (I.PrimOp o@I.PrimGe) [e1, e2] _) = inferPrimBinop o e1 e2
inferPrim (I.Prim (I.PrimOp o@I.PrimLe) [e1, e2] _) = inferPrimBinop o e1 e2
inferPrim (I.Prim I.Break [] _) = return $ I.Prim I.Break [] unit
inferPrim (I.Prim I.Now [] _) = return $ I.Prim I.Now [] $ int 32 -- TODO: make this timestamp type
inferPrim (I.Prim I.Loop es _) = do
  es' <- mapM inferExpr es
  return $ I.Prim I.Loop es' unit
inferPrim (I.Prim I.Wait es _) = do
  es' <- mapM inferExpr es
  return $ I.Prim I.Wait es' unit
inferPrim (I.Prim I.Par es _) = do
  es' <- mapM inferExpr es
  let ts = map extract es'
  return $ I.Prim I.Par es' $ tuple ts
inferPrim (I.Prim c@(I.CQuote _) [] _) = do
  return $ I.Prim c [] $ Classes.TVar $ fromString "cquote"
inferPrim (I.Prim c@(I.CCall _) es _) = do
  es' <- mapM inferExpr es
  return $ I.Prim c es' unit
inferPrim e = throwError $ Compiler.TypeError $ fromString $ "Unable to type Prim expression: " ++ show e

-- | Check type of a binary integer operation.
inferPrimBinop :: I.PrimOp -> I.Expr Ann.Type -> I.Expr Ann.Type -> InferFn (I.Expr Classes.Type)
inferPrimBinop o e1 e2 = do
  e1' <- inferExpr e1
  e2' <- inferExpr e2
  return $ I.Prim (I.PrimOp o) [e1', e2'] $ int 32

-- | Helper function to support local modificaton of type context.
withNewScope :: InferFn a -> InferFn a
withNewScope inf = do
  ctx <- get
  x <- inf
  modify $ const ctx
  return x

-- | Helper function to do type inference with the knowledge of the correct type for a binding.
withVty :: I.Expr Ann.Type -> Binder -> Ann.Type -> InferFn ()
withVty _ Nothing _ = return ()
withVty e (Just v) t =
  case dearrow t of
    Just (vty, _) -> do
      vty' <- anns2Class vty
      insertVar v vty'
    Nothing -> throwError $ Compiler.TypeError $ fromString $ "Lambda wasn't annotated (or it wasn't an arrow type): " ++ show e

-- | Transfrom an Ann.Type to Classes.Type.
anns2Class :: Ann.Type -> InferFn Classes.Type
anns2Class (Ann.Type []) = throwError $ Compiler.TypeError $ fromString "Cannot change empty Ann type to Classes type"
anns2Class (Ann.Type ts) = ann2Class $ head ts

{- Transfrom an Ann.TypeAnnote to Classes.Type.

Since we are doing this in a monadic way, and Builtin is also a Functor, we have
to do explicit pattern matching on the `Ann.TBuiltin bty` case to transform
`Builtin Ann.Type` to `Builtin Classes.Type`.
-}
ann2Class :: Ann.TypeAnnote -> InferFn Classes.Type
ann2Class (Ann.TBuiltin bty) = case bty of
  Unit -> return $ Classes.TBuiltin Unit
  Void -> return $ Classes.TBuiltin Void
  Integral s -> return $ Classes.TBuiltin $ Integral s
  Arrow l r -> do
    l' <- anns2Class l
    r' <- anns2Class r
    return $ Classes.TBuiltin $ Arrow l' r'
  Ref t -> do
    t' <- anns2Class t
    return $ Classes.TBuiltin $ Ref t'
  Tuple tys -> do
    tys' <- mapM anns2Class tys
    return $ Classes.TBuiltin $ Tuple tys'
ann2Class (Ann.TCon tid tys) = do
  t <- mapM anns2Class tys
  return $ Classes.TCon tid t
ann2Class (Ann.TVar _tid) = throwError $ Compiler.UnexpectedError $ fromString "Do not know how to translate type variable yet"
