{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module IR.Constraint.Constrain.Expression where

import qualified Common.Identifiers as Ident
import Control.Monad (forM, zipWithM)
import Data.Foldable (foldrM)
import qualified Data.Map.Strict as Map
import IR.Constraint.Canonical ((-->))
import qualified IR.Constraint.Canonical as Can
import qualified IR.Constraint.Constrain.Annotation as Ann
import qualified IR.Constraint.Constrain.Pattern as Pattern
import IR.Constraint.Monad (TC, freshAnnVar, getExtern, throwError)
import IR.Constraint.Type as Type
import qualified IR.IR as I
import IR.SegmentLets (segmentDefs')


-- | HELPER ALIASES
type Def = (I.VarId, [I.Annotation], Variable, I.Expr Attachment)


type BinderDef = (I.Binder Attachment, I.Expr Attachment)


-- | DEFS
constrainBinderDefs :: [BinderDef] -> Constraint -> TC Constraint
constrainBinderDefs binderDefs finalConstraint = do
  defs <- forM binderDefs \(binder, expr) -> do
    -- TODO: these binders will also contain annotations; we need to do
    -- something with them!

    -- resolved I think?
    name <- Type.binderToVarId binder
    let (anns, uvar) = uncarryAttachment binder
    return (name, anns, uvar, expr)
  constrainDefs defs finalConstraint


constrainDefs :: [Def] -> Constraint -> TC Constraint
constrainDefs defs finalConstraint = do
  let segments = segmentDefs' defs
  foldrM constrainRecDefs finalConstraint segments


constrainRecDefs :: [Def] -> Constraint -> TC Constraint
constrainRecDefs defs finalConstraint = do
  let flexVars = [uvar | (_, _, uvar, _) <- defs]
      flexHeader = Map.fromList [(Ident.fromId name, TVarN uvar) | (name, _, uvar, _) <- defs]
  headerConstraints <- forM defs \(_, anns, uvar, e) -> do
    Ann.withAnnotations anns (TVarN uvar) $ constrainExprAttached e
  if shouldGeneralize
    then
      return $
        CLet
          []
          flexVars
          flexHeader
          (CLet [] [] flexHeader CTrue (CAnd headerConstraints))
          finalConstraint
    else
      return $
        exists flexVars $
          CLet
            []
            []
            flexHeader
            (CLet [] [] flexHeader CTrue (CAnd headerConstraints))
            finalConstraint
 where
  shouldGeneralize = all (\(_, _, _, e) -> I.isValue e) defs -- see concept: value restriction


-- | CONSTRAIN EXPRESSIONS
constrainExprAttached :: I.Expr Attachment -> Type -> TC Constraint
constrainExprAttached expr expected = do
  let (anns, u) = uncarryAttachment expr
  constraint <- Ann.withAnnotations anns expected $ constrainExpr expr
  return $ exists [u] $ CAnd [CEqual (TVarN u) expected, constraint]


constrainExprAnnotated
  :: I.Expr Attachment -> [Can.Annotation] -> Type -> TC Constraint
constrainExprAnnotated expr [] expected = constrainExpr expr expected
constrainExprAnnotated expr (Can.AnnDCon _ _ : anns) expected =
  constrainExprAnnotated expr anns expected
constrainExprAnnotated expr (ann : anns) expected = do
  dummyId <- Ident.fromId <$> freshAnnVar
  (Ann.State rigidMap flexs, tipe) <- Ann.add ann
  innerConstraint <- constrainExprAnnotated expr anns tipe
  return $
    CLet
      { _rigidVars = Map.elems rigidMap
      , _flexVars = flexs
      , _header = Map.singleton dummyId tipe
      , _headerCon = innerConstraint
      , _bodyCon = CLocal dummyId expected
      }


constrainExpr :: I.Expr Attachment -> Type -> TC Constraint
constrainExpr expr expected = do
  case expr of
    I.Var name _ -> return $ CLocal (Ident.fromId name) expected
    I.Data name _ -> return $ CLocal (Ident.fromId name) expected
    I.Lit lit _ -> constrainLit lit expected
    I.App func arg _ -> constrainApp func arg expected
    I.Lambda binder body _ -> constrainLambda binder body expected
    I.Let binderDefs body _ -> do
      constrainBinderDefs binderDefs =<< constrainExprAttached body expected
    I.Match e branches _ -> constrainMatch e branches expected
    I.Prim prim args _ -> constrainPrim prim args expected
    I.Exception _ _ ->
      let scheme = Can.schemeOf $ Can.TVar "a"
       in return $ CForeign scheme expected


constrainLit :: I.Literal -> Type -> TC Constraint
constrainLit (I.LitIntegral _) expected = return $ CEqual Type.i32 expected
constrainLit I.LitEvent expected = return $ CEqual Type.unit expected


constrainApp :: I.Expr Attachment -> I.Expr Attachment -> Type -> TC Constraint
constrainApp func arg resultType = do
  argVar <- mkFlexVar
  let argType = TVarN argVar
      funcType = argType ==> resultType
  funcCon <- constrainExprAttached func funcType
  argCon <- constrainExprAttached arg argType
  return $ exists [argVar] (CAnd [funcCon, argCon])


constrainLambda :: I.Binder Attachment -> I.Expr Attachment -> Type -> TC Constraint
constrainLambda binder body expected = do
  argName <- binderToVarId binder
  resultVar <- mkFlexVar
  let (anns, argVar) = uncarryAttachment binder
      resultType = TVarN resultVar

  exists [argVar, resultVar] <$> Ann.withAnnotations anns (TVarN argVar) \argExpected -> do
    bodyCon <- constrainExprAttached body resultType

    return $
      CAnd
        [ CLet [] [] (Map.singleton (Ident.fromId argName) argExpected) CTrue bodyCon
        , CEqual (argExpected ==> resultType) expected
        ]


constrainMatch
  :: I.Expr Attachment -> [(I.Alt Attachment, I.Expr Attachment)] -> Type -> TC Constraint
constrainMatch expr branches expected = do
  exprVar <- mkFlexVar
  let exprType = TVarN exprVar
  exprCon <- constrainExprAttached expr exprType

  branchCons <- forM branches \(alt, branchExpr) -> constrainBranch alt branchExpr exprType expected

  return $ exists [exprVar] $ CAnd [exprCon, CAnd branchCons]


constrainBranch :: I.Alt Attachment -> I.Expr Attachment -> Type -> Type -> TC Constraint
constrainBranch alt expr pExpect bExpect = do
  (Pattern.State headers pvars revCons) <- Pattern.add alt pExpect Pattern.emptyState
  CLet [] pvars headers (CAnd (reverse revCons))
    <$> constrainExprAttached expr bExpect


constrainPrim :: I.Primitive -> [I.Expr Attachment] -> Type -> TC Constraint
constrainPrim prim args expected = do
  argVars <- mapM (const mkFlexVar) args
  let argTypes = map TVarN argVars
  argCons <- zipWithM constrainExprAttached args argTypes
  let fromArgs = foldArrow (argTypes, expected)
  fromPrim <- lookupPrim (length args) prim
  return $ exists argVars $ CAnd (CForeign fromPrim fromArgs : argCons)


lookupPrim :: Int -> I.Primitive -> TC Can.Scheme
lookupPrim len prim = do
  Can.schemeOf <$> primType
 where
  primType :: TC Can.Type
  primType = case prim of
    I.New -> return $ Can.TVar "a" --> Can.Ref (Can.TVar "a")
    I.Dup -> return $ Can.TVar "a" --> Can.TVar "a"
    I.Drop -> return $ Can.TVar "a" --> Can.Unit
    I.Deref -> return $ Can.Ref (Can.TVar "a") --> Can.TVar "a"
    I.Assign -> return $ Can.Ref (Can.TVar "a") --> Can.TVar "a" --> Can.Unit
    I.After ->
      return $ Can.I32 --> Can.Ref (Can.TVar "a") --> Can.TVar "a" --> Can.Unit
    I.Now -> return Can.U64
    I.Last -> return $ Can.Ref (Can.TVar "a") --> Can.U64
    I.CQuote _ -> return $ Can.TVar "a"
    I.Loop -> return $ Can.TVar "a" --> Can.Unit
    I.Break -> return Can.Unit
    I.FfiCall name -> do
      maybeType <- getExtern name
      case maybeType of
        Just canType -> return canType
        Nothing -> throwError $ "Unknown ffi call: " ++ show name
    I.CCall _ -> return $ Can.TVar "a"
    I.PrimOp primOp -> return $ primOpType primOp
    I.Par ->
      let tvs = take len $ map (("a" <>) . Ident.showId) [(1 :: Int) ..]
          args = map Can.TVar tvs
          ret = Can.tuple args
       in return $ Can.foldArrow (args, ret)
    I.Wait ->
      let tvs = take len $ map (("a" <>) . Ident.showId) [(1 :: Int) ..]
          args = map Can.TVar tvs
          ret = Can.Unit
       in return $ Can.foldArrow (args, ret)

  primOpType :: I.PrimOp -> Can.Type
  primOpType primOp = case primOp of
    I.PrimNeg -> Can.I32 --> Can.I32
    I.PrimNot -> Can.I32 --> Can.I32
    I.PrimBitNot -> Can.I32 --> Can.I32
    I.PrimAdd -> Can.I32 --> Can.I32 --> Can.I32
    I.PrimSub -> Can.I32 --> Can.I32 --> Can.I32
    I.PrimMul -> Can.I32 --> Can.I32 --> Can.I32
    I.PrimDiv -> Can.I32 --> Can.I32 --> Can.I32
    I.PrimMod -> Can.I32 --> Can.I32 --> Can.I32
    I.PrimBitAnd -> Can.I32 --> Can.I32 --> Can.I32
    I.PrimBitOr -> Can.I32 --> Can.I32 --> Can.I32
    I.PrimEq -> Can.I32 --> Can.I32 --> Can.I32
    I.PrimNeq -> Can.I32 --> Can.I32 --> Can.I32
    I.PrimGt -> Can.I32 --> Can.I32 --> Can.I32
    I.PrimGe -> Can.I32 --> Can.I32 --> Can.I32
    I.PrimLt -> Can.I32 --> Can.I32 --> Can.I32
    I.PrimLe -> Can.I32 --> Can.I32 --> Can.I32
