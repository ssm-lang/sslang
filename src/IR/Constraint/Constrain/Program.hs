{-# LANGUAGE TupleSections #-}
module IR.Constraint.Constrain.Program where

import qualified Common.Identifiers as Ident
import Data.Foldable (foldrM)
import qualified Data.Map.Strict as Map
import qualified IR.Constraint.Canonical as Can
import qualified IR.Constraint.Constrain.Expression as Expr
import IR.Constraint.Monad (TC)
import IR.Constraint.Type
import qualified IR.IR as I


constrain :: I.Program (Can.Annotations, Variable) -> TC Constraint
constrain prog = do
  constrainTypeDefs (I.typeDefs prog)
    =<< constrainExternDecls (I.externDecls prog)
    =<< Expr.constrainBinderDefs (I.programDefs prog) CTrue


-- | EXTERN DECLS

-- TODO: do I still need this? depends on whether these are only used in fficall prim
constrainExternDecls ::
  [(Ident.VarId, Can.Type)] -> Constraint -> TC Constraint
constrainExternDecls decls finalConstraint =
  foldrM constrainExternDecl finalConstraint decls


constrainExternDecl :: (Ident.VarId, Can.Type) -> Constraint -> TC Constraint
constrainExternDecl (varId, canTyp) finalConstraint = do
  constrainDeclaration
    (Ident.fromId varId)
    (Can.schemeOf canTyp)
    finalConstraint


-- | TYPE DEFS
constrainTypeDefs :: [(Ident.TConId, I.TypeDef)] -> Constraint -> TC Constraint
constrainTypeDefs tdefs finalConstraint =
  foldrM
    ( \(tcon, typeDef) innerConstraint ->
        constrainTypeDef tcon typeDef innerConstraint
    )
    finalConstraint
    tdefs


constrainTypeDef :: Ident.TConId -> I.TypeDef -> Constraint -> TC Constraint
constrainTypeDef tcon (I.TypeDef variants args) finalConstraint =
  foldrM
    ( \(dcon, variant) innerConstraint ->
        constrainVariant tcon args dcon variant innerConstraint
    )
    finalConstraint
    variants


constrainVariant ::
  Ident.TConId ->
  [Ident.TVarId] ->
  Ident.DConId ->
  I.TypeVariant ->
  Constraint ->
  TC Constraint
constrainVariant tcon targs dcon (I.VariantUnnamed dargs) finalConstraint = do
  let typ = Can.foldArrow (dargs, Can.TCon tcon (map Can.TVar targs))
      scheme = Can.Forall (Map.fromList (map (,()) targs)) typ
  constrainDeclaration (Ident.fromId dcon) scheme finalConstraint
constrainVariant _ _ _ (I.VariantNamed _) _ =
  error "No support for named variants yet"


-- | HELPER: CONSTRAIN NAME TO TYPE
constrainDeclaration ::
  Ident.Identifier -> Can.Scheme -> Constraint -> TC Constraint
constrainDeclaration name scheme finalConstraint = do
  v <- mkFlexVar
  let t = TVarN v
  let header = Map.singleton (Ident.fromId name) t
  return $ CLet [] [v] header (CForeign scheme t) finalConstraint
