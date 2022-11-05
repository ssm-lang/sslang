module IR.Constraint.Constrain.Expression where

import qualified Common.Identifiers            as Ident
import           Control.Monad                  ( zipWithM )
import           Data.Bifunctor                 ( first )
import           Data.Foldable                  ( foldrM )
import qualified Data.Map.Strict               as Map
import           IR.Constraint.Monad            ( TC
                                                , freshVar
                                                )
import           IR.Constraint.Type            as Type
import qualified IR.IR                         as I
import           IR.SegmentLets                 ( segmentDefs' )


-- | HELPER ALIASES


type Attachment = (I.Annotations, Variable)

type BinderDef = (I.Binder, I.Expr Attachment)
type Def = (I.VarId, I.Expr Attachment)


-- | DEFS


constrainBinderDefs :: [BinderDef] -> Constraint -> TC Constraint
constrainBinderDefs binderDefs finalConstraint = do
  defs <- mapM
    (\(binder, expr) -> do
      name <- binderToVar binder
      return (name, expr)
    )
    binderDefs
  constrainDefs defs finalConstraint


constrainDefs :: [Def] -> Constraint -> TC Constraint
constrainDefs defs finalConstraint = do
  let segments = segmentDefs' defs
  foldrM constrainRecDefs finalConstraint segments


constrainRecDefs :: [Def] -> Constraint -> TC Constraint
constrainRecDefs defs finalConstraint = do
  flexVars <- mapM (const mkFlexVar) defs
  let flexHeaderList = zipWith
        (\(name, _) flexVar -> (Ident.fromId name, TVarN flexVar))
        defs
        flexVars
      flexHeaders = Map.fromList flexHeaderList
  headerConstraints <- zipWithM (\(_, e) (_, tipe) -> constrainExpr e tipe)
                                defs
                                flexHeaderList
  return $ CLet []
                flexVars
                flexHeaders
                (CLet [] [] flexHeaders CTrue (CAnd headerConstraints))
                finalConstraint


-- | CONSTRAIN EXPRESSIONS

constrainExpr :: I.Expr Attachment -> Type -> TC Constraint
constrainExpr expr expected = do
  constraint <- exprConstraint
  constrainAttachment (I.extract expr) expected constraint
 where
  exprConstraint = do
    case expr of
      I.Var  name _              -> return $ CLocal (Ident.fromId name) expected
      I.Data name _              -> return $ CLocal (Ident.fromId name) expected
      I.Lit  lit  _              -> constrainLit lit expected
      I.App    func       arg  _ -> constrainApp func arg expected
      I.Lambda binder     body _ -> constrainLambda binder body expected
      I.Let    binderDefs body _ -> do
        constrainBinderDefs binderDefs =<< constrainExpr body expected
      _ -> error "Constraint generation not supported yet"

constrainAttachment :: Attachment -> Type -> Constraint -> TC Constraint
constrainAttachment (_, u) expected finalConstraint =
  return $ exists [u] $ CAnd [CEqual (TVarN u) expected, finalConstraint]

constrainLit :: I.Literal -> Type -> TC Constraint
constrainLit (I.LitIntegral _) expected = return $ CEqual Type.i32 expected
constrainLit I.LitEvent        expected = return $ CEqual Type.unit expected

constrainApp :: I.Expr Attachment -> I.Expr Attachment -> Type -> TC Constraint
constrainApp func arg resultType = do
  argVar <- mkFlexVar
  let argType  = TVarN argVar
      funcType = argType ==> resultType
  funcCon <- constrainExpr func funcType
  argCon  <- constrainExpr arg argType
  return $ exists [argVar] (CAnd [funcCon, argCon])

constrainLambda :: I.Binder -> I.Expr Attachment -> Type -> TC Constraint
constrainLambda binder body expected = do
  argName   <- binderToVar binder

  argVar    <- mkFlexVar
  resultVar <- mkFlexVar
  let argType    = TVarN argVar
      resultType = TVarN resultVar

  bodyCon <- constrainExpr body resultType

  return $ exists [argVar, resultVar] $ CAnd
    [ CLet [] [] (Map.singleton (Ident.fromId argName) argType) CTrue bodyCon
    , CEqual (argType ==> resultType) expected
    ]


-- | BINDER HELPERS

binderToVar :: I.Binder -> TC I.VarId
binderToVar Nothing    = freshVar
binderToVar (Just var) = return var
