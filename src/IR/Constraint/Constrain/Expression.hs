module IR.Constraint.Constrain.Expression where

import qualified Common.Identifiers            as Ident
import           Data.Bifunctor                 ( first )
import           Data.Foldable                  ( foldrM )
import           IR.Constraint.Monad            ( TC
                                                , freshVar
                                                )
import           IR.Constraint.Type
import qualified IR.IR                         as I
import           IR.SegmentLets                 ( segmentDefs' )


-- | DEFS


constrainBinderDefs
  :: [(I.Binder, I.Expr (I.Annotations, Variable))]
  -> Constraint
  -> TC Constraint
constrainBinderDefs binderDefs finalConstraint = do
  defs <- mapM
    (\(binder, expr) -> do
      name <- binderToVar binder
      return (name, expr)
    )
    binderDefs
  constrainDefs defs finalConstraint


constrainDefs
  :: [(Ident.VarId, I.Expr (I.Annotations, Variable))]
  -> Constraint
  -> TC Constraint
constrainDefs defs finalConstraint = do
  let segments = segmentDefs' defs
  foldrM constrainRecDefs finalConstraint segments


constrainRecDefs
  :: [(I.VarId, I.Expr (I.Annotations, Variable))]
  -> Constraint
  -> TC Constraint
constrainRecDefs defs finalConstraint = undefined


-- | BINDER HELPERS

binderToVar :: I.Binder -> TC I.VarId
binderToVar Nothing    = freshVar
binderToVar (Just var) = return var
