module IR.Constraint.Constrain.Expression where

import qualified Common.Identifiers            as Ident
import           IR.Constraint.Monad            ( TC
                                                , freshVar
                                                )
import           IR.Constraint.Type
import qualified IR.IR                         as I
import           IR.SegmentLets                 ( segmentDefs )

constrainBinderDefs
  :: [(I.Binder, I.Expr I.Annotations)]
  -> Constraint
  -> TC (Constraint, [(I.Binder, I.Expr Variable)])
constrainBinderDefs = undefined


constrainDefs
  :: [(Ident.VarId, I.Expr I.Annotations)]
  -> Constraint
  -> TC (Constraint, [(Ident.VarId, I.Expr Variable)])
constrainDefs = undefined


-- | BINDER HELPERS

binderToVar :: I.Binder -> TC I.VarId
binderToVar Nothing    = freshVar
binderToVar (Just var) = return var
