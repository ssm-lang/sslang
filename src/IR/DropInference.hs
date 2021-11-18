-- | Infer where references should be dropped in the IR.
module IR.DropInference where

import           IR.IR                          ( Expr(..) )

-- | Infer where references should be dropped in an IR program.
inferDrops :: Expr t -> Expr t
inferDrops = error "todo"
