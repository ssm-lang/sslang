module IR.Constraint.Constrain where

import           IR.Constraint.Constraint
import qualified IR.Constraint.Type            as Type
import qualified IR.IR                         as I

run :: I.Program Type.Annotations -> IO (Constraint, I.Program Variable)
run = undefined
