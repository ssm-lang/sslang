module IR.Constraint.Constrain where

import qualified IR.Constraint.Canonical       as Can
import           IR.Constraint.Type
import qualified IR.IR                         as I

run :: I.Program Can.Annotations -> IO (Constraint, I.Program Variable)
run = undefined
