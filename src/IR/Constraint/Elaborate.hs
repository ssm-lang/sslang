module IR.Constraint.Elaborate where

import           IR.Constraint.Constraint
import qualified IR.IR                         as Type
import qualified IR.IR                         as I

run :: I.Program Variable -> IO (I.Program Type.Type)
run = undefined
