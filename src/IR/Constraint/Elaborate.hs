module IR.Constraint.Elaborate where

import qualified IR.Constraint.Canonical       as Can
import           IR.Constraint.Monad            ( TC )
import           IR.Constraint.Type            as Type
import qualified IR.IR                         as I

run :: I.Program Variable -> TC (I.Program Can.Type)
run pVar = do
  let (names, exprs) = unzip $ I.programDefs pVar
  exprs' <- mapM (mapM Type.toCanType) exprs
  let pdefs' = zip names exprs'
  return $ pVar { I.programDefs = pdefs' }
