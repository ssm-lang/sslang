module IR.Constraint.Typechecking
  ( typecheckProgram
  ) where

import qualified Common.Compiler               as Compiler
import qualified IR.Constraint.Canonical       as Can
import qualified IR.Constraint.Constrain       as Constrain
import qualified IR.Constraint.Elaborate       as Elaborate
import           IR.Constraint.Monad            ( mkTCState
                                                , runTC
                                                )
import qualified IR.Constraint.Solve           as Solve
import qualified IR.IR                         as I


typecheckProgram
  :: I.Program Can.Annotations -> Compiler.Pass (I.Program Can.Type)
typecheckProgram pAnn = case unsafeTypecheckProgram pAnn of
  Left  e     -> Compiler.throwError e
  Right pType -> return pType

unsafeTypecheckProgram
  :: I.Program Can.Annotations -> Either Compiler.Error (I.Program Can.Type)
unsafeTypecheckProgram pAnn = runTC (mkTCState pAnn) $ do
  (constraint, pVar) <- Constrain.run pAnn
  Solve.run constraint
  Elaborate.run pVar
