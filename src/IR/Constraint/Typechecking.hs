module IR.Constraint.Typechecking
  ( typecheckProgram
  ) where

import qualified Common.Compiler               as Compiler
import qualified IR.Constraint.Constrain       as Constrain
import qualified IR.Constraint.Elaborate       as Elaborate
import qualified IR.Constraint.Solve           as Solve
import qualified IR.Constraint.Type            as Type
import qualified IR.IR                         as I

import           System.IO.Unsafe               ( unsafePerformIO )

typecheckProgram
  :: I.Program Type.Annotations -> Compiler.Pass (I.Program Type.Type)
typecheckProgram pAnn = case unsafeTypecheckProgram pAnn of
  Nothing ->
    Compiler.throwError $ Compiler.TypeError $ Compiler.fromString "Type error"
  Just pType -> return pType

unsafeTypecheckProgram
  :: I.Program Type.Annotations -> Maybe (I.Program Type.Type)
unsafeTypecheckProgram pAnn = unsafePerformIO $ do
  (constraint, pVar) <- Constrain.run pAnn
  solved             <- Solve.run constraint
  if solved then Just <$> Elaborate.run pVar else return Nothing
