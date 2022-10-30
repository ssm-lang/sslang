module IR.Constraint.Typechecking
  ( typecheckProgram
  ) where

import qualified Common.Compiler               as Compiler
import qualified IR.Constraint.Canonical       as Can
import qualified IR.Constraint.Constrain       as Constrain
import qualified IR.Constraint.Elaborate       as Elaborate
import qualified IR.Constraint.Error           as ET
import qualified IR.Constraint.Solve           as Solve
import qualified IR.IR                         as I

import           System.IO.Unsafe               ( unsafePerformIO )

typecheckProgram
  :: I.Program Can.Annotations -> Compiler.Pass (I.Program Can.Type)
typecheckProgram pAnn = case unsafeTypecheckProgram pAnn of
  Left [] -> Compiler.throwError $ Compiler.TypeError $ Compiler.fromString
    "Some unknown type error occured, but this case should never happen."
  Left  (e : _) -> Compiler.throwError e
  Right pType   -> return pType

unsafeTypecheckProgram
  :: I.Program Can.Annotations -> Either [Compiler.Error] (I.Program Can.Type)
unsafeTypecheckProgram pAnn = unsafePerformIO $ do
  (constraint, pVar) <- Constrain.run pAnn
  result             <- Solve.run constraint
  case result of
    Left  errors -> return $ Left (map ET.toCompilerError errors)
    Right _      -> Right <$> Elaborate.run pVar
