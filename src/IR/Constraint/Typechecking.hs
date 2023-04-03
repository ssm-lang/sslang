module IR.Constraint.Typechecking
  ( typecheckProgram
  , TypecheckOptions(..)
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

import IR.Constraint.PrettyPrint                (printConstraint
                                                , PrintingOptions(..) 
                                                , Doc
                                                )

data TypecheckOptions = None | PrintConstraints | PrintNoAuxiliary deriving (Eq)

typecheckProgram
  :: I.Program Can.Annotations -> TypecheckOptions -> Compiler.Pass (I.Program Can.Type, Doc ann)
typecheckProgram pAnn opt = case unsafeTypecheckProgram pAnn opt of
  Left  e     -> Compiler.throwError e
  Right pType -> return pType

unsafeTypecheckProgram
  :: I.Program Can.Annotations -> TypecheckOptions -> Either Compiler.Error (I.Program Can.Type, Doc ann)
unsafeTypecheckProgram pAnn opt = runTC (mkTCState pAnn) $ do
  (constraint, pVar) <- Constrain.run pAnn

  let printing_opt = case opt of
        PrintNoAuxiliary -> NoAuxiliary
        _ -> Pretty
  c <- printConstraint printing_opt constraint 
  
  Solve.run constraint
  program <- Elaborate.run pVar
  return (program, c)
