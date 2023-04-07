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

import IR.Constraint.PrettyPrint                (printConstraint
                                                , Doc
                                                )


typecheckProgram
  :: I.Program Can.Annotations -> Bool -> Compiler.Pass (I.Program Can.Type, Maybe(Doc ann))
typecheckProgram pAnn print = case unsafeTypecheckProgram pAnn print of
  Left  e     -> Compiler.throwError e
  Right pType -> return pType

unsafeTypecheckProgram
  :: I.Program Can.Annotations -> Bool -> Either Compiler.Error (I.Program Can.Type, Maybe(Doc ann))
unsafeTypecheckProgram pAnn print = runTC (mkTCState pAnn) $ do
  (constraint, pVar) <- Constrain.run pAnn
  doc <- if print 
         then do
            c <- printConstraint constraint
            return $ Just c
          else
            return Nothing
      
  Solve.run constraint
  program <- Elaborate.run pVar
  return (program, doc)
