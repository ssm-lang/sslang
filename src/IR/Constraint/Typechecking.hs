module IR.Constraint.Typechecking (
  typecheckProgram,
) where

import qualified Common.Compiler as Compiler
import Common.Pretty (hardline, pretty)
import qualified IR.Constraint.Canonical as Can
import qualified IR.Constraint.Constrain as Constrain
import qualified IR.Constraint.Elaborate as Elaborate
import IR.Constraint.Monad (
  mkTCState,
  runTC,
 )
import qualified IR.Constraint.Solve as Solve
import qualified IR.IR as I
import IR.Pretty ()

import IR.Constraint.PrettyPrint (
  Doc,
  printConstraint,
 )

import IR.Constraint.Type as Typ (
  toCanType,
 )

import Prettyprinter ( 
   surround,
   vsep,
  )

typecheckProgram
  :: I.Program Can.Annotations -> Bool -> Compiler.Pass (I.Program Can.Type, Maybe (Doc ann))
typecheckProgram pAnn prettyprint = case unsafeTypecheckProgram pAnn prettyprint of
  Left e -> Compiler.throwError e
  Right pType -> return pType


unsafeTypecheckProgram
  :: I.Program Can.Annotations -> Bool -> Either Compiler.Error (I.Program Can.Type, Maybe (Doc ann))
unsafeTypecheckProgram pAnn prettyprint = runTC (mkTCState pAnn) $ do
  (constraint, pVar) <- Constrain.run pAnn

  -- Get IORefs in the program IR
  let refs = foldr (:) [] pVar

  let vars = mapM toCanType refs

  -- Depends on being called before solve
  namesDoc <- map pretty <$> vars 
  
  doc <-
    if not prettyprint
      then return Nothing
      else do
        -- Convert IORef Variables to printable "type variables" embedded in IR
        pIR <- pretty <$> mapM toCanType pVar
        pConstraint <- printConstraint constraint
        
        return $ Just $ pConstraint <> hrule <> pIR

  -- Runs constraint solver
  Solve.run constraint

  -- Depends on being called after solve
  resolutionDoc <- map pretty <$> vars 

  -- Document containing the mapping from flex vars to types
  let mappingDoc = vsep $ zipWith (surround (pretty " = ")) namesDoc resolutionDoc

  let finalDoc = (<> hrule <> mappingDoc) <$> doc

  program <- Elaborate.run pVar
  return (program, finalDoc)

  where hrule = hardline <> hardline <> pretty (replicate 20 '-') <> hardline <> hardline <> hardline