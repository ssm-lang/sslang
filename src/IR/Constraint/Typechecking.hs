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

import Control.Monad.Writer (
  tell,
  when,
 )


typecheckProgram ::
  I.Program Can.Annotations -> Bool -> Compiler.Pass (I.Program Can.Type)
typecheckProgram pAnn False = do
  let (result, _) = unsafeTypecheckProgram pAnn False
  case result of
    Left e -> Compiler.throwError e
    Right pType -> return pType
typecheckProgram pAnn True = do
  let (result, pp) = unsafeTypecheckProgram pAnn True
  case result of
    Left e -> do
      -- Note: Provisionally misusing exception/error by showing it and appending it to dump, open to revision.
      Compiler.dump $ show pp ++ "\n\n" ++ show e 
    Right _ -> Compiler.dump $ show pp


unsafeTypecheckProgram ::
  I.Program Can.Annotations -> Bool -> (Either Compiler.Error (I.Program Can.Type), Doc String)
unsafeTypecheckProgram pAnn prettyprint = runTC (mkTCState pAnn) $ do
  (constraint, pVar) <- Constrain.run pAnn

  -- Get IORefs in the program IR, then type variable names
  let refs = foldr (:) [] pVar
      vars = mapM toCanType refs

  -- Pretty-printing separator
  let hrule = hardline <> hardline <> pretty (replicate 20 '-') <> hardline <> hardline

  -- Log the pretty-printed constraints and program
  when prettyprint $ do
    -- Convert IORef Variables to printable "type variables" embedded in IR
    pIR <- pretty <$> mapM toCanType pVar
    pConstraint <- printConstraint constraint

    tell $ pConstraint <> hrule <> pIR

  -- Depends on being called before solve
  -- Gets the prettified version of the type variable names
  namesDoc <- map pretty <$> vars

  -- Runs constraint solver
  Solve.run constraint

  -- Depends on being called after solve
  -- Gets the prettified version of the unification results
  resolutionDoc <- map pretty <$> vars

  -- Document containing the mapping from flex vars to types
  let mappingDoc = vsep $ zipWith (surround (pretty " = ")) namesDoc resolutionDoc
      finalDoc = hrule <> mappingDoc

  -- Log the unification results
  when prettyprint $ tell finalDoc

  program <- Elaborate.run pVar

  return program
