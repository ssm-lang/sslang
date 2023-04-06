{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Intermediate representation (IR) stages of the compiler pipeline.
module IR where

import Common.Compiler
import Common.Default (Default (..))
import qualified Front.Ast as A
import qualified IR.IR as I

import Control.Monad (
  when,
  (>=>),
 )
import IR.ClassInstantiation (instProgram)
import IR.DConToFunc (dConToFunc)
import IR.DesugarPattern (desugarPattern)
import IR.ExternToCall (externToCall)
import IR.InsertRefCounting (insertRefCounting)
import IR.LambdaLift (liftProgramLambdas)
import IR.LowerAst (lowerProgram)
import IR.OptimizePar (optimizePar)
import IR.Pattern (checkAnomaly)
import IR.SegmentLets (segmentLets)
import IR.Constraint.Typechecking ( typecheckProgram, TypecheckOptions(..) )
import IR.Types.Type ( fromAnnotations )
import IR.Simplify (simplifyProgram)

import System.Console.GetOpt (
  ArgDescr (..),
  OptDescr (..),
 )
import Text.Show.Pretty


{- | Operation modes for the IR compiler stage.

 By default, 'Continue' completes the pipeline end-to-end.
-}
data Mode
  = Continue
  | DumpIR
  | DumpIRAnnotated
  | DumpIRConstraints
  | DumpIRNoAuxiliary
  | DumpIRTyped
  | DumpIRTypedUgly
  | DumpIRInlined
  | DumpIRTypedShow
  | DumpIRLifted
  | DumpIRFinal
  deriving (Eq, Show)


-- | Compiler options for the IR compiler stage.
newtype Options = Options {mode :: Mode}
  deriving (Eq, Show)


instance Default Options where
  def = Options{mode = Continue}


-- | CLI options for the IR compiler stage.
options :: [OptDescr (Options -> Options)]
options =
  [ Option
      ""
      ["dump-ir"]
      (NoArg $ setMode DumpIRAnnotated)
      "Print the IR immediately after lowering"
  , Option
      ""
      ["dump-ir-annotated"]
      (NoArg $ setMode DumpIRTyped)
      "Print the fully-typed IR just before type inference"
  , Option
      ""
      ["dump-ir-constraints"]
      (NoArg $ setMode DumpIRConstraints  )
      "Print the constraint IR used by the constraint solver type inference"
  , Option
      ""
      ["dump-ir-constraints-no-auxiliary"]
      (NoArg $ setMode DumpIRNoAuxiliary  )
      "Print the constraint IR used by the constraint solver type inference without auxiliary constraints"
  , Option
      ""
      ["dump-ir-typed"]
      (NoArg $ setMode DumpIRTyped)
      "Print the fully-typed IR after type inference"
  , Option
      ""
      ["dump-ir-typed-ugly"]
      (NoArg $ setMode DumpIRTypedShow)
      "Ugly-Print the fully-typed IR after type inference"
  , Option
      ""
      ["dump-ir-lifted"]
      (NoArg $ setMode DumpIRLifted)
      "Print the IR after lambda lifting"
  , Option
      ""
      ["dump-ir-inlined"]
      (NoArg $ setMode DumpIRInlined)
      "Print IR after inlining optimization and before dup drops"
  , Option
      ""
      ["dump-ir-final"]
      (NoArg $ setMode DumpIRFinal)
      "Print the last IR representation before code generation"
  ]
 where
  setMode m o = o{mode = m}


-- | Lower from AST to IR (with annotations).
lower :: Options -> A.Program -> Pass (I.Program I.Annotations)
lower opt p = do
  p <- lowerProgram p
  when (mode opt == DumpIR) $ dump $ fmap fromAnnotations p
  return p


{- | Type inference + check against type annotations.

 After this stage, no compiler errors should be thrown.
-}
typecheck :: Options -> I.Program I.Annotations -> Pass (I.Program I.Type)
typecheck opt p = do
  when (mode opt == DumpIRAnnotated) $ dump $ fmap fromAnnotations p

  let typecheck_opt = case mode opt of
        DumpIRConstraints -> PrintConstraints
        DumpIRNoAuxiliary -> PrintNoAuxiliary
        _ -> None

  (p, constraints) <- typecheckProgram p typecheck_opt 
  when (mode opt == DumpIRConstraints || mode opt == DumpIRNoAuxiliary) $ dump $ show constraints
  when (mode opt == DumpIRTyped) $ dump p
  when (mode opt == DumpIRTypedShow) $ (throwError . Dump . ppShow) p
  return p


anomalycheck :: I.Program I.Type -> Pass (I.Program I.Type)
anomalycheck p = do
  checkAnomaly p
  return p


-- | IR transformations to prepare for codegen.
transform :: Options -> I.Program I.Type -> Pass (I.Program I.Type)
transform opt p = do
  p <- desugarPattern p
  p <- instProgram p
  p <- segmentLets p
  p <- dConToFunc p
  p <- externToCall p
  p <- optimizePar p
  p <- liftProgramLambdas p
  when (mode opt == DumpIRLifted) $ dump p
  p <- simplifyProgram p -- TODO: inline BEFORE lambda lifting!!
  when (mode opt == DumpIRInlined) $ dump p
  p <- insertRefCounting p
  when (mode opt == DumpIRFinal) $ dump p
  return p


-- | IR compiler stage.
run :: Options -> A.Program -> Pass (I.Program I.Type)
run opt = lower opt >=> typecheck opt >=> anomalycheck >=> transform opt
