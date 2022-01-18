{- | Intermediate representation (IR) stage of the compiler pipeline.

The IR stage is organized into 4 distinct substages: 'lower', 'ann2Class',
'class2Poly', and 'poly2Flat', delineated by each type system used in the
compiler.
-}
module IR where

import           Common.Compiler
import           Common.Default                 ( Default(..) )

import qualified Front.Ast                     as A

import qualified IR.IR                         as I
import qualified IR.Types.Annotated            as Ann
import qualified IR.Types.Classes              as Cls
import qualified IR.Types.Flat                 as Flat
import qualified IR.Types.Poly                 as Poly
import qualified IR.TypeChecker                as TC
import qualified IR.HM                         as HM

import           IR.ClassInstantiation          ( instProgram )
import           IR.LambdaLift                  ( liftProgramLambdas )
import           IR.LowerAst                    ( lowerProgram )
import           IR.Monomorphize                ( monoProgram )

import           Control.Monad                  ( when )
import           System.Console.GetOpt          ( ArgDescr(..)
                                                , OptDescr(..)
                                                )

-- | Operation modes for the IR compiler stage.
data Mode
  = Continue          -- ^ Compile end-to-end (default).
  | DumpIR            -- ^ Print the IR immediately after lowering.
  | DumpIRTyped       -- ^ Print the fully-typed IR after type inference.
  | DumpIRLifted      -- ^ Print the IR after lambda lifting.
  | DumpIRFinal       -- ^ Print the final IR representation before codegen.
  deriving (Eq, Show)

data TIType
 = HMOnly             -- ^ Only run HM type inference
 | TCOnly             -- ^ Only run type checker
 | Both               -- ^ Run both HM type Inference and type checker
 deriving (Eq, Show)

-- | Compiler options for the IR compiler stage.
data Options = Options { mode :: Mode, tiType :: TIType } deriving (Eq, Show)

instance Default Options where
  def = Options { mode = Continue, tiType = Both }

-- | CLI options for the IR compiler stage.
options :: [OptDescr (Options -> Options)]
options =
  [ Option ""
           ["dump-ir"]
           (NoArg $ setMode DumpIR)
           "Print the IR immediately after lowering"
  , Option ""
           ["dump-ir-typed"]
           (NoArg $ setMode DumpIRTyped)
           "Print the fully-typed IR after type inference"
  , Option ""
           ["dump-ir-lifted"]
           (NoArg $ setMode DumpIRLifted)
           "Print the IR after lambda lifting"
  , Option ""
           ["dump-ir-final"]
           (NoArg $ setMode DumpIRFinal)
           "Print the last IR representation before code generation"
  , Option ""
           ["only-hm"]
           (NoArg $ setHM)
           "Only run HM type inference"
  , Option ""
           ["only-tc"]
           (NoArg $ setTC)
           "Only run type checker"
  ]
  where setMode m o = o { mode = m }

-- | IR compiler sub-stage, lowering AST to optionally type-annotated IR.
lower :: Options -> A.Program -> Pass (I.Program Ann.Type)
lower opt prg = do
  ir <- lowerProgram prg
  when (mode opt == DumpIR) $ dump ir
  return ir

-- | IR compiler sub-stage, ultimately producing a fully-typed IR. Run HM type
-- inference, type checker, or both (default) based on the command line option.
-- Note that when both algorithms are run, the program doesn't type check iff
-- both algorithms throw an error.
ann2Class :: Options -> I.Program Ann.Type -> Pass (I.Program Cls.Type)
ann2Class opt ir = do
  irInferred <- do
    let irHMInferred = HM.inferProgram ir
        irTCInferred = TC.inferProgram ir
    case tiType opt of
      HMOnly -> irHMInferred
      TCOnly -> irTCInferred
      _ -> case (runPass irHMInferred, runPass irTCInferred) of
        (Left e , Left _) -> throwError e
        (Right _, _     ) -> irHMInferred
        _                 -> irTCInferred
  when (mode opt == DumpIRTyped) $ dump irInferred
  return irInferred

-- | IR compiler sub-stage, ultimately instantiating all type classes.
class2Poly :: Options -> I.Program Cls.Type -> Pass (I.Program Poly.Type)
class2Poly _ = instProgram

{- | IR compiler sub-stage, ultimately monomorphizing the polymorphic type
system.

TODO: This is probably not necessary in the future.
-}
poly2Flat :: Options -> I.Program Poly.Type -> Pass (I.Program Flat.Type)
poly2Flat opt ir = do
  irLifted <- liftProgramLambdas ir
  when (mode opt == DumpIRLifted) $ dump irLifted
  ir' <- monoProgram irLifted
  when (mode opt == DumpIRFinal) $ dump ir'
  return ir'

-- | IR compiler stage.
run :: Options -> A.Program -> Pass (I.Program Flat.Type)
run opt prg =
  lower opt prg >>= ann2Class opt >>= class2Poly opt >>= poly2Flat opt

-- | Helper function to set ti type to HM-only
setHM :: Options -> Options
setHM o = o { tiType = HMOnly }

-- | Helper function to set ti type to TC-only
setTC :: Options -> Options
setTC o = o { tiType = TCOnly }
