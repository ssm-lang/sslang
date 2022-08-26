-- | Intermediate representation (IR) stages of the compiler pipeline.
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module IR where

import           Common.Compiler
import           Common.Default                 ( Default(..) )
import qualified Front.Ast                     as A
import qualified IR.IR                         as I

import           IR.ClassInstantiation          ( instProgram )
import           IR.DConToFunc                  ( dConToFunc )
import           IR.ExternToCall                ( externToCall )
import           IR.InsertRefCounting           ( insertRefCounting )
import           IR.LambdaLift                  ( liftProgramLambdas )
import           IR.LowerAst                    ( lowerProgram )
import           IR.SegmentLets                 ( segmentLets )
import           IR.Types                       ( fromAnnotations
                                                , typecheckProgram
                                                )

import           Control.Monad                  ( when )
import           System.Console.GetOpt          ( ArgDescr(..)
                                                , OptDescr(..)
                                                )

-- | Operation modes for the IR compiler stage.
--
-- By default, 'Continue' completes the pipeline end-to-end.
data Mode
  = Continue
  | DumpIR
  | DumpIRAnnotated
  | DumpIRTyped
  | DumpIRLifted
  | DumpIRFinal
  deriving (Eq, Show)

-- | Compiler options for the IR compiler stage.
newtype Options = Options { mode :: Mode }
  deriving (Eq, Show)

instance Default Options where
  def = Options { mode = Continue }

-- | CLI options for the IR compiler stage.
options :: [OptDescr (Options -> Options)]
options =
  [ Option ""
           ["dump-ir"]
           (NoArg $ setMode DumpIR)
           "Print the IR immediately after lowering"
  , Option ""
           ["dump-ir-annotated"]
           (NoArg $ setMode DumpIRTyped)
           "Print the fully-typed IR just before type inference"
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
  ]
  where setMode m o = o { mode = m }

-- | IR compiler stage.
run :: Options -> A.Program -> Pass (I.Program I.Type)
run opt p = do
  p <- lowerProgram p
  when (mode opt == DumpIR) $ dump $ fmap fromAnnotations p
  when (mode opt == DumpIRAnnotated) $ dump $ fmap fromAnnotations p
  p <- typecheckProgram p
  when (mode opt == DumpIRTyped) $ dump p
  p <- instProgram p
  p <- segmentLets p
  p <- dConToFunc p
  p <- externToCall p
  p <- liftProgramLambdas p
  when (mode opt == DumpIRLifted) $ dump p
  p <- insertRefCounting p
  when (mode opt == DumpIRFinal) $ dump p
  return p
