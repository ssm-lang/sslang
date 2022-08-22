{- | Intermediate representation (IR) stage of the compiler pipeline.

The IR stage is organized into 4 distinct substages: 'lower', 'ann2Class',
'class2Poly', and 'poly2Flat', delineated by each type system used in the
compiler.
-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module IR where

import           Common.Compiler
import           Common.Default                 ( Default(..) )
import qualified Front.Ast                     as A
import qualified IR.IR                         as I

import           IR.ClassInstantiation          ( instProgram )
import           IR.DConToFunc                  ( dConToFunc )
import           IR.DropInference               ( insertDropsProgram )
import           IR.ExternToCall                ( externToCall )
import           IR.LambdaLift                  ( liftProgramLambdas )
import           IR.LowerAst                    ( lowerProgram )
import           IR.Types                       ( fromAnnotations
                                                , inferProgram
                                                )

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

-- | Compiler options for the IR compiler stage.
data Options = Options
  { mode    :: Mode
  , dupDrop :: Bool
  }
  deriving (Eq, Show)

instance Default Options where
  def = Options { mode = Continue, dupDrop = False }

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
  , Option "" ["dup-drop"] (NoArg setDropInf) "run with drop inference"
  ]
 where
  setMode m o = o { mode = m }
  setDropInf o = o { dupDrop = True }

-- | IR compiler stage.
run :: Options -> A.Program -> Pass (I.Program I.Type)
run opt p = do
  p <- lowerProgram p
  when (mode opt == DumpIR) $ dump $ fmap fromAnnotations p
  p <- inferProgram p
  when (mode opt == DumpIRTyped) $ dump p
  p <- instProgram p
  p <- dConToFunc p
  p <- externToCall p
  p <- liftProgramLambdas p
  when (mode opt == DumpIRLifted) $ dump p
  p <- if dupDrop opt then insertDropsProgram p else return p
  when (mode opt == DumpIRFinal) $ dump p
  return p
