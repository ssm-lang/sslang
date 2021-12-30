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

import           IR.ClassInstantiation          ( instProgram )
import           IR.LambdaLift                  ( liftProgramLambdas )
import           IR.LowerAst                    ( lowerProgram )
import           IR.Monomorphize                ( monoProgram )
import           IR.TypeInference               ( inferProgram )

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
newtype Options = Options { mode :: Mode } deriving (Eq, Show)

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

-- | IR compiler sub-stage, lowering AST to optionally type-annotated IR.
lower :: Options -> A.Program -> Pass (I.Program Ann.Type)
lower opt prg = do
  ir <- lowerProgram prg
  when (mode opt == DumpIR) $ dump ir
  return ir

-- | IR compiler sub-stage, ultimately producing a fully-typed IR.
ann2Class :: Options -> I.Program Ann.Type -> Pass (I.Program Cls.Type)
ann2Class opt ir = do
  irInferred <- inferProgram ir
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
