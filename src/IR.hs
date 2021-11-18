-- | Entry point for the IR portion of the compiler pipeline.
module IR where

import qualified Common.Compiler               as Compiler

import qualified Front.Ast                     as A

import qualified IR.IR                         as I

import qualified IR.Types.Annotated            as Ann
import qualified IR.Types.Classes              as Classes
import qualified IR.Types.Flat                 as Flat
import qualified IR.Types.Poly                 as Poly

import           IR.ClassInstantiation          ( instProgram )
import           IR.LowerAst                    ( lowerProgram )
import           IR.Monomorphize                ( monoProgram )
import           IR.TypeInference               ( inferProgram )

-- | Lower the AST representation of a program into its IR representation.
lowerAst :: A.Program -> Compiler.Pass (I.Program Ann.Type)
lowerAst = lowerProgram

-- | Infer the types of a program.
inferTypes :: I.Program Ann.Type -> Compiler.Pass (I.Program Classes.Type)
inferTypes = inferProgram

-- | Explicitly pass around class instances.
instantiateClasses
  :: I.Program Classes.Type -> Compiler.Pass (I.Program Poly.Type)
instantiateClasses = instProgram

-- | Lift non-application expressions out of parallel evaluation.
yieldAbstraction :: I.Program Poly.Type -> Compiler.Pass (I.Program Poly.Type)
yieldAbstraction = return

-- | Lift lambdas to top-level
lambdaLift :: I.Program Poly.Type -> Compiler.Pass (I.Program Poly.Type)
lambdaLift = return

-- | Remove partially applied functions
defunctionalize :: I.Program Poly.Type -> Compiler.Pass (I.Program Poly.Type)
defunctionalize = return

-- | Infer where variables go out of scope
inferDrops :: I.Program a -> Compiler.Pass (I.Program a)
inferDrops = return

-- | Remove polymorphic definitions
monomorphize :: I.Program Poly.Type -> Compiler.Pass (I.Program Flat.Type)
monomorphize = monoProgram
