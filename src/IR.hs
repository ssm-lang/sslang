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

lowerAst :: A.Program -> Compiler.Pass (I.Program Ann.Type)
lowerAst = lowerProgram

inferTypes :: I.Program Ann.Type -> Compiler.Pass (I.Program Classes.Type)
inferTypes = inferProgram

instantiateClasses
  :: I.Program Classes.Type -> Compiler.Pass (I.Program Poly.Type)
instantiateClasses = instProgram

yieldAbstraction :: I.Program Poly.Type -> Compiler.Pass (I.Program Poly.Type)
yieldAbstraction = return

lambdaLift :: I.Program Poly.Type -> Compiler.Pass (I.Program Poly.Type)
lambdaLift = return

defunctionalize :: I.Program Poly.Type -> Compiler.Pass (I.Program Poly.Type)
defunctionalize = return

inferDrops :: I.Program a -> Compiler.Pass (I.Program a)
inferDrops = return

monomorphize :: I.Program Poly.Type -> Compiler.Pass (I.Program Flat.Type)
monomorphize = monoProgram
