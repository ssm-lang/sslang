module Types where

import Common.Errors (CompileError(..))
import IR.IR(Program(..))

import qualified Types.Ast     as Ast
import qualified Types.Classes as Classes
import qualified Types.Poly as Poly
import qualified Types.Flat as Flat

inferTypes :: Program Ast.Type -> Either CompileError (Program Classes.Type)
inferTypes = undefined

instantiateClasses :: Program Classes.Type -> Either CompileError (Program Poly.Type)
instantiateClasses = undefined

monomorphize :: Program Poly.Type -> Either CompileError (Program Flat.Type)
monomorphize = undefined
