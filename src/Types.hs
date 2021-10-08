module Types where

import qualified Common.Compiler               as Compiler
import           IR.IR                          ( Program(..) )

import qualified Types.Ast                     as Ast
import qualified Types.Classes                 as Classes
import qualified Types.Flat                    as Flat
import qualified Types.Poly                    as Poly

inferTypes :: Program Ast.Type -> Compiler.Pass (Program Classes.Type)
inferTypes = undefined

instantiateClasses :: Program Classes.Type -> Compiler.Pass (Program Poly.Type)
instantiateClasses = undefined

monomorphize :: Program Poly.Type -> Compiler.Pass (Program Flat.Type)
monomorphize = undefined
