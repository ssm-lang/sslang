module Codegen.TypeDef where

import qualified IR.IR                         as L
import qualified Types.TypeSystem              as L
import qualified Types.Flat                    as L

import           Language.C.Quote.GCC
import qualified Language.C.Syntax             as C

todo :: a
todo = error "Not yet implemented"

-- | Generate definitions for SSM type definitions.
genTypeDef :: L.TypeDef L.Type -> C.Definition
genTypeDef = todo
