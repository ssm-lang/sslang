-- | Generate C type definitions for IR type definitions.
module Codegen.TypeDef where

import qualified IR.Types.Flat                 as L
import qualified IR.Types.TypeSystem           as L

import qualified Language.C.Syntax             as C

-- | Generate definitions for SSM type definitions.
genTypeDef :: L.TypeDef L.Type -> C.Definition
genTypeDef = error "Not yet implemented"
