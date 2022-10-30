module IR.Constraint.Constrain.Environment where

import qualified Common.Identifiers            as Ident
import qualified Data.Map.Strict               as Map
import qualified IR.Constraint.Canonical       as Can


-- | ENVIRONMENT

-- for now, _tcons is only used to check type annotaions
newtype Env
  = Env { _tcons :: Map.Map Ident.TConId Can.Kind }
