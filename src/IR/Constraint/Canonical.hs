module IR.Constraint.Canonical
  ( Type(..)
  , Scheme(..)
  , FreeVars
  , Annotation(..)
  , Annotations(..)
  ) where

import qualified Common.Identifiers            as Ident
import qualified Data.Map.Strict               as Map
import qualified IR.IR                         as I
import           IR.Types.Type                  ( Annotation(..)
                                                , Annotations(..)
                                                , Type(..)
                                                )


-- | SCHEMES

data Scheme = Forall FreeVars I.Type

type FreeVars = Map.Map Ident.TVarId ()
