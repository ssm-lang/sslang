module IR.Constraint.Constrain.Annotation where

import qualified Common.Identifiers            as Ident
import qualified Data.Map.Strict               as Map
import qualified IR.Constraint.Canonical       as Can
import           IR.Constraint.Monad            ( TC )
import           IR.Constraint.Type            as Type


-- | CONSTRAIN ANNOTATION

type RigidMap = Map.Map Ident.TVarId Variable

data State = State
  { _rigidMap :: RigidMap
  , _flex     :: [Variable]
  }

add :: Can.Annotation -> State -> TC (State, Type)
add ann state = case ann of
  Can.AnnType canType -> undefined
  Can.AnnDCon   _ _   -> error "No need for AnnDCon anymore."
  Can.AnnArrows _ _   -> undefined

emptyState :: State
emptyState = State Map.empty []
