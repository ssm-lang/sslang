module IR.Constraint.Instantiate (
  fromScheme,
) where

import qualified Common.Identifiers as Ident
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map

import qualified IR.Constraint.Canonical as Can
import IR.Constraint.Monad (TC)
import qualified IR.Constraint.Type as Type


-- | FREE VARS
type FreeVars = Map.Map Ident.TVarId Type.Type


-- | FROM SCHEME
fromScheme :: FreeVars -> Can.Type -> TC Type.Type
fromScheme freeVars sourceType = case sourceType of
  Can.TCon tcon args -> Type.TConN tcon <$> mapM (fromScheme freeVars) args
  Can.TVar name -> return $ freeVars ! name
