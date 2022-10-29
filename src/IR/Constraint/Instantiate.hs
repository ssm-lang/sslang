module IR.Constraint.Instantiate
  ( fromScheme
  ) where


import qualified Common.Identifiers            as Ident
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( (!) )

import qualified IR.Constraint.Constraint      as Constraint
import qualified IR.Constraint.Type            as Type



-- | FREE VARS

type FreeVars = Map.Map Ident.TVarId Constraint.Type


-- | FROM SCHEME

fromScheme :: FreeVars -> Type.Type -> IO Constraint.Type
fromScheme freeVars sourceType = case sourceType of
  Type.TCon tcon args ->
    Constraint.TConN tcon <$> mapM (fromScheme freeVars) args
  Type.TVar name -> return $ freeVars ! name
