module IR.Constraint.Canonical
  ( Type(..)
  , Scheme(..)
  , FreeVars
  , Annotation(..)
  , Annotations(..)
  , Kind
  , schemeOf
  , freeVars
  , foldArrow
  , unfoldArrow
  ) where

import qualified Common.Identifiers            as Ident
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified IR.IR                         as I
import           IR.Types.Type                  ( Annotation(..)
                                                , Annotations(..)
                                                , Type(..)
                                                , foldArrow
                                                , unfoldArrow
                                                )


-- | SCHEMES

data Scheme = Forall FreeVars I.Type

type FreeVars = Map.Map Ident.TVarId ()

-- | Construct a scheme from all free type variables and a trivial constraint.
schemeOf :: I.Type -> Scheme
schemeOf t = Forall (freeVars t) t

freeVars :: I.Type -> FreeVars
freeVars t =
  Map.fromList . map (\name -> (name, ())) . Set.toList $ Ident.freeVars t

-- | KINDS

-- no support for higher-kinded stuff yet, so Int suffices
type Kind = Int
