module Constraint.Datatype where

import           Common.Identifiers             ( DConId
                                                , TConId(..)
                                                , TVarId(..)
                                                )
import qualified Data.Map                      as M
import qualified Front.Ast                     as A

type AstEnv = Env TVarId A.Typ

data Env b t = Env
  { envDatatypes :: M.Map TConId (Decl b t)
  , envLabels    :: M.Map DConId (Label t)
  }

data DeclKind = Variant | Record

data Decl b t = Decl
  { declName   :: TConId
  , declParams :: [b]
  , declKind   :: DeclKind
  , declLabels :: [Label t]
  }

data Label t = Label
  { labelName     :: DConId
  , labelTypeName :: TConId
  , labelParams   :: [t]
  }

emptyEnv :: Env b t
emptyEnv = Env { envDatatypes = M.empty, envLabels = M.empty }
