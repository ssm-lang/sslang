module Elaboration.AST where

import           Elaboration.Datatype           ( DataTypeEnv
                                                , LabelId
                                                , TyConId
                                                )

type TeVar = String
type TyVar = String

data Typ
  = TyVar TyVar
  | TyArrow Typ Typ
  | TyCon TyConId [Typ]
  deriving (Eq, Show)

data Rigidity = Flexible | Rigid
   deriving (Eq, Show)

data Term
 = Var TeVar
 | Apply Term Term
 -- ...
 deriving (Eq, Show)

type ASTDataTypeEnv = DataTypeEnv TyVar (Maybe Typ)
