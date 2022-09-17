module Elaboration.Datatype where

type TyConId = String

type LabelId = String

data DataTypeEnv a b = DataTypeEnv
  { datatypes :: a,
    labels :: b
  }
