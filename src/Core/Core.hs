module Core.Core where

data Typ a b
  = TyVar a
  | TyArrow (Typ a b) (Typ a b)
  | TyProduct (Typ a b) (Typ a b)
  | TyForall b (Typ a b)
  | TyMu b (Typ a b)
  deriving (Eq, Show)

type TyVar = Int

type NominalType = Typ TyVar TyVar

type TeVar = String

data Term a b
  = Var TeVar
  | Abs TeVar (Typ a b) (Typ a b)
  | App (Term a b) (Term a b)
  | Let TeVar (Term a b) (Term a b)
  | TyAbs b (Term a b)
  | TyApp (Term a b) (Typ a b)
  | Tuple [Term a b]
  | Proj Int (Term a b)
  | LetProd [TeVar] (Term a b) (Term a b)
  | Variant
  deriving (Eq, Show)

type NominalTerm = Term TyVar TyVar
