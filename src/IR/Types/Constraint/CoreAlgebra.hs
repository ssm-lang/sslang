module IR.Types.Constraint.CoreAlgebra
  ( Term(..)
  , ARTerm(..)
  ) where

data Term a = App a a
            | Var a

data ARTerm a = TVariable a
              | TTerm (Term (ARTerm a))

instance Functor Term where
  fmap f t = case t of
    App l r -> App (f l) (f r)
    Var v   -> Var (f v)
