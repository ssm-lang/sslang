module IR.Types.Constraint.CoreAlgebra
  ( Term(..)
  , ARTerm(..)
  , changeARTermVars
  , changeTermVars
  ) where
import           Data.Maybe                     ( fromMaybe )

data Term a = App a a
            | Var a
  deriving (Eq, Show)

data ARTerm a = TVariable a
              | TTerm (Term (ARTerm a))
  deriving (Eq, Show)

instance Functor Term where
  fmap f t = case t of
    App l r -> App (f l) (f r)
    Var v   -> Var (f v)

changeARTermVars :: Eq a => [(a, a)] -> ARTerm a -> ARTerm a
changeARTermVars c (TTerm     term) = TTerm (changeTermVars c term)
changeARTermVars c (TVariable x   ) = TVariable $ fromMaybe x (lookup x c)

changeTermVars :: Eq a => [(a, a)] -> Term (ARTerm a) -> Term (ARTerm a)
changeTermVars c = fmap (changeARTermVars c)
