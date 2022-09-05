module IR.Types.Constraint.Constraint
  ( SName(..)
  , TypeConstraint(..)
  , Rigidity(..)
  , Variable
  , CRTerm
  , TConstraint
  , TScheme
  , (<?)
  , (==?)
  , (^?)
  , conj
  , ex
  , fl
  , exists
  , exists3
  , existsList
  , forallList
  , existsSet
  , monoScheme
  , scheme
  , scheme'
  ) where

import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           IR.Types.Constraint.CoreAlgebra
                                                ( ARTerm(..) )
import           IR.Types.Constraint.Inference  ( InferM )
import           IR.Types.Constraint.Misc       ( foldrM )
import           IR.Types.Constraint.MultiEquation
                                                ( CRTerm
                                                , Rigidity(..)
                                                , Structure
                                                , TName(..)
                                                , Variable
                                                , variable
                                                , variableList
                                                , variableSet
                                                )

newtype SName = SName String -- TODO: what type should I use here?
  deriving (Eq, Show)

data TypeConstraint c v = CTrue
                          | CDump
                          | CEquation c c
                          | CConjunction [TypeConstraint c v]
                          | CLet [Scheme c v] (TypeConstraint c v)
                          | CInstance SName c
                          | CDisjunction [TypeConstraint c v]

data Scheme c v = Scheme [v] [v] (TypeConstraint c v) (M.Map String c)

type TConstraint s = TypeConstraint (CRTerm s) (Variable s)

type TScheme s = Scheme (CRTerm s) (Variable s)

(<?) :: SName -> c -> TypeConstraint c v
x <? t = CInstance x t

(==?) :: c -> c -> TypeConstraint c v
t1 ==? t2 = CEquation t1 t2

(^?) :: TypeConstraint c v -> TypeConstraint c v -> TypeConstraint c v
CTrue ^? c                 = c
c     ^? CTrue             = c
c     ^? (CConjunction cs) = CConjunction (c : cs)
c1    ^? c2                = CConjunction [c1, c2]

conj :: Foldable t => t (TypeConstraint c v) -> TypeConstraint c v
conj = foldl (^?) CTrue

ex :: [v] -> TypeConstraint c v -> TypeConstraint c v
ex qs c = CLet [Scheme [] qs c M.empty] CTrue

fl :: [v] -> TypeConstraint c v -> TypeConstraint c v
fl qs c = CLet [Scheme qs [] c M.empty] CTrue

exists :: (CRTerm s -> InferM s (TConstraint s)) -> InferM s (TConstraint s)
exists f = do
  v <- variable Flexible Nothing Nothing
  c <- f $ TVariable v
  return $ ex [v] c

exists3
  :: (CRTerm s -> CRTerm s -> CRTerm s -> InferM s (TConstraint s))
  -> InferM s (TConstraint s)
exists3 f = exists (\x -> exists (\y -> exists (\z -> f x y z)))

existsList
  :: [a] -> ([(a, CRTerm s)] -> TConstraint s) -> InferM s (TConstraint s)
existsList l f = do
  (l, m) <- variableList Flexible l
  return $ ex l (f m)

forallList
  :: [TName]
  -> ([(TName, CRTerm s)] -> TConstraint s)
  -> InferM s (TConstraint s)
forallList l f =
  let g x (vs, xts) = do
        v <- variable Rigid (Just x) Nothing
        return (v : vs, (x, TVariable v) : xts)
  in  do
        (l', m) <- foldrM g ([], []) l
        return $ fl l' (f m)

existsSet
  :: S.Set String
  -> (M.Map String (CRTerm s) -> TConstraint s)
  -> InferM s (TConstraint s)
existsSet names f = do
  (l, m) <- variableSet (const (Flexible, Nothing)) names
  return $ ex l (f m)

monoScheme :: M.Map String c -> Scheme c v
monoScheme = Scheme [] [] CTrue

scheme
  :: [Variable s]
  -> S.Set String
  -> (M.Map String (CRTerm s) -> TConstraint s)
  -> InferM s (TScheme s)
scheme rqs names f = do
  (l, m) <- variableSet (const (Flexible, Nothing)) names
  return $ Scheme rqs l (f m) m

scheme'
  :: [Variable s]
  -> S.Set String
  -> S.Set String
  -> (M.Map String (CRTerm s) -> TConstraint s)
  -> InferM s (TScheme s)
scheme' rqs rnames fnames f = do
  (fls, fm) <- variableSet (const (Flexible, Nothing)) fnames
  (rls, rm) <- variableSet (\v -> (Rigid, Just v)) rnames
  let m = M.union fm rm
  return $ Scheme (rqs ++ rls) fls (f m) m
