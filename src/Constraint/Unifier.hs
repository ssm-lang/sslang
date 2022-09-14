module Constraint.Unifier
  ( Variable,
    Descriptor (..),
    Status (..),
    Mark,
    Rank,
    baseRank,
    freshMark,
    adjustRank,
    makeDesc,
    unify,
    freshId,
  )
where

import Constraint.SolverM (SolverM, currId, currMark)
import Constraint.Structure (Structure (..), isLeaf)
import Constraint.UnionFind
  ( Point,
    equivalent,
    union',
  )
import Constraint.Utils (throwTypeError, throwVariableScopeEscapeError)
import Control.Monad (unless, when)
import Control.Monad.ST.Trans (STRef, newSTRef, readSTRef, writeSTRef)
import Control.Monad.State.Class (get, put)

type Variable s = Point s (Descriptor s)

data Descriptor s = Descriptor
  { descId :: Int,
    descStructure :: STRef s (Maybe (Structure (Variable s))),
    descRank :: STRef s Rank,
    descStatus :: STRef s Status,
    descMark :: STRef s Mark
  }

data Status = Rigid | Flexible | Generic
  deriving (Eq, Show)

type Mark = Int

type Rank = Int

baseRank :: Int
baseRank = 0

freshId :: SolverM s Int
freshId = do
  ctx <- get
  put ctx {currId = currId ctx + 1}
  return $ currId ctx

freshMark :: SolverM s Mark
freshMark = do
  ctx <- get
  put ctx {currMark = currMark ctx + 1}
  return $ currMark ctx

dummyMark :: Mark
dummyMark = 0

adjustRank :: Descriptor s -> Rank -> SolverM s ()
adjustRank d k = do
  st <- readSTRef $ descStatus d
  unless (st /= Generic) $ throwTypeError "adjustRank: equivalence class cannot have Generic status"
  oldk <- readSTRef $ descRank d
  if k < oldk
    then do
      unless (st == Flexible) throwVariableScopeEscapeError
      writeSTRef (descRank d) k
    else return ()

makeDesc :: Maybe (Structure (Variable s)) -> Rank -> Status -> SolverM s (Descriptor s)
makeDesc so rank status = do
  so' <- newSTRef so
  rank' <- newSTRef rank
  status' <- newSTRef status
  i <- freshId
  mark <- newSTRef dummyMark
  return
    Descriptor
      { descId = i,
        descStructure = so',
        descRank = rank',
        descStatus = status',
        descMark = mark
      }

-- | Unification Queue
-- | TODO
type Queue s = STRef s [(Variable s, Variable s)]

pop :: Queue s -> SolverM s (Maybe (Variable s, Variable s))
pop qref = do
  q <- readSTRef qref
  case q of
    [] -> return Nothing
    (x : xs) -> do
      writeSTRef qref xs
      return $ Just x

insert :: Queue s -> Variable s -> Variable s -> SolverM s ()
insert qref v1 v2 = do
  q <- readSTRef qref
  iseq <- equivalent v1 v2
  unless iseq $ writeSTRef qref ((v1, v2) : q)

unifyDesc ::
  Queue s ->
  Descriptor s ->
  Descriptor s ->
  SolverM s (Descriptor s)
unifyDesc qref d1 d2 = do
  let i = descId d1
  struc <- unifyStructure
  struc' <- newSTRef struc
  rank <- unifyRank
  rank' <- newSTRef rank
  status <- unifyStatus rank struc
  status' <- newSTRef status
  mark <- newSTRef dummyMark
  return
    Descriptor
      { descId = i,
        descStructure = struc',
        descRank = rank',
        descStatus = status',
        descMark = mark
      }
  where
    unifyStructure = do
      s1 <- readSTRef (descStructure d1)
      s2 <- readSTRef (descStructure d2)
      conjunction (insert qref) s1 s2
    unifyRank = do
      r1 <- readSTRef (descRank d1)
      r2 <- readSTRef (descRank d2)
      return $ min r1 r2
    unifyStatus rank struc = do
      s1 <- readSTRef (descStatus d1)
      r1 <- readSTRef (descRank d1)
      s2 <- readSTRef (descStatus d2)
      r2 <- readSTRef (descRank d2)
      let oneRigidCase rigidRank = do
            when (rank < rigidRank) $ throwTypeError "can't lower rank of a rigid variable"
            unless (isLeaf struc) $ throwTypeError "can't assign non-leaf structure to a rigid variable"
            return Rigid
      case (s1, r1, s2, r2) of
        (Generic, _, _, _) -> throwTypeError "can't unify generic type variables"
        (_, _, Generic, _) -> throwTypeError "can't unify generic type variables"
        (Flexible, _, Flexible, _) -> return Flexible
        (Rigid, _, Rigid, _) -> throwTypeError "can't unify two rigid type variables"
        (Rigid, rigidRank, Flexible, _) -> oneRigidCase rigidRank
        (Flexible, _, Rigid, rigidRank) -> oneRigidCase rigidRank

unify' :: Queue s -> Variable s -> Variable s -> SolverM s ()
unify' qref v1 v2 = do
  -- desc <- unifyDesc qref v1 v2
  union' v1 v2 (unifyDesc qref)
  unifyPending qref

unifyPending :: Queue s -> SolverM s ()
unifyPending qref = do
  x <- pop qref
  case x of
    Just (v1, v2) -> unify' qref v1 v2
    Nothing -> return ()

unify :: Variable s -> Variable s -> SolverM s ()
unify v1 v2 = do
  qref <- newSTRef []
  unify' qref v1 v2

conjunction ::
  (Variable s -> Variable s -> SolverM s ()) ->
  Maybe (Structure (Variable s)) ->
  Maybe (Structure (Variable s)) ->
  SolverM s (Maybe (Structure (Variable s)))
conjunction _ Nothing so = return so
conjunction _ so Nothing = return so
conjunction f (Just s1) (Just s2) = Just <$> conjunction' f s1 s2

conjunction' ::
  (Variable s -> Variable s -> SolverM s ()) ->
  Structure (Variable s) ->
  Structure (Variable s) ->
  SolverM s (Structure (Variable s))
conjunction' f s1@(TyConS m ls) (TyConS n rs)
  | m /= n = throwTypeError $ "unable to unify " ++ show m ++ " with " ++ show n
  | otherwise = do
    conjunctionList f ls rs
    return s1

conjunctionList ::
  (Variable s -> Variable s -> SolverM s ()) ->
  [Variable s] ->
  [Variable s] ->
  SolverM s ()
conjunctionList f l1 l2
  | length l1 /= length l2 = throwTypeError "unable to unify due to different number of type constructor parameters"
  | otherwise = mapM_ (uncurry f) (zip l1 l2)
