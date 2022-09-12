module Constraint.Unifier where

import Constraint.SolverM (SolverM)
import Constraint.Structure (Structure (..))
import qualified Constraint.UnionFind as UF
import Constraint.Utils (throwTypeError, throwVariableScopeEscapeError)
import Control.Monad (unless)
import Control.Monad.ST.Trans (STRef, newSTRef, readSTRef, writeSTRef)

type Variable s = UF.Point s (Descriptor s)

data Descriptor s = Descriptor
  { descId :: Int,
    descStructure :: STRef s (Maybe (Structure (Variable s))),
    descRank :: STRef s Rank,
    descStatus :: STRef s Status,
    descMark :: STRef s (Mark s)
  }

data Status = Rigid | Flexible | Generic
  deriving (Eq, Show)

type Mark s = STRef s Int

type Rank = Int

baseRank :: Integer
baseRank = 0

freshMark :: SolverM s (Mark s)
freshMark = newSTRef 0

adjustRank :: Descriptor s -> Rank -> SolverM s ()
adjustRank d k = do
  st <- readSTRef $ descStatus d
  unless (st /= Generic) $ throwTypeError "adjustRank: Equivalence class cannot have Generic status"
  oldk <- readSTRef $ descRank d
  if k < oldk
    then do
      unless (st == Flexible) throwVariableScopeEscapeError
      writeSTRef (descRank d) k
    else return ()

type Queue s = [(Variable s, Variable s)]

insert :: Queue s -> Variable s -> Variable s -> SolverM s (Queue s)
insert q v1 v2 = do
  iseq <- UF.equivalent v1 v2
  if iseq
    then return q
    else return ((v1, v2) : q)

-- unifyDesc :: Queue s -> Descriptor s -> Descriptor s ->

unify :: Variable s -> Variable s -> SolverM s ()
unify = undefined
