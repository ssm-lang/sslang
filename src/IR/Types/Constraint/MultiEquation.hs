module IR.Types.Constraint.MultiEquation where

import           Control.Monad                  ( foldM )
import           Control.Monad.ST.Trans
import qualified Data.Map                      as M
import           Data.Maybe                     ( isNothing )
import qualified Data.Set                      as S
import           IR.Types.Constraint.CoreAlgebra
import           IR.Types.Constraint.Inference  ( InferM )
import qualified IR.Types.Constraint.IntRank   as Rank
import           IR.Types.Constraint.Misc       ( foldrM
                                                , modifySTRef
                                                )
import qualified IR.Types.Constraint.UnionFind as UF

type Variable s = UF.Point s (Descriptor s)

data Descriptor s = Descriptor
  { structure :: STRef s (Maybe (Structure s))
  , rank      :: STRef s Rank.Rank
  , rigidity  :: STRef s Rigidity
  , name      :: STRef s (Maybe TName)
  }

type Structure s = Term (Variable s)

data Rigidity = Rigid | Flexible | Constant
  deriving (Eq, Show)

newtype TName = TName String -- TODO: what type should I use here?
  deriving (Eq, Show)

type CRTerm s = ARTerm (Variable s)

data Pool s = Pool
  { number      :: Int
  , inhabitants :: STRef s [Variable s]
  }

isRigid :: Variable s -> InferM s Bool
isRigid v = do
  d <- UF.descriptor v
  r <- readSTRef $ rigidity d
  return $ r == Rigid

isFlexible :: Variable s -> InferM s Bool
isFlexible v = do
  d <- UF.descriptor v
  r <- readSTRef $ rigidity d
  return $ r == Flexible

new :: Pool s -> InferM s (Pool s)
new pool = do
  inh <- newSTRef []
  return $ Pool { number = number pool, inhabitants = inh }

init :: InferM s (Pool s)
init = do
  inh <- newSTRef []
  return $ Pool { number = Rank.outermost, inhabitants = inh }

register :: Pool s -> Variable s -> InferM s ()
register pool v = modifySTRef (inhabitants pool) (v :)

-- TODO: undefined right now
introduce :: Pool s -> Variable s -> InferM s (Variable s)
introduce pool v = undefined

-- TODO: undefined right now
chop :: Pool s -> CRTerm s -> InferM s (Variable s)
chop pool (TVariable v) = undefined
chop pool (TTerm     v) = undefined

-- TODO: undefined right now
chopi :: Rank.Rank -> CRTerm s -> InferM s (Variable s)
chopi rank term = undefined

variableName :: Variable s -> InferM s (Maybe TName)
variableName v = do
  d <- UF.descriptor v
  readSTRef $ name d

isStructured :: Variable s -> InferM s Bool
isStructured v = do
  d <- UF.descriptor v
  s <- readSTRef $ structure d
  return $ isNothing s

variableStructure :: Variable s -> InferM s (Maybe (Structure s))
variableStructure v = do
  d <- UF.descriptor v
  readSTRef $ structure d

isRedundant :: Variable s -> InferM s Bool
isRedundant = UF.redundant

areEquivalent :: Variable s -> Variable s -> InferM s Bool
areEquivalent = UF.equivalent

poolInhabitants :: Pool s -> InferM s [Variable s]
poolInhabitants pool = do
  readSTRef $ inhabitants pool

poolNumber :: Pool s -> InferM s Int
poolNumber pool = return $ number pool

variable
  :: Rigidity -> Maybe TName -> Maybe (CRTerm s) -> InferM s (Variable s)
variable rig n s = do
  s' <- case s of
    Just t -> do
      v <- chopi Rank.none t
      return $ Just (Var v)
    Nothing -> return Nothing
  s''  <- newSTRef s'
  rig' <- newSTRef rig
  n'   <- newSTRef n
  r'   <- newSTRef Rank.none
  UF.fresh
    $ Descriptor { structure = s'', rank = r', name = n', rigidity = rig' }

variableList :: Rigidity -> [a] -> InferM s ([Variable s], [(a, CRTerm s)])
variableList rig xs =
  let f x (vs, xts) = do
        v <- variable rig Nothing Nothing
        return (v : vs, (x, TVariable v) : xts)
  in  foldrM f ([], []) xs

variableListFromNames
  :: (a -> (Rigidity, Maybe TName))
  -> [a]
  -> InferM s ([Variable s], [(a, CRTerm s)])
variableListFromNames getRig xs =
  let f x (vs, xts) = do
        let (rig, n) = getRig x
        v <- variable rig n Nothing
        return (v : vs, (x, TVariable v) : xts)
  in  foldrM f ([], []) xs

variableSet
  :: (TName -> (Rigidity, Maybe TName))
  -> S.Set String
  -> InferM s ([Variable s], M.Map String (CRTerm s))
variableSet getRig xs =
  let f (vs, xts) x = do
        let (rig, n) = getRig (TName x)
        v <- variable rig n Nothing
        return (v : vs, M.insert x (TVariable v) xts)
  in  foldM f ([], M.empty) xs
