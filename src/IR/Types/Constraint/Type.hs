{-# OPTIONS_GHC -funbox-strict-fields #-}
{- | Type system from the constraint solver's perspective

The constraint system works with flat types (non-recursive types),
constraints and variables (represented with integers).
-}

module IR.Types.Constraint.Type
  ( UVar
  , UType(..)
  -- , Co(..)
  , Status(..)
  , Mark
  , dummyMark
  , Structure
  , Rank
  , outermostRank
  , Descriptor(..)
  , Point(..)
  , Link(..)
  , Info(..)
  , makeDescriptor
  , adjustRank
  , Infer
  , occursError
  , mismatchError
  , genericError
  , getStatus
  , getRank
  , getMark
  , getStructure
  , getTag
  , freshMark
  , projectNonLeaf
  , isLeaf
  , leaf
  , internalError
  , initInferCtx) where

import qualified Common.Compiler               as Compiler
import           Common.Identifiers             ( DConId(..)
                                                , TConId(..)
                                                , TVarId(..)
                                                )
import           Control.Monad.ST.Trans         ( STRef
                                                , STT
                                                , newSTRef
                                                , readSTRef
                                                , writeSTRef
                                                )
import           Control.Monad.State.Lazy       ( StateT(..)
                                                , get
                                                , put
                                                )
import qualified Data.Map                      as M

import           Common.Compiler                ( fromString )
import qualified Constraint.InfiniteArray      as IA
import           Control.Monad                  ( unless
                                                , when
                                                )
import           Control.Monad.Except           ( ExceptT
                                                , throwError
                                                )
import           Data.Maybe                     ( fromJust
                                                , isNothing
                                                )
import           IR.IR                          ( Program(..) )
import           IR.Types.Type                  ( Annotations
                                                , Type
                                                )


-- | Union find data types


-- | The abstract type of an element of the sets we work on.  It is
-- parameterised over the type of the descriptor.
newtype Point s a = Pt (STRef s (Link s a)) deriving (Eq)

data Link s a
  = -- | This is the descriptive element of the equivalence class.
    Info {-# UNPACK #-} !(STRef s (Info a))
  | -- | Pointer to some other element of the equivalence class.
    Link {-# UNPACK #-} !(Point s a)
  deriving (Eq)

data Info a = MkInfo
  { -- | The size of the equivalence class, used by 'union'.
    weight :: {-# UNPACK #-} !Int
  , descr  :: a
  }
  deriving Eq



-- | Unificiation variable, represented as a point in union-find


type UVar s = Point s (Descriptor s)



-- | Flat type for unification


data UType a = UTCon TConId [a]



-- | Structure is either a leaf (Nothing) or a flat type (Just UType)


type Structure a = Maybe (UType a)

leaf :: Structure a
leaf = Nothing

isLeaf :: Structure a -> Bool
isLeaf = isNothing

projectNonLeaf :: Structure a -> UType a
projectNonLeaf = fromJust



-- | Constraints


-- data Co a where
--   CTrue         :: Co ()
--   CMap          :: Co a -> (a -> b) -> Co b
--   CPure         :: a -> Co a
--   CConj         :: Co a -> Co b -> Co (a, b)
--   CEq           :: UVar -> UVar -> Co ()
--   CExist        :: UVar -> Maybe (UType UVar) -> Co a -> Co a
--   CDecode       :: UVar -> Co Type
--   CInstance     :: VarId -> UVar -> Co (Scheme, [Type])
--   CDef          :: VarId -> UVar -> Co a -> Co a
--   CLet          :: [UVar] -> [VarId] -> [UVar] -> Co a -> Co b -> Co ([TVarId], [Scheme], a, b)



-- | Unification variable status


data Status = Rigid | Flexible | Generic
  deriving (Eq, Show)



-- | Mark, used internally to mark visited nodes


type Mark = Int

dummyMark :: Mark
dummyMark = 0



-- | Rank of unification variables, denoting the depth into the CLet constraints


type Rank = Int

outermostRank :: Int
outermostRank = 0



-- | Unification point descriptor


data Descriptor s = Descriptor
  { _descTag       :: Int
  , _descStructure :: STRef s (Structure (UVar s))
  , _descRank      :: STRef s Rank
  , _descStatus    :: STRef s Status
  , _descMark      :: STRef s Mark
  }

makeDescriptor
  :: Maybe Int -> Structure (UVar s) -> Rank -> Status -> Infer s (Descriptor s)
makeDescriptor tagOpt s rank status = do
  tag'    <- maybe freshTag return tagOpt
  s'      <- newSTRef s
  rank'   <- newSTRef rank
  status' <- newSTRef status
  mark'   <- newSTRef dummyMark
  return Descriptor { _descTag       = tag'
                    , _descStructure = s'
                    , _descRank      = rank'
                    , _descStatus    = status'
                    , _descMark      = mark'
                    }

-- get
getTag :: Descriptor s -> Int
getTag = _descTag

getStructure :: Descriptor s -> Infer s (Structure (UVar s))
getStructure d = readSTRef $ _descStructure d

getRank :: Descriptor s -> Infer s Rank
getRank d = readSTRef $ _descRank d

getStatus :: Descriptor s -> Infer s Status
getStatus d = readSTRef $ _descStatus d

getMark :: Descriptor s -> Infer s Mark
getMark d = readSTRef $ _descMark d

-- set
adjustRank :: Descriptor s -> Rank -> Infer s ()
adjustRank d k = do
  st <- getStatus d
  unless (st /= Generic) $ throwError mismatchError
  oldk <- readSTRef $ _descRank d
  when (k < oldk) $ do
    unless (st == Flexible) $ throwError scopeEscapeError
    writeSTRef (_descRank d) k



-- | Infer Monad


type Infer s a
  = STT s (StateT (InferCtx s) (ExceptT Compiler.Error Compiler.Pass)) a

-- | Internal state used during the entire inference procedure
data InferCtx s = InferCtx
  {
    -- General
    _prog    :: Program Annotations

    -- Generation
  , _dconEnv :: M.Map DConId DConInfo

    -- Solver
  , _tag     :: Int
  , _mark    :: Int
  , _tvarId  :: Int

    -- Generalization
  , _pool    :: IA.InfiniteArray s [UVar s]
  , _young   :: Int
  }

type DConInfo = (DConId, TConId, [TVarId], [Type])

initInferCtx :: InferCtx s
initInferCtx = undefined

freshTag :: Infer s Mark
freshTag = do
  ctx <- get
  put ctx { _tag = _tag ctx + 1 }
  return $ _tag ctx

freshMark :: Infer s Mark
freshMark = do
  ctx <- get
  put ctx { _mark = _mark ctx + 1 }
  return $ _mark ctx



-- | Error reporting in Infer monad


internalError :: String -> Compiler.Error
internalError msg = Compiler.TypeError $ fromString msg

occursError :: Compiler.Error
occursError = Compiler.TypeError $ fromString "Infinite type"

mismatchError :: Compiler.Error
mismatchError = Compiler.TypeError $ fromString "Unable to unify"

genericError :: Compiler.Error
genericError = Compiler.TypeError
  $ fromString "Generic unification variables cannot be unified"

scopeEscapeError :: Compiler.Error
scopeEscapeError =
  Compiler.TypeError $ fromString "Unification variable scope escape"
