{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveTraversable #-}
{- | Type system from the constraint solver's perspective

The constraint system works with flat types (non-recursive types),
constraints and variables (represented with integers).
-}

module IR.Types.Constraint.Type
  ( UVar
  , UType(..)
  , Co(..)
  , Status(..)
  , Mark
  , dummyMark
  , Structure
  , Rank
  , outermostRank
  , Descriptor(..)
  , makeDescriptor
  , setRank
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
  , initInferCtx
  , getDecoderMap
  , putDecoderMap
  , getPool
  , getYoungRank
  , decYoungRank
  , incYoungRank
  , makeUVar
  , getDescriptor
  , setMark
  , setStatus
  , UScheme(..)
  , setStructure
  ) where

import qualified Common.Compiler               as Compiler
import           Common.Identifiers             ( DConId(..)
                                                , TConId(..)
                                                , TVarId(..)
                                                , VarId(..)
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
import qualified IR.Types.Constraint.InfiniteArray
                                               as IA
import qualified IR.Types.Constraint.UnionFind as UF
import           IR.Types.Type                  ( Annotations
                                                , Scheme
                                                , Type
                                                )



-- | Unificiation variable, represented as a point in union-find


type UVar s = UF.Point s (Descriptor s)

makeUVar :: Descriptor s -> Infer s (UVar s)
makeUVar = UF.fresh

getDescriptor :: UVar s -> Infer s (Descriptor s)
getDescriptor = UF.descriptor

-- | Flat type for unification


data UType a = UTCon TConId [a]
  deriving (Eq, Show, Functor, Foldable, Traversable)



-- | Scheme in the unification world

data UScheme s = UScheme
  { uschemeRoot        :: UVar s
  , uschemeGenerics    :: [UVar s]
  , uschemeQuantifiers :: [UVar s]
  }


-- | Structure is either a leaf (Nothing) or a flat type (Just UType)


type Structure a = Maybe (UType a)

leaf :: Structure a
leaf = Nothing

isLeaf :: Structure a -> Bool
isLeaf = isNothing

projectNonLeaf :: Structure a -> UType a
projectNonLeaf = fromJust



-- | Constraints

newtype CVar = CVar Int
 deriving (Eq, Show)

data Co a where
  CTrue     :: Co ()
  CMap      :: Co a -> (a -> b) -> Co b
  CPure     :: a -> Co a
  CConj     :: Co a -> Co b -> Co (a, b)
  CEq       :: CVar -> CVar -> Co ()
  CExist    :: CVar -> Maybe (UType CVar) -> Co a -> Co a
  CDecode   :: CVar -> Co Type
  CInstance :: VarId -> CVar -> Co (Scheme, [Type])
  CDef      :: VarId -> CVar -> Co a -> Co a
  CLet      :: [CVar] -> [VarId] -> [CVar] -> Co a -> Co b -> Co ([TVarId], [Scheme], a, b)

instance Show (Co a) where
  show CTrue             = "(CTrue)"
  show (CMap c _       ) = "(CMap " ++ show c ++ ")"
  show (CPure _        ) = "(CPure)"
  show (CConj c1 c2    ) = "(" ++ show c1 ++ " ^& " ++ show c2 ++ ")"
  show (CEq   v1 v2    ) = "(CEq " ++ show v1 ++ " " ++ show v2 ++ ")"
  show (CExist v _ c   ) = "(CExist " ++ show v ++ " " ++ show c ++ ")"
  show (CDecode v      ) = "(CDecode " ++ show v ++ ")"
  show (CInstance vid v) = "(CInstance " ++ show vid ++ " " ++ show v ++ ")"
  show (CDef vid v c) =
    "(CDef " ++ show vid ++ " " ++ show v ++ " " ++ show c ++ ")"
  show (CLet vs vids vs' c1 c2) =
    "(CLet "
      ++ show vs
      ++ " "
      ++ show vids
      ++ " "
      ++ show vs'
      ++ " "
      ++ show c1
      ++ " "
      ++ show c2
      ++ ")"

instance Functor Co where
  fmap f c = CMap c f

instance Applicative Co where
  pure = CPure
  mf <*> mx = fmap (\(f, x) -> f x) (CConj mf mx)


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
setRank :: Descriptor s -> Rank -> Infer s ()
setRank d k = do
  st <- getStatus d
  unless (st /= Generic) $ throwError mismatchError
  oldk <- readSTRef $ _descRank d
  when (k < oldk) $ do
    unless (st == Flexible) $ throwError scopeEscapeError
    writeSTRef (_descRank d) k

setMark :: Descriptor s -> Mark -> Infer s ()
setMark d = writeSTRef (_descMark d)

setStatus :: Descriptor s -> Status -> Infer s ()
setStatus d = writeSTRef (_descStatus d)

setStructure :: Descriptor s -> Structure (UVar s) -> Infer s ()
setStructure d = writeSTRef (_descStructure d)


-- | Infer Monad


type Infer s a
  = STT s (StateT (InferCtx s) (ExceptT Compiler.Error Compiler.Pass)) a

-- | Internal state used during the entire inference procedure
data InferCtx s = InferCtx
  {
    -- General
    _prog       :: Program Annotations

    -- Generation
  , _dconEnv    :: M.Map DConId DConInfo

    -- Solver
  , _tag        :: Int
  , _mark       :: Int
  , _tvarId     :: Int

    -- Generalization
  , _pool       :: IA.InfiniteArray s [UVar s]
  , _youngRank  :: Rank

    -- Decoder
  , _decoderMap :: M.Map Int Type
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

getPool :: Infer s (IA.InfiniteArray s [UVar s])
getPool = _pool <$> get

getYoungRank :: Infer s Rank
getYoungRank = _youngRank <$> get

decYoungRank :: Infer s ()
decYoungRank = do
  ctx <- get
  put ctx { _youngRank = _youngRank ctx - 1 }

incYoungRank :: Infer s ()
incYoungRank = do
  ctx <- get
  put ctx { _youngRank = _youngRank ctx + 1 }

getDecoderMap :: Infer s (M.Map Int Type)
getDecoderMap = _decoderMap <$> get

putDecoderMap :: M.Map Int Type -> Infer s ()
putDecoderMap m = do
  ctx <- get
  put ctx { _decoderMap = m }

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
