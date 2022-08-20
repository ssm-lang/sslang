{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
module IR.Types.Unify where

import           IR.Types.Type

import qualified Common.Compiler               as Compiler
import           Common.Identifiers             ( TConId(..)
                                                , TVarId(..)
                                                )
import           Common.Pretty                  ( Pretty(..) )

import           Control.Monad                  ( (<=<)
                                                , when
                                                , zipWithM
                                                )
import           Control.Monad.State.Strict     ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                , gets
                                                , modify
                                                )
import           Control.Monad.Trans            ( MonadTrans(lift) )
import qualified Control.Monad.Trans.UnionFind as UF
import           Data.Functor                   ( (<&>) )
import qualified Data.Map.Strict               as M
import           Data.Maybe
import qualified Data.Set                      as S

{- | Monadic unification, parameterized handlers for unity and conflict.

@unifyM unify conflict t1 t2@ recursively traverses down @t1@ and @t2@ to try to
unify them. It reports conflicts using the @conflict@ handler, and unifies type
variables with other types using the @unify@ handler.

Note the asymmetry of @unify@'s type signature; this reflects the fact we never
need to unify type constructors with one another.

This function also encapsulates the handling of type holes (which the @unify@
and @conflict@ handlers should never need to handle). Since a 'Hole' represents
an anonymous (i.e., fresh and unique) type variable, they always unify with
anything. If unification is successful, this function returns @t1@ with all of
its type holes instantiated by @t2@ where possible.
-}
unifyM
  :: Monad m
  => (TVarId -> Type -> m ())
  -> (forall a . Type -> Type -> m a)
  -> Type
  -> Type
  -> m Type
unifyM unifier conflict = go
 where
  go t1@(TCon tcon1 ts1) t2@(TCon tcon2 ts2)
    | tcon1 == tcon2 = zipWithM go ts1 ts2 <&> TCon tcon1
    | otherwise      = conflict t1 t2
  go Hole         t2        = return t2
  go t1           Hole      = return t1 -- Even if t1 is a hole
  go t1@(TVar v1) t2        = unifier v1 t2 >> return t1
  go t1           (TVar v2) = unifier v2 t1 >> return t1

type Point = UF.Point TVarId

data UnifySt = UnifySt
  { points        :: M.Map TVarId Point
  , types         :: M.Map TVarId Type
  , _currentLevel :: Int
  }

type UnifyM' = UF.UnionFindT TVarId (StateT UnifySt Compiler.Pass)

type UnifyM = UnifyM'
-- newtype UnifyM a = UnifyM { unwrapUnify :: UnifyM' a}
--   deriving Functor via UnifyM
--   deriving Applicative via UnifyM
--   deriving Monad via UnifyM
--   deriving MonadFail via UnifyM
--   deriving (Compiler.MonadError Compiler.Error) via UnifyM
--   deriving (MonadState UnifySt) via UnifyM
--   deriving (Compiler.MonadWriter [Compiler.Warning]) via UnifyM

runUnify :: UnifyM a -> Compiler.Pass a
runUnify = (`evalStateT` UnifySt M.empty M.empty 0) . UF.runUnionFind -- . unwrapUnify

fresh :: TVarId -> UnifyM Point
-- fresh = UnifyM . UF.fresh
fresh = UF.fresh

repr :: Point -> UnifyM Point
-- repr = UnifyM . UF.repr
repr = UF.repr

descriptor :: Point -> UnifyM TVarId
-- descriptor = UnifyM . UF.descriptor
descriptor = UF.descriptor

equivalent :: Point -> Point -> UnifyM Bool
-- equivalent a b = UnifyM $ UF.equivalent a b
equivalent = UF.equivalent

union :: Point -> Point -> UnifyM ()
-- union a b = UnifyM $ UF.union a b
union = UF.union

makePoint :: TVarId -> UnifyM ()
makePoint v = do
  mp <- lift $ gets (M.lookup v . points)
  when (isJust mp) $ lift $ Compiler.unexpected "hi"
  -- Nothing <- gets (M.lookup v . points)
  p <- fresh v
  lift $ modify $ \st -> st { points = M.insert v p $ points st }
  -- modify $ \st -> st { points = M.insert v p $ points st }

makePointsFrom :: Foldable t => t TVarId -> UnifyM ()
makePointsFrom = mapM_ makePoint

getPoint :: TVarId -> UnifyM Point
getPoint v = lift $ do
-- getPoint v = do
  mp <- gets (M.lookup v . points)
  case mp of
    Just p  -> return p
    Nothing -> Compiler.unexpected "h[]"
  -- Just p <- gets (M.lookup v . points)
  -- return p

lookupType :: TVarId -> UnifyM (Maybe Type)
lookupType v = lift $ gets $ M.lookup v . types
-- lookupType v = gets $ M.lookup v . types

lookupTCon :: TVarId -> UnifyM (Maybe (TConId, [Type]))
lookupTCon v = do
  t <- lookupType v
  lift $ do
    case t of
      Just (TCon tc ts) -> return $ Just (tc, ts)
      Nothing -> return Nothing
      _ -> Compiler.unexpected $ "Found type in UnifyM types: " ++ show t

insertType :: TVarId -> Type -> UnifyM ()
insertType v t = lift $ modify $ \st -> st { types = M.insert v t $ types st }
-- insertType v t = modify $ \st -> st { types = M.insert v t $ types st }

currentLevel :: UnifyM Int
currentLevel = lift $ gets _currentLevel
-- currentLevel = gets _currentLevel

nextLevel :: UnifyM a -> UnifyM a
-- nextLevel m = lift $ do
nextLevel m = do
  lvl <- currentLevel
  lift $ modify $ \st -> st { _currentLevel = lvl + 1 }
  a <- m
  lift $ modify $ \st -> st { _currentLevel = lvl }
  return a

canonicalize :: S.Set TVarId -> UnifyM (S.Set TVarId)
canonicalize s =
  S.fromList <$> mapM (descriptor <=< repr <=< getPoint) (S.toList s)

typesConflict :: Type -> Type -> UnifyM a
typesConflict t1 t2 =
  lift
    $  Compiler.typeError
    $  "Types conflict:\n"
    ++ show (pretty t1)
    ++ "\n"
    ++ show (pretty t2)

unifyHoles :: Type -> Type -> UnifyM Type
unifyHoles = unifyM (const . const $ return ()) typesConflict

unifyWith :: (TVarId -> Type -> UnifyM ()) -> Type -> Type -> UnifyM Type
unifyWith = (`unifyM` typesConflict)
