{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module IR.Types.Unification
  ( UType
  , UScheme
  , pattern UTCon
  , pattern UTVar
  , freeze
  , unfreeze
  , freezeScheme
  , unfreezeScheme
  , Infer
  , Ctx
  , lookupType
  , withBinding
  , (=:=)
  , generalize
  , instantiate
  , runInfer
  ) where


import qualified Common.Compiler               as Compiler
import           Common.Identifiers             ( TConId(..)
                                                , TVarId(..)
                                                , fromString
                                                , genId
                                                )
import qualified IR.Types.Type                 as T
import           IR.Types.Type                  ( SchemeOf(Forall) )

import           Control.Unification     hiding ( (=:=)
                                                , applyBindings
                                                , freeze
                                                , unfreeze
                                                )
import qualified Control.Unification           as U
import           Control.Unification.IntVar

import           Control.Arrow                  ( (>>>) )
import           Control.Monad                  ( forM )
import           Control.Monad.Except           ( ExceptT
                                                , MonadError(..)
                                                , runExceptT
                                                )
import           Control.Monad.Identity         ( Identity(runIdentity) )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(runReaderT)
                                                )
import           Control.Monad.Trans            ( MonadTrans(..) )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Set                      as S
import           Data.Set                       ( (\\) )
import           GHC.Generics                   ( Generic1 )

deriving instance Ord IntVar
deriving instance Unifiable []

data TypeF a = TConF TConId [a] | TVarF TVarId
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic1, Unifiable)

type UType = UTerm TypeF IntVar
type UScheme = T.SchemeOf UType

pattern UTCon :: TConId -> [UType] -> UType
pattern UTCon tc ts = UTerm (TConF tc ts)

pattern UTVar :: TVarId -> UType
pattern UTVar v = UTerm (TVarF v)

-- | Promote a 'T.Type' to a 'UType'.
unfreeze :: T.Type -> UType
unfreeze (T.TCon tc ts) = UTerm $ TConF tc $ fmap unfreeze ts
unfreeze (T.TVar v    ) = UTerm $ TVarF v

-- | Demote a 'UType' to a regular 'T.Type'.
--
-- Fails (i.e., returns 'Nothing') if the unification type contains more than
-- just contructors and variables.
freeze :: UType -> Maybe T.Type
freeze (UTCon tc ts) = mapM freeze ts >>= Just . T.TCon tc
freeze (UTVar v    ) = Just $ T.TVar v
freeze _             = Nothing

unfreezeScheme :: T.Scheme -> UScheme
unfreezeScheme (T.Scheme s) = fmap unfreeze s

freezeScheme :: UScheme -> Maybe T.Scheme
freezeScheme s = T.Scheme <$> mapM freeze s

-- | Catamorphism over unification types; useful for term rewriting.
ucata :: Functor t => (v -> a) -> (t a -> a) -> UTerm t v -> a
ucata f _ (UVar  v) = f v
ucata f g (UTerm t) = g (fmap (ucata f g) t)

substU :: M.Map (Either TVarId IntVar) UType -> UType -> UType
substU m = ucata f g
 where
  f v = fromMaybe (UVar v) $ M.lookup (Right v) m
  g (TVarF v) = fromMaybe (UTVar v) (M.lookup (Left v) m)
  g t         = UTerm t

instance Fallible TypeF IntVar Compiler.Error where
  occursFailure v t =
    Compiler.TypeError
      $  fromString
      $  "Infinite type: "
      <> show v
      <> " ~ "
      <> show t
  mismatchFailure t1 t2 =
    Compiler.TypeError
      $  fromString
      $  "Cannot unify: "
      <> show t1
      <> " ~ "
      <> show t2

class HasFreeVars a where
   freeVars :: a -> Infer (S.Set IntVar)

instance HasFreeVars UType where
  freeVars = fmap S.fromList . lift . lift . getFreeVars

instance HasFreeVars UScheme where
  freeVars (Forall _ _ t) = freeVars t

instance HasFreeVars Ctx where
  freeVars = fmap S.unions . mapM freeVars . M.elems

type Ctx = M.Map TVarId UScheme

lookupType :: TVarId -> Infer UType
lookupType x = ask >>= maybe (throwError unbound) instantiate . M.lookup x
 where
  unbound = Compiler.TypeError $ fromString $ "Unbound variable: " <> show x

withBinding :: MonadReader Ctx m => TVarId -> UScheme -> m a -> m a
withBinding x ty = local (M.insert x ty)

-- | Inference monad, build on top of the unification algorithm.
type Infer = ReaderT Ctx (ExceptT Compiler.Error (IntBindingT TypeF Identity))

fresh :: Infer UType
fresh = UVar <$> lift (lift freeVar)

(=:=) :: UType -> UType -> Infer UType
s =:= t = lift $ s U.=:= t

applyBindings :: UType -> Infer UType
applyBindings = lift . U.applyBindings

instantiate :: UScheme -> Infer UType
instantiate (Forall (S.toList -> xs) _ uty) = do
  subs <- forM xs $ \v -> do
    fv <- fresh
    return (Left v, fv)
  return $ substU (M.fromList subs) uty

generalize :: UType -> Infer UScheme
generalize uty = do
  uty'   <- applyBindings uty
  ctx    <- ask
  tmfvs  <- freeVars uty'
  ctxfvs <- freeVars ctx
  let fvs  = S.toList $ tmfvs \\ ctxfvs
      xs   = map (mkVarName "a") fvs
      subs = zip (map Right fvs) (map UTVar xs)
      bty  = substU (M.fromList subs) uty'
  return $ Forall (S.fromList xs) T.CTrue bty

mkVarName :: String -> IntVar -> TVarId
mkVarName nm (IntVar v) = genId $ fromString $ nm ++ show v

runInfer :: Infer UType -> Either Compiler.Error T.Scheme
runInfer =
  (>>= applyBindings)
    >>> (>>= (generalize >>> fmap freezeScheme))
    >>> flip runReaderT M.empty
    >>> runExceptT
    >>> evalIntBindingT
    >>> runIdentity
    >>> ensureJust
 where
  ensureJust
    :: Either Compiler.Error (Maybe T.Scheme) -> Either Compiler.Error T.Scheme
  ensureJust (Right (Just s)) = Right s
  ensureJust (Right Nothing) =
    Left $ Compiler.UnexpectedError $ fromString "Could not unfreeze scheme."
  ensureJust (Left e) = Left e
