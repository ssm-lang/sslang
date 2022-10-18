{-# LANGUAGE StandaloneDeriving #-}
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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Wrapper around the unification-fd library.

Compared to normal types ('T.Type'), unification types ('Type') may contain
unification variables, which only appear during the type inference process.

This module provides a definition for unification types, an inference monad, as
well as some pattern synonyms for the builtin types.
-}
module IR.Types.Unification
  ( -- * Unification types
    Type
  , Scheme
  , pattern TVar
  , pattern TCon
  , HasFreeUVars(..)

  -- * The inference monad
  , InferM
  , runInfer
  , fresh
  , applyBindings
  , (=:=)
  , (<:=)
  , unfreeze
  , freeze
  , instantiate
  , generalize
  , MonadReader(..)
  , MonadError(..)
  , asks

  -- * Builtin types
  , pattern Hole
  , pattern Arrow
  , foldArrow
  , unfoldArrow
  , pattern Unit
  , pattern Ref
  , pattern List
  , pattern Time
  , pattern I64
  , pattern U64
  , pattern I32
  , pattern U32
  , pattern I16
  , pattern U16
  , pattern I8
  , pattern U8
  , tuple
  ) where

import qualified Common.Compiler               as Compiler
import           Common.Identifiers             ( HasFreeVars(..)
                                                , TConId(..)
                                                , TVarId(..)
                                                , fromString
                                                , showId
                                                )
import qualified IR.Types.Type                 as T
import           IR.Types.Type                  ( SchemeOf(Forall) )

import           Control.Unification            ( BindingMonad(freeVar)
                                                , Fallible(..)
                                                , UTerm(..)
                                                , Unifiable
                                                , getFreeVars
                                                )
import qualified Control.Unification           as U
import           Control.Unification.IntVar     ( IntBindingT
                                                , IntVar(..)
                                                , evalIntBindingT
                                                )

import           Control.Monad                  ( forM )
import           Control.Monad.Except           ( ExceptT
                                                , MonadError(..)
                                                , runExceptT
                                                )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                , asks
                                                )
import           Control.Monad.Trans            ( MonadTrans(..) )
import           Data.Function                  ( (&) )
import           Data.Functor                   ( (<&>) )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Set                      as S
import           Data.Set                       ( (\\) )
import           GHC.Generics                   ( Generic1 )

deriving instance Ord IntVar
deriving instance Unifiable []

-- | Base functor for the unification 'Type'.
data TypeF a
  = TConF TConId [a]  -- ^ type constructors
  | TVarF TVarId      -- ^ (quantified) type variables
  deriving (Eq, Functor, Foldable, Traversable, Generic1, Unifiable)

-- | A type that may also contain unification variables.
type Type = UTerm TypeF IntVar

-- | An explicitly quantified 'Type', which may contain unification variables.
type Scheme = T.SchemeOf Type

-- | Pattern synonym for 'Type' variables.
pattern TVar :: TVarId -> Type
pattern TVar v = UTerm (TVarF v)

-- | Pattern synonym for 'Type' constructors.
pattern TCon :: TConId -> [Type] -> Type
pattern TCon tc ts = UTerm (TConF tc ts)

instance Show a => Show (TypeF a) where
  show (TConF "->" [a, b]) = show a <> " -> " <> show b
  show (TConF "[]" [a]   ) = "[" <> show a <> "]"
  show (TConF "&"  [a]   ) = "&" <> show a
  show (TConF tc   []    ) = show tc
  show (TConF tc   ts    ) = unwords (show tc : map show ts)
  show (TVarF v          ) = show v

-- | Catamorphism over unification types; useful for term rewriting.
ucata :: Functor t => (v -> a) -> (t a -> a) -> UTerm t v -> a
ucata f _ (UVar  v) = f v
ucata f g (UTerm t) = g (fmap (ucata f g) t)

substU :: M.Map (Either TVarId IntVar) Type -> Type -> Type
substU m = ucata f g
 where
  f v = fromMaybe (UVar v) $ M.lookup (Right v) m
  g (TVarF v) = fromMaybe (TVar v) (M.lookup (Left v) m)
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

class HasFreeUVars a where
  freeUVars :: a -> InferM ctx (S.Set IntVar)

instance HasFreeUVars Type where
  freeUVars = fmap S.fromList . lift . lift . getFreeVars

instance HasFreeUVars Scheme where
  freeUVars (Forall _ t) = freeUVars t

instance HasFreeVars Type TVarId where
  freeVars (TCon _ ts) = S.unions $ map freeVars ts
  freeVars Hole        = S.empty
  freeVars (TVar v)    = S.singleton v
  freeVars _           = S.empty

-- | Inference monad, build on top of the unification algorithm.
type InferM ctx
  = ReaderT ctx (ExceptT Compiler.Error (IntBindingT TypeF Compiler.Pass))

-- | Execute the 'InferM' inference monad.
runInfer :: ctx -> InferM ctx a -> Compiler.Pass a
runInfer ctx m =
  m & (`runReaderT` ctx) & runExceptT & evalIntBindingT >>= \case
    Left  err -> Compiler.throwError err
    Right res -> return res

-- | Create a fresh unification variable.
fresh :: InferM ctx Type
fresh = UVar <$> lift (lift freeVar)

-- | Apply bindings from current monad so remaining bindings must be free.
applyBindings :: Type -> InferM ctx Type
applyBindings = lift . U.applyBindings

-- | Unifies two types; fails if they cannot unify.
(=:=) :: Type -> Type -> InferM ctx ()
s =:= t = lift $ s U.=:= t >> return ()
infix 4 =:=

-- | Whether the LHS subsumes the RHS.
(<:=) :: Type -> Type -> InferM ctx Bool
s <:= t = lift $ s U.<:= t
infix 4 <:=

-- | Promote a 'T.Type' to a 'Type'.
unfreeze :: T.Type -> Type
unfreeze (T.TCon tc ts) = UTerm $ TConF tc $ fmap unfreeze ts
unfreeze (T.TVar v    ) = UTerm $ TVarF v

{- | Demote a 'Type' to a regular 'T.Type'.

Fails if the unification type contains more than just contructors and
variables.
-}
freeze :: Type -> InferM ctx T.Type
freeze (TCon tc ts) = mapM freeze ts <&> T.TCon tc
freeze (TVar v    ) = return $ T.TVar v
freeze t =
  throwError
    $  Compiler.UnexpectedError
    $  fromString
    $  "Could not freeze: "
    ++ show t

-- | Instantiate a 'Scheme' by replacing all quantified type varibles with fresh
-- unification variables
instantiate :: Scheme -> InferM ctx Type
instantiate (Forall (S.toList -> xs) uty) = do
  subs <- forM xs $ \v -> do
    fv <- fresh
    return (Left v, fv)
  return $ substU (M.fromList subs) uty

-- | Generalize a 'Type' by replacing all free unification variables with
-- quantified type variables.
generalize :: HasFreeUVars ctx => Type -> InferM ctx Scheme
generalize uty = do
  uty'   <- applyBindings uty
  ctx    <- ask
  tmfvs  <- freeUVars uty'
  ctxfvs <- freeUVars ctx
  let fvs  = S.toList $ tmfvs \\ ctxfvs
      xs   = take (length fvs) tvarNames
      subs = zip (map Right fvs) (map TVar xs)
      bty  = substU (M.fromList subs) uty'
  return $ Forall (S.fromList xs) bty
 where
  -- TODO: generate prettier names
  tvarNames :: [TVarId]
  tvarNames = map (("a" <>) . showId) [(1 :: Int) ..]

-- | A fresh, free type variable; may appear in type annotations.
pattern Hole :: Type
pattern Hole = UTerm (TVarF "_")

-- | The type constructor for function arrows.
pattern Arrow :: Type -> Type -> Type
pattern Arrow a b = UTerm (TConF "->" [a, b])

-- | Unfold an 'Arrow' 'Type' into a list of argument types and a return type.
unfoldArrow :: Type -> ([Type], Type)
unfoldArrow (Arrow a b) = let (as, rt) = unfoldArrow b in (a : as, rt)
unfoldArrow t           = ([], t)

-- | Fold a list of argument types and a return type into an 'Arrow' 'Type'.
foldArrow :: ([Type], Type) -> Type
foldArrow (a : as, rt) = a `Arrow` foldArrow (as, rt)
foldArrow ([]    , t ) = t

-- | The builtin singleton 'Type', whose only data constructor is just @()@.
pattern Unit :: Type
pattern Unit = TCon "()" []

-- | The builtin reference 'Type', created using @new@.
pattern Ref :: Type -> Type
pattern Ref a = TCon "&" [a]

-- | The builtin list 'Type', created using list syntax, e.g., @[a, b]@.
pattern List :: Type -> Type
pattern List a = TCon "[]" [a]

-- | The builtin 64-bit timestamp 'Type'.
pattern Time :: Type
pattern Time = TCon "Time" []

-- | Builtin 'Type' for signed 64-bit integers.
pattern I64 :: Type
pattern I64 = TCon "Int64" []

-- | Builtin 'Type' for unsigned 64-bit integers.
pattern U64 :: Type
pattern U64 = TCon "UInt64" []

-- | Builtin 'Type' for signed 32-bit integers.
pattern I32 :: Type
pattern I32 = TCon "Int32" []

-- | Builtin 'Type' for unsigned 32-bit integers.
pattern U32 :: Type
pattern U32 = TCon "UInt32" []

-- | Builtin 'Type' for signed 16-bit integers.
pattern I16 :: Type
pattern I16 = TCon "Int16" []

-- | Builtin 'Type' for unsigned 16-bit integers.
pattern U16 :: Type
pattern U16 = TCon "UInt16" []

-- | Builtin 'Type' for signed 8-bit integers.
pattern I8 :: Type
pattern I8 = TCon "Int8" []

-- | Builtin 'Type' for unsigned 8-bit integers.
pattern U8 :: Type
pattern U8 = TCon "UInt8" []

-- | Construct a builtin tuple type out of a list of at least 2 types.
tuple :: [Type] -> Type
tuple ts = UTerm $ TConF (T.tupleId $ length ts) ts
