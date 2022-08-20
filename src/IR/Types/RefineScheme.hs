{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
module IR.Types.RefineScheme where

import           IR.Types.Type
import           IR.Types.Unify

import qualified Common.Compiler               as Compiler
import           Common.Identifiers             ( TVarId(..)
                                                , fromString
                                                , ident
                                                )

import           Common.Pretty                  ( Pretty(..) )
import           Control.Monad                  ( unless
                                                , zipWithM_
                                                )
import           Data.Functor                   ( (<&>) )
import qualified Data.Set                      as S
import Control.Monad.Trans (MonadTrans(..))

refineScheme :: Scheme -> Scheme -> Compiler.Pass Scheme
refineScheme s1@(Scheme q1 c1 t1) s2@(Scheme q2 c2 t2) = do
  unless (c1 == CTrue) $ do
    Compiler.unexpected "Do not know how to handle non-trivial constraint"
  unless (c2 == CTrue) $ do
    Compiler.unexpected "Do not know how to handle non-trivial constraint"

  -- NOTE: assume there are no free, non-hole type variables in either scheme.
  unless (ftvScheme s1 == S.empty || ftvScheme s1 == S.singleton "_") $ do
    Compiler.unexpected $ "free type variables found: " ++ show (ftvScheme s1)

  unless (ftvScheme s2 == S.empty || ftvScheme s2 == S.singleton "_") $ do
    Compiler.unexpected $ "free type variables found: " ++ show (ftvScheme s2)

  let mangler :: Char -> S.Set TVarId -> TVarId -> TVarId
      mangler c s v | v `S.member` s = fromString [c] <> "," <> v
                    | otherwise      = v

      mangle1 = mangler '1' q1
      mangle2 = mangler '2' q2

      unmangler :: TVarId -> TVarId
      unmangler (ident -> _:',':v) = fromString v
      unmangler (ident -> _:',':v) = fromString v
      unmangler v                  = v

  -- Ensure quantified type variable names in t1 and t2 are unique.
  t1m <- rewriteTVars (return . mangle1) t1
  t2m <- rewriteTVars (return . mangle2) t2

  -- Also mangle quantifier sets
  let q1' = S.map mangle1 q1
      q2' = S.map mangle2 q2

  -- We run a localized version of union-find.
  --
  -- We maintain two maps as state: one maps each type variable to their
  -- 'Point', and the other maps representative type variables to full types.
  (t', q') <- runUnify $ do
    -- First, let t1 and t2 unify each other's holes.
    t1' <- unifyHoles t1m t2m
    t2' <- unifyHoles t2m t1m

    -- Initialize fresh points for each quantified type variable.
    makePointsFrom $ S.toList q1' ++ S.toList q2'

    let unifier :: TVarId -> Type -> UnifyM ()
        unifier v1 (TVar v2) = do
          p1 <- getPoint v1
          p2 <- getPoint v2
          p1 `union` p2
        unifier v1 (TCon tc2 ts2) = do
          lookupTCon v1 >>= \case
            Just (tc1, ts1) -> do
              lift $ unless (tc1 == tc2) $ do
                Compiler.unexpected "todo"

              zipWithM_ (unifyWith unifier) ts1 ts2
            Nothing -> insertType v1 $ TCon tc2 ts2

        rewriteType :: Type -> UnifyM Type
        rewriteType (TVar v) = do
          lookupType v >>= \case
            Just tc@(TCon _ _) -> rewriteType tc
            Nothing            -> getPoint v >>= repr >>= descriptor <&> TVar
            _                  -> error "Internal error"
        rewriteType (TCon tc ts) = do
          ts' <- mapM rewriteType ts
          return $ TCon tc ts'

    -- Try to unify our two types, with substitution
    t' <- rewriteType =<< unifyWith unifier t2' t1'

    -- Also perform appropriate substitutions in q'
    q' <- canonicalize q1'

    -- If s1 is more specific than s2, then t2' should have been instantiated to
    -- t1' via t'.
    lift $ unless (t1' == t') $ do
      Compiler.typeError
        $  "Could not refine schemes:\n"
        ++ show (pretty s1)
        ++ "\nDoes not appear to be more specific than:\n"
        ++ show (pretty s2)

    return (t', q')

  t <- rewriteTVars (return . unmangler) t'
  let q = S.map unmangler q'
  return $ Scheme q c1 t

s1, s2, s3 :: Scheme
(s1, s2, s3) = (scheme ["a", "b"] CTrue $ Arrow (tv "a") (tv "b"), scheme ["a"] CTrue $ Arrow (tv "a") (tv "a"), scheme ["a"] CTrue $ Arrow (tv "a") Hole)
