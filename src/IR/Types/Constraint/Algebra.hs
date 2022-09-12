{-# LANGUAGE OverloadedStrings #-}
module IR.Types.Constraint.Algebra where

import           Common.Identifiers             ( DConId(..)
                                                , TVarId(..)
                                                )
import           IR.Types.Constraint.CoreAlgebra
                                                ( ARTerm(..)
                                                , Term(..)
                                                )
import           IR.Types.Constraint.Inference  ( InferM )

-- data BuiltinDataConstructor = BuiltinDataConstructor DConId [TVarId]

type Environment s a = TVarId -> InferM s (ARTerm a)

symbol :: Environment s a -> TVarId -> InferM s (ARTerm a)
symbol tenv = tenv

resultType :: Eq a => Environment s a -> ARTerm a -> InferM s (ARTerm a)
resultType tenv t = do
  x <- symbol tenv (TVarId "->")
  let chop t' = case t' of
        TTerm (App (TTerm (App v _)) u) | v == x -> chop u
        _ -> t'
  return $ chop t

argTypes :: Eq a => Environment s a -> ARTerm a -> InferM s [ARTerm a]
argTypes tenv t = do
  x <- symbol tenv (TVarId "->")
  let chop acu t' = case t' of
        TTerm (App (TTerm (App v t'')) u) | v == x -> chop (t'' : acu) u
        _ -> acu
  return $ reverse (chop [] t)

typConArgs :: ARTerm a -> [ARTerm a]
typConArgs t =
  let chop acu t' = case t' of
        TTerm (App t'' u) -> chop (u : acu) t''
        _                 -> acu
  in  chop [] t

typConName :: ARTerm a -> ARTerm a
typConName t = case t of
  TTerm     (App u _) -> typConName u
  TVariable _         -> t
  _                   -> error "Incorrect usage"
