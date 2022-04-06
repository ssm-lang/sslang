-- | Substitute AST nodes with magical primitives.
{-# LANGUAGE OverloadedStrings #-}
module IR.SubstMagic
  ( substMagic
  ) where

import qualified IR.IR                         as I

import           Data.Data                      ( Proxy(..) )
import           Data.Generics                  ( Data(..)
                                                , everywhere
                                                , mkT
                                                )
import           IR.Types.TypeSystem            ( TypeSystem )


{- | Substitute AST nodes with magical primitives.

Implemented as a syb-style generic tree traversal.

Example usage:

@@
-- Given:
myExpr :: Expr Poly.Type

-- Do:
substMagic (Proxy :: Proxy Poly.Type) myExpr

-- Given:
myProm :: Program Annotated.Type

-- Do:
substMagic (Proxy :: Proxy Annotated.Type) myProg
@@
-}
substMagic :: (TypeSystem t, Data t, Data a) => Proxy t -> a -> a
substMagic p = everywhere $ mkT $ substMagicExpr p

-- | Replace applications to built-in names with corresponding primitives.
substMagicExpr :: TypeSystem t => Proxy t -> I.Expr t -> I.Expr t
substMagicExpr _ e = case I.unzipApp e of
  (I.Var "new"  _, (a, t) : ats) -> I.zipApp (I.Prim I.New [a] t) ats
  (I.Var "dup"  _, (a, t) : ats) -> I.zipApp (I.Prim I.Dup [a] t) ats
  (I.Var "drop" _, (a, t) : ats) -> I.zipApp (I.Prim I.Drop [a] t) ats
  _                              -> e
