{-# LANGUAGE OverloadedStrings #-}

-- | Substitute AST nodes with magical primitives.
module IR.SubstMagic (
  substMagic,
) where

import qualified IR.IR as I

import Data.Data (Proxy (..))
import Data.Generics (
  Data (..),
  everywhere,
  mkT,
 )


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
substMagic :: (Data t, Data a) => Proxy t -> a -> a
substMagic p = everywhere $ mkT $ substMagicExpr p


-- | Replace applications to built-in names with corresponding primitives.
substMagicExpr :: Proxy t -> I.Expr t -> I.Expr t
substMagicExpr _ e = case I.unfoldApp e of
  (I.Var "new" _, (a, t) : ats) -> I.foldApp (I.Prim I.New [a] t) ats
  (I.Var "dup" _, (a, t) : ats) -> I.foldApp (I.Prim I.Dup [a] t) ats
  (I.Var "drop" _, (a, t) : (b, _) : ats) ->
    I.foldApp (I.Prim I.Drop [a, b] t) ats
  _ -> e
