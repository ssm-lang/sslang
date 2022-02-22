-- | Turns partially applied data constructors into constructor functions.

module IR.DConToFunc
  ( dConToFunc
  ) where

import qualified Common.Compiler               as Compiler

import qualified IR.IR                         as I

import qualified IR.Types.Poly                 as Poly


dConToFunc :: I.Program Poly.Type -> Compiler.Pass (I.Program Poly.Type)
dConToFunc = pure
