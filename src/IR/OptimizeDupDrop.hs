{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}

module IR.OptimizeDupDrop where

import qualified Common.Compiler               as Compiler
import           Common.Identifiers
import           Control.Monad.State.Lazy       ( MonadState(..)
                                                , StateT(..)
                                                , evalStateT
                                                , forM
                                                , modify
                                                )
import           Data.Maybe                     ( fromJust )
import qualified IR.IR                         as I
import qualified IR.Types                      as I


-- * The external interface

-- $external

-- | Insert dup and drop primitives throughout a program
--
-- Applies `insertTop` to the program's definitions
optimizeDupDrop :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
optimizeDupDrop program = (`evalStateT` 0) $ do
  return program

-- * Module internals, not intended for use outside this module
--
-- $internal


-- | Monad for creating fresh variables: add an Int to the pass
type Fresh = StateT Int Compiler.Pass

-- $internal
