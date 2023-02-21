{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}

module IR.OptimizeDupDrop where

import qualified Common.Compiler               as Compiler
import           Common.Compiler                ( Warning(..)
                                                , ErrorMsg
                                                , warn
                                                , unexpected
                                                )
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
import           Debug.Trace                    ( traceM )


-- * The external interface

-- $external

-- | Insert dup and drop primitives throughout a program
--
-- Applies `insertTop` to the program's definitions
optimizeDupDrop :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
optimizeDupDrop program = (`evalStateT` 0) $ do
    let programDefs = I.programDefs program
    let (nDups, nDrops) = foldl (\(a,b) (a',b') -> (a + a', b + b')) (0,0) $ map countDupsAndDropsDef programDefs 
    --traceM $ ("dups: " ++ (show nDups) ++ ", drops: " ++ (show nDrops))
    return program

-- * Module internals, not intended for use outside this module
--
-- $internal

countDupsAndDropsDef :: (I.VarId, I.Expr I.Type) -> (Int, Int)
countDupsAndDropsDef def@(_,e) = countDupsAndDropsExpr e

countDupsAndDropsExpr :: I.Expr I.Type -> (Int, Int)
countDupsAndDropsExpr (I.Prim I.Dup [_r] _t) = (1, 0)
countDupsAndDropsExpr (I.Prim I.Drop [e', _r] _t) =
    (\(a,b) -> (a, 1+b)) $ countDupsAndDropsExpr e'
countDupsAndDropsExpr (I.Prim p xs _t) =
    foldl (\(a,b) (a',b') -> (a + a', b + b')) (0,0) $ map countDupsAndDropsExpr xs
countDupsAndDropsExpr (I.App f x _t) = (a + a', b + b')
    where (a , b ) = countDupsAndDropsExpr f
          (a', b') = countDupsAndDropsExpr x
countDupsAndDropsExpr (I.Let bins e _t) =
    let (a , b ) = foldl (\(a,b) (a',b') -> (a + a', b + b')) (0,0) $ map (countDupsAndDropsExpr . snd) bins
        (a', b') = countDupsAndDropsExpr e
     in (a + a', b + b')
countDupsAndDropsExpr (I.Lambda _ e _t) = countDupsAndDropsExpr e
countDupsAndDropsExpr (I.Match e alts _t) =
    let (a, b) = foldl (\(a,b) (a',b') -> (a + a', b + b')) (0,0) $ map (countDupsAndDropsExpr . snd) alts
        (a', b') = countDupsAndDropsExpr e
     in (a + a', b + b')
-- Ignore Data, Lit, Var
countDupsAndDropsExpr _ = (0,0)

-- | Monad for creating fresh variables: add an Int to the pass
type Fresh = StateT Int Compiler.Pass

-- $internal
