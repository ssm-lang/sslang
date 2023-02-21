{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

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

-- | Remove unnecessary dup and drop primitives throughout a program
--
-- Applies `pruneTop` to the program's definitions
optimizeDupDrop :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
optimizeDupDrop program = (`evalStateT` 0) $ do
    let programDefs = I.programDefs program
    let (nDups, nDrops) = foldl (\(a,b) (a',b') -> (a + a', b + b')) (0,0) $ map countDupsAndDropsDef programDefs 
    traceM $ ("dups: " ++ (show nDups) ++ ", drops: " ++ (show nDrops))
    let defs = map pruneTop programDefs
    let (nDups, nDrops) = foldl (\(a,b) (a',b') -> (a + a', b + b')) (0,0) $ map countDupsAndDropsDef defs 
    traceM $ ("dups: " ++ (show nDups) ++ ", drops: " ++ (show nDrops))
    return $ program { I.programDefs  = defs
                     , I.programEntry = I.programEntry program
                     , I.typeDefs     = I.typeDefs program
                     }

-- * Module internals, not intended for use outside this module
--
-- $internal

-- | Prune referencing counting for top-level expressions
pruneTop :: (I.VarId, I.Expr I.Type) -> (I.VarId, I.Expr I.Type)
pruneTop (var, expr) = (var, ) $ pruneExpr expr

-- | Prune reference counting for an expression
pruneExpr :: I.Expr I.Type -> I.Expr I.Type
pruneExpr dcon@I.Data{}     = dcon
pruneExpr lit@I.Lit{}       = lit
pruneExpr var@I.Var{}       = var
pruneExpr (I.App f x t)     =
    let f' = pruneExpr f
        x' = pruneExpr x
     in I.App f' x' t
pruneExpr dup@(I.Prim I.Dup [e] typ) =
    case e of
        I.Var _ (I.TCon "&" _) -> dup
        _ -> e
pruneExpr drop@(I.Prim I.Drop [e, r]  typ) =
    case e of
        I.Var _ (I.TCon "&" _) -> drop
        _ -> r
pruneExpr (I.Prim p es typ) = I.Prim p (map pruneExpr es) typ
pruneExpr (I.Let bins expr typ) = I.Let bins' expr' typ
    where pruneBins (v, d) = (v, pruneExpr d)
          bins' = pruneBins <$> bins
          expr' = pruneExpr expr
pruneExpr lam@(I.Lambda v e typ) = I.Lambda v (pruneExpr e) typ
pruneExpr (I.Match e alts typ) =
    let alts' = map pruneAlt alts
        e'    = pruneExpr e
     in I.Match e' alts' typ

pruneAlt :: (I.Alt, I.Expr I.Type) -> (I.Alt, I.Expr I.Type)
pruneAlt (I.AltDefault v, e)          = (I.AltDefault v, ) $ pruneExpr e
pruneAlt (I.AltLit l, e)              = (I.AltLit l, ) $ pruneExpr e
pruneAlt (I.AltData dcon binds, body) =
    let body' = pruneExpr body in (I.AltData dcon binds, body')
    -- This will do nothing until we fix insertAlt in insertRefCounting

-- | Count dups and drops in a definition.
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
