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
import Data.Generics.Aliases ( mkM )
import Data.Generics.Schemes ( everywhereM )


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
    let defs = programDefs --map optimizeTop programDefs
    let (nDups, nDrops) = foldl (\(a,b) (a',b') -> (a + a', b + b')) (0,0) $ map countDupsAndDropsDef defs 
    traceM $ ("dups: " ++ (show nDups) ++ ", drops: " ++ (show nDrops))
    return $ program { I.programDefs  = defs
                     , I.programEntry = I.programEntry program
                     , I.typeDefs     = I.typeDefs program
                     }

-- * Module internals, not intended for use outside this module
--
-- $internal

-- ** Elimination Patterns

-- | Pattern: drop (dup v1) v1 -> v1
fusePair :: I.Expr I.Type -> Maybe (I.Expr I.Type)
fusePair drop@(I.Prim I.Drop [v1, dup@(I.Prim I.Dup [v2] _)] _)
    | v1 == v2  = Just v1
    | otherwise = Nothing

-- | Pattern: drop (f (dup v1)) v1 -> f v1
-- When a drop and a dup 
eliminateConsumptionPat :: I.Expr I.Type -> Maybe (I.Expr I.Type)
eliminateConsumptionPat (I.Prim I.Drop [v1, I.App f (I.Prim I.Dup [v2] t2) t3] t1)
    | v1 == v2  = Just $ I.App f v2 t3
    | otherwise = Nothing

-- ** Transformations/Tree rotations

-- | Swap a drop with a function application
-- Pattern:
-- case 1
--   let _ = f x
--   let _ = drop y r
-- just swap the two
-- case 2
--   drop y (f x)
-- would insert a let:
--   let _ = drop y () in f x
-- a compelling example
swapAppAndDrop :: I.Expr I.Type -> Maybe (I.Expr I.Type)
swapAppAndDrop all@(I.Prim I.Drop [v1, I.App f v2@I.Var{} t2] t1)
    | v1 /= v2  = Just $ I.App f v1 t2  -- insert a new let
    | otherwise = Nothing

-- | Pattern: drop (let a = b in c) x | x not in b -> let a = b in drop c x
-- i.e. move drop inside the body of the `let` if the variable to drop is not
-- used in the binder
-- Since lets are already segmented by this pass, we can safely assume bins has
-- one element.
moveDropInsideLet :: I.Expr I.Type -> Maybe (I.Expr I.Type)
moveDropInsideLet drop@(I.Prim I.Drop [v, I.Let bins expr t2] t1) =
    -- Does v appear on the RHS of the binder?
    let bin = head bins
    in if v `inExpr` snd bin
       then Just $ I.Let bins (I.Prim I.Drop [v, expr] t1) t2
       -- TODO: is t1 the right type? use extract?
       else Just $ I.Let [(fst bin, I.Prim I.Drop [v, snd bin] t1)] expr t2

-- | Utility function for traversing a subtree of the IR to check if it
-- contains a reference to a variable `v`.
inExpr :: I.Expr I.Type -> I.Expr I.Type -> Bool
v `inExpr` e = undefined--foldr (\x acc -> x==v || acc) False e

-- instead, use everywhereM from SYB


-- ** Tree algorithm (WIP)

-- | Optimize reference counting by moving drops as early (i.e. deep in tree) as possible
optimizeTop :: (I.VarId, I.Expr I.Type) -> (I.VarId, I.Expr I.Type)
optimizeTop (var, expr) = (var, ) $ optimizeExpr expr

-- | Optimize reference counting for an expression
optimizeExpr :: I.Expr I.Type -> (I.Expr I.Type)
optimizeExpr drop@(I.Prim I.Drop [e, r] typ) =
    case insertEarlyDrop e r of
        Nothing -> drop
        Just e' -> e'
optimizeExpr dcon@I.Data{}     = dcon
optimizeExpr lit@I.Lit{}       = lit
optimizeExpr var@I.Var{}       = var
optimizeExpr (I.App f x t)     =
    let f' = optimizeExpr f
        x' = optimizeExpr x
     in I.App f' x' t
optimizeExpr dup@(I.Prim I.Dup [e] typ) = dup
optimizeExpr (I.Prim p es typ) = I.Prim p (map optimizeExpr es) typ
optimizeExpr (I.Let bins expr typ) = I.Let bins' expr' typ
    where optimizeBins (v, d) = (v, optimizeExpr d)
          bins' = optimizeBins <$> bins
          expr' = optimizeExpr expr
optimizeExpr lam@(I.Lambda v e typ) = I.Lambda v (optimizeExpr e) typ
optimizeExpr (I.Match e alts typ) =
    let alts' = map optimizeAlt alts
        e'    = optimizeExpr e
     in I.Match e' alts' typ
optimizeAlt :: (I.Alt, I.Expr I.Type) -> (I.Alt, I.Expr I.Type)
optimizeAlt (I.AltDefault v, e)          = (I.AltDefault v, ) $ optimizeExpr e
optimizeAlt (I.AltLit l, e)              = (I.AltLit l, ) $ optimizeExpr e
optimizeAlt (I.AltData dcon binds, body) =
    let body' = optimizeExpr body in (I.AltData dcon binds, body')
    -- does this work?

insertEarlyDrop :: I.Expr I.Type -> I.Expr I.Type -> Maybe (I.Expr I.Type)
insertEarlyDrop dcon@I.Data{} v = Nothing
insertEarlyDrop lit@I.Lit{}   v = Nothing
insertEarlyDrop var@I.Var{}   v =
    if var == v then Just (I.Prim I.Drop [var, v] (I.extract var)) else Nothing
insertEarlyDrop (I.App f x t) v =
    let f' = case insertEarlyDrop f v of
                 Nothing -> f
                 Just f' -> f'
        x' = case insertEarlyDrop x v of
                 Nothing -> x
                 Just x' -> x'
        in return $ I.App f' x' t
        -- check body of let/App first, to see if var is used
        -- if not used, then put drop in binders
        -- if there is no use of the var in the binders, then Nothing
insertEarlyDrop dup@(I.Prim I.Dup [e] typ) v =
    if e == v then Just e else Nothing
insertEarlyDrop drop@(I.Prim I.Drop [e, r] typ) v =
    if r == v then Just drop    -- is this right? stop if there's already a drop of this var
              else case insertEarlyDrop e v of
                       Nothing -> Nothing
                       Just e' -> Just $ I.Prim I.Drop [e', r] typ
insertEarlyDrop e v = Nothing 

-- ignore below

-- | Prune referencing counting for top-level expressions based on types
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
        I.Var _ (I.Ref _) -> dup
        _ -> e
pruneExpr drop@(I.Prim I.Drop [e, r] typ) =
    case e of
        I.Var _ (I.Ref _) -> error "ref type" --drop
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

-- ** Debugging utilities

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
