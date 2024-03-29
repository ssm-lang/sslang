{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Unroll let-def blocks to break apart non-mutually recursive definitions.
module IR.SegmentLets (
  segmentLets,
  segmentDefs,
  segmentDefs',
) where

import qualified Common.Compiler as Compiler
import Common.Identifiers (
  HasFreeVars (..),
  VarId (..),
  genId,
  showId,
  ungenId,
 )
import qualified IR.IR as I

import Control.Monad.State (
  State,
  evalState,
  gets,
  modify,
 )
import Data.Bifunctor (Bifunctor (..))
import Data.Generics.Aliases (mkT)
import Data.Generics.Schemes (everywhere)
import Data.Graph (
  SCC (..),
  stronglyConnComp,
 )
import qualified Data.Set as S


type T = I.Type
type Def t = (I.Binder t, I.Expr t)


segmentLets :: I.Program T -> Compiler.Pass (I.Program T)
segmentLets p =
  return p{I.programDefs = everywhere (mkT segmentLetExpr) $ I.programDefs p}


segmentLetExpr :: I.Expr T -> I.Expr T
segmentLetExpr (I.Let ds b t) = foldr ilet b $ segmentDefs ds
 where
  ilet d' b' = I.Let d' b' t
segmentLetExpr e = e


segmentDefs :: [Def t] -> [[Def t]]
segmentDefs = map fromSCC . stronglyConnComp . (`evalState` 0) . mapM toGraph
 where
  toGraph :: Def t -> State Int (Def t, I.VarId, [I.VarId])
  toGraph d@(I._binderId -> b, e) = do
    v <- maybe (modify (+ 1) >> gets genVar) return b
    return (d, v, S.toList $ freeVars e)

  fromSCC :: SCC (Def t) -> [Def t]
  fromSCC = map (first ungenVar) . from
   where
    from (AcyclicSCC d) = [d]
    from (CyclicSCC ds) = ds

  genVar :: Int -> I.VarId
  genVar = genId . ("_wild" <>) . showId

  ungenVar :: I.Binder t -> I.Binder t
  ungenVar b@(I.BindVar v _) = b{I._binderId = ungenId v}
  ungenVar b = b


-- Used for constraint generation

type Def' t a b = (VarId, a, b, I.Expr t)


segmentDefs' :: [Def' t a b] -> [[Def' t a b]]
segmentDefs' = map fromSCC . stronglyConnComp . map toGraph
 where
  toGraph :: Def' t a b -> (Def' t a b, I.VarId, [I.VarId])
  toGraph d@(v, _, _, e) = (d, v, S.toList $ freeVars e)

  fromSCC :: SCC (Def' t a b) -> [Def' t a b]
  fromSCC = from
   where
    from (AcyclicSCC d) = [d]
    from (CyclicSCC ds) = ds
