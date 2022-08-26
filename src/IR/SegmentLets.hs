-- | Unroll let-def blocks to break apart non-mutually recursive definitions.
module IR.SegmentLets
  ( segmentLets
  , segmentDefs
  ) where

import qualified Common.Compiler               as Compiler
import           Common.Identifiers             ( HasFreeVars(..) )
import qualified IR.IR                         as I

import           Data.Generics.Aliases          ( mkT )
import           Data.Generics.Schemes          ( everywhere )
import qualified Data.Set                      as S

type T = I.Type

segmentLets :: I.Program T -> Compiler.Pass (I.Program T)
segmentLets p =
  return p { I.programDefs = everywhere (mkT segmentLetExpr) $ I.programDefs p }

segmentLetExpr :: I.Expr T -> I.Expr T
segmentLetExpr (I.Let ds b t) = foldr ilet b $ segmentDefs ds
  where ilet d' b' = I.Let d' b' t
segmentLetExpr e = e

segmentDefs :: [(I.Binder, I.Expr t)] -> [[(I.Binder, I.Expr t)]]
segmentDefs = segment [] . reverse
 where
  segment acc []       = acc
  segment []  (d : ds) = segment [[d]] ds
  segment acc (d@(Just v, _) : ds)
    | v `S.member` precedingFreeVars ds = pushSegment acc d `segment` ds
    | otherwise                         = newSegment acc d `segment` ds
  segment acc (d : ds) = segment (pushSegment acc d) ds

  precedingFreeVars = S.unions . fmap (freeVars . snd)
  newSegment xs x = [x] : xs
  pushSegment xs x = (x : head xs) : tail xs
