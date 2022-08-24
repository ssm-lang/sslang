-- | Unroll let-def blocks to break apart non-mutually recursive definitions.
module IR.UnrollLets
  ( unrollLets
  , segmentDefs
  ) where

import qualified Common.Compiler               as Compiler
import           Common.Identifiers             ( HasFreeVars(..) )
import qualified IR.IR                         as I

import           Data.Generics                  ( Data )
import           Data.Generics.Aliases          ( mkT )
import           Data.Generics.Schemes          ( everywhere )
import qualified Data.Set                      as S

unrollLets :: Data t => I.Program t -> Compiler.Pass (I.Program t)
unrollLets p = return
  $ p { I.programDefs = everywhere (mkT unrollLetExpr) $ I.programDefs p }

unrollLetExpr :: I.Expr I.Type -> I.Expr I.Type
unrollLetExpr (I.Let ds b t) = foldr ilet b $ segmentDefs ds
  where ilet d' b' = I.Let d' b' t
unrollLetExpr e = e

segmentDefs :: [(I.Binder, I.Expr I.Type)] -> [[(I.Binder, I.Expr I.Type)]]
segmentDefs = reverse . map reverse . segment [] . reverse
 where
  segment acc [] = acc
  segment acc (d@(Just v, _) : ds)
    | v `S.member` precedingFreeVars ds = pushSegment acc d `segment` ds
    | otherwise                         = newSegment acc d `segment` ds
  segment acc (d : ds) = segment (pushSegment acc d) ds

  precedingFreeVars = S.unions . fmap (freeVars . snd)
  newSegment xs x = [x] : xs
  pushSegment xs x = (x : head xs) : tail xs
