module Constraint.SolverM where

import Common.Compiler (Pass (..))
import Common.Identifiers (TVarId)
import Control.Monad.ST.Trans
import Control.Monad.State.Lazy (StateT (..))
import qualified Data.Map as M
import IR.IR

newtype SolverGen = SolverGen
  { dconMap :: M.Map DConId (DConId, TConId, [TVarId], [Type])
  }

data SolverCtx = SolverCtx
  { currId :: Int,
    currMark :: Int,
    currNameId :: Int,
    solverProg :: Program Annotations,
    solverGen :: SolverGen
  }

type SolverM s a = STT s (StateT SolverCtx Pass) a

initSolverCtx :: Program Annotations -> SolverCtx
initSolverCtx prog =
  SolverCtx
    { currId = 0,
      currMark = 0,
      currNameId = 0,
      solverProg = prog,
      solverGen = initGenCtx prog
    }

initGenCtx :: Program Annotations -> SolverGen
initGenCtx Program {typeDefs = tdefs} =
  let cmap = foldl (\m (dcid, tcid, tvs, ts) -> M.insert dcid (dcid, tcid, tvs, ts) m) M.empty (concatMap tdef2dcon tdefs)
   in SolverGen {dconMap = cmap}
  where
    tdef2dcon (tcid, tdef) = [(dcid, tcid, targs tdef, getVariantArgTypes tv) | (dcid, tv) <- variants tdef]
    -- tdef2dconCons (_, tdef) = [(dcid, getVariantArgTypes tv) | (dcid, tv) <- variants tdef]
    getVariantArgTypes (VariantNamed ns) = map snd ns
    getVariantArgTypes (VariantUnnamed ts) = ts
