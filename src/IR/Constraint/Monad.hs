module IR.Constraint.Monad where

import           Common.Identifiers             ( DConId(..)
                                                , TConId(..)
                                                , TVarId(..)
                                                , VarId(..)
                                                )
import qualified Common.Identifiers            as Ident
import           Control.Monad.State            ( StateT )
import qualified Control.Monad.State           as State
import qualified Data.Map.Strict               as Map
import           GHC.IO.Unsafe                  ( unsafePerformIO )
import qualified IR.Constraint.Canonical       as Can
import qualified IR.IR                         as I


type TC a = StateT TCState IO a

type DConInfo = (DConId, TConId, [TVarId], [Can.Type])

type DConMap = Map.Map DConId DConInfo

data TCState = TCState
  { _freshName :: Int -- fresh tvar name
  , _freshVar  :: Int -- fresh var name for replace binders in tc
  , _dconMap   :: DConMap
  }

runTC :: TCState -> TC a -> a
runTC state m = unsafePerformIO $ State.evalStateT m state

mkTCState :: I.Program Can.Annotations -> TCState
mkTCState prog =
  TCState { _freshName = 0, _freshVar = 0, _dconMap = mkDConMap prog }

mkDConMap :: I.Program Can.Annotations -> DConMap
mkDConMap I.Program { I.typeDefs = tdefs } = foldl
  (\m (dcid, tcid, tvs, ts) -> Map.insert dcid (dcid, tcid, tvs, ts) m)
  Map.empty
  (concatMap tdef2dcon tdefs)
 where
  tdef2dcon (tcid, tdef) =
    [ (dcid, tcid, I.targs tdef, getVariantArgTypes tv)
    | (dcid, tv) <- I.variants tdef
    ]
  -- tdef2dconCons (_, tdef) = [(dcid, getVariantArgTypes tv) | (dcid, tv) <- variants tdef]
  getVariantArgTypes (I.VariantNamed   ns) = map snd ns
  getVariantArgTypes (I.VariantUnnamed ts) = ts

freshName :: TC TVarId
freshName = do
  n <- State.gets _freshName
  State.modify $ \state -> state { _freshName = n + 1 }
  return $ Ident.fromString $ "_t" ++ show n

freshVar :: TC VarId
freshVar = do
  n <- State.gets _freshVar
  State.modify $ \state -> state { _freshVar = n + 1 }
  return $ Ident.fromString $ "_tc_anon_" ++ show n

getDConInfo :: DConId -> TC (Maybe DConInfo)
getDConInfo dcon = do
  dconMap <- State.gets _dconMap
  return $ Map.lookup dcon dconMap
