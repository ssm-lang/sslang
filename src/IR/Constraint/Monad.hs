module IR.Constraint.Monad where

import qualified Common.Compiler as Compiler
import Common.Identifiers (
  DConId (..),
  TConId (..),
  TVarId (..),
  VarId (..),
  fromId,
 )
import qualified Common.Identifiers as Ident
import Control.Monad.Except (ExceptT)
import qualified Control.Monad.Except as Except
import Control.Monad.State (StateT)
import qualified Control.Monad.State as State
import Data.Bifunctor (second)
import qualified Data.Map.Strict as Map
import GHC.IO.Unsafe (unsafePerformIO)
import qualified IR.Constraint.Canonical as Can
import qualified IR.IR as I

import Control.Monad.Writer (WriterT)
import qualified Control.Monad.Writer as Writer
import Prettyprinter (Doc)


type TC a = StateT TCState (ExceptT Compiler.Error (WriterT (Doc String) IO)) a


type DConInfo = (DConId, TConId, [TVarId], [Can.Type])


type DConMap = Map.Map DConId DConInfo


data TCState = TCState
  { _freshCtr :: Int -- Used for generating fresh variable names
  , _dconMap :: DConMap
  , _kindMap :: Map.Map TConId Can.Kind
  , _externMap :: Map.Map VarId Can.Type
  }


runTC :: TCState -> TC a -> (Either Compiler.Error a, Doc String)
runTC state m = unsafePerformIO $ Writer.runWriterT $ Except.runExceptT $ State.evalStateT m state


mkTCState :: I.Program Can.Annotations -> TCState
mkTCState prog =
  let kenv = Map.fromList $ map (second $ length . I.targs) $ I.typeDefs prog
   in TCState
        { _freshCtr = 0
        , _dconMap = mkDConMap prog
        , _kindMap = Map.union kenv Can.builtinKinds
        , _externMap = Map.fromList $ I.externDecls prog
        }


mkDConMap :: I.Program Can.Annotations -> DConMap
mkDConMap I.Program{I.typeDefs = tdefs} =
  foldl
    (\m (dcid, tcid, tvs, ts) -> Map.insert dcid (dcid, tcid, tvs, ts) m)
    Map.empty
    (concatMap tdef2dcon tdefs)
 where
  tdef2dcon (tcid, tdef) =
    [ (dcid, tcid, I.targs tdef, getVariantArgTypes tv)
    | (dcid, tv) <- I.variants tdef
    ]
  -- tdef2dconCons (_, tdef) = [(dcid, getVariantArgTypes tv) | (dcid, tv) <- variants tdef]
  getVariantArgTypes (I.VariantNamed ns) = map snd ns
  getVariantArgTypes (I.VariantUnnamed ts) = ts


-- | Generate a fresh identifier based on some prefix.
freshName :: String -> TC TVarId
freshName prefix = do
  n <- State.gets _freshCtr
  State.modify $ \state -> state{_freshCtr = n + 1}
  return $ Ident.fromString $ prefix ++ show n


-- | Generate a fresh program variable name, e.g., for anonymous binders.
freshVar :: TC VarId
freshVar = fromId <$> freshName "__anon_binder"


-- | Generate a fresh type identifier to be associated with annotations.
freshAnnVar :: TC VarId
freshAnnVar = fromId <$> freshName "$ann"


getDConInfo :: DConId -> TC (Maybe DConInfo)
getDConInfo dcon = do
  dconMap <- State.gets _dconMap
  return $ Map.lookup dcon dconMap


getKind :: TConId -> TC (Maybe Can.Kind)
getKind tcon = do
  kenv <- State.gets _kindMap
  return $ Map.lookup tcon kenv


getExtern :: VarId -> TC (Maybe Can.Type)
getExtern var = do
  externs <- State.gets _externMap
  return $ Map.lookup var externs


throwError :: String -> TC a
throwError s = Except.throwError $ Compiler.TypeError $ Compiler.fromString s
