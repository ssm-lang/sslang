module IR.Constraint.Monad where

import qualified Common.Identifiers            as Ident
import           Control.Monad.State            ( StateT )
import qualified Control.Monad.State           as State
import           GHC.IO.Unsafe                  ( unsafePerformIO )


type TC a = StateT TCState IO a

data TCState = TCState
  { _freshName :: Int
  , _freshVar  :: Int
  }

runTC :: TC a -> a
runTC m = unsafePerformIO $ State.evalStateT m mkTCState

mkTCState :: TCState
mkTCState = TCState 0 0

freshName :: TC Ident.TVarId
freshName = do
  n <- State.gets _freshName
  State.modify $ \state -> state { _freshName = n + 1 }
  return $ Ident.fromString $ "_t" ++ show n

freshVar :: TC Ident.VarId
freshVar = do
  n <- State.gets _freshVar
  State.modify $ \state -> state { _freshVar = n + 1 }
  return $ Ident.fromString $ "_tc_anon_" ++ show n
