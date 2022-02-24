{-# LANGUAGE DerivingVia #-}


module IR.DropInference
  ( insertDropsProgram
  ) where


import qualified Common.Compiler               as Compiler
import           Common.Identifiers
import qualified IR.IR                         as I

import qualified IR.Types.Poly                 as Poly
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                , gets
                                                , modify
                                                )

import           Data.Maybe                      ( mapMaybe )                    
import qualified Data.Set                      as S

{-
import           IR.Types.TypeSystem            ( collectArrow
                                                , dearrow
                                                )
import           Control.Comonad                ( Comonad(..) )
import           Control.Monad                  ( forM_ )
import IR.IR (Program(programEntry))
import           Data.Bifunctor                 ( first )
import           Data.List                      ( intersperse )
import qualified Data.Map                      as M
-}

{-
inferDrops :: I.Expr t -> I.Expr t
inferDrops = error "todo"
-}

-- | Inserting State
data InsertState = InsertState
  { globScope :: S.Set I.VarId
  , currScope :: S.Set I.VarId
  , newDrops  :: [(I.VarId, I.Expr Poly.Type)]
  , anonCount :: Int
  }

-- | Insert Monad
newtype InsertFn a = InsertFn (StateT InsertState Compiler.Pass a)
  deriving Functor                      via (StateT InsertState Compiler.Pass)
  deriving Applicative                  via (StateT InsertState Compiler.Pass)
  deriving Monad                        via (StateT InsertState Compiler.Pass)
  deriving MonadFail                    via (StateT InsertState Compiler.Pass)
  deriving (MonadError Compiler.Error)  via (StateT InsertState Compiler.Pass)
  deriving (MonadState InsertState)     via (StateT InsertState Compiler.Pass)


-- | Run a InsertFn computation.
runInsertFn :: InsertFn a -> Compiler.Pass a
runInsertFn (InsertFn m) = evalStateT
  m
  InsertState { globScope = S.empty, currScope = S.empty, newDrops = [], anonCount = 0 }

getFresh :: InsertFn I.VarId
getFresh = do
  curCount <- gets anonCount
  modify $ \st -> st { anonCount = anonCount st + 1 }
  let name = fromString $ "anon" <> show curCount
  return name

-- | Entry-point to insert dup/drops.
insertDropsProgram
  :: I.Program Poly.Type -> Compiler.Pass (I.Program Poly.Type)
insertDropsProgram program = runInsertFn $ do
  let programDefs = I.programDefs program
  defs <- mapM insertDropTop programDefs
  return $ program { I.programDefs  = defs
                   , I.programEntry = I.programEntry program
                   , I.typeDefs     = I.typeDefs program
                   }

-- | Entry point for top-level expressions.
insertDropTop
  :: (I.VarId, I.Expr Poly.Type) -> InsertFn (I.VarId, I.Expr Poly.Type)
insertDropTop (var, expr) = do
  insertedExpr <- insertDropExpr expr
  return (var, insertedExpr)

-- | Entry-point to inserting into expressions.
insertDropExpr :: I.Expr Poly.Type -> InsertFn (I.Expr Poly.Type)
insertDropExpr (I.Let bins expr typ) = do

  ret_var <- getFresh
  bins'   <- mapM (\(v, d) -> insertDropExpr d >>= (\d' -> return (v, d'))) bins

  let var       = mapMaybe fst bins
  let utyp      = Poly.TBuiltin Poly.Unit
  let var_bin   = map (\v -> (Nothing, I.Prim I.Drop [I.Var v typ] utyp)) var
  let expr_bin  = (Just ret_var, expr) : var_bin
  let lfolder   = \x y -> I.Let [x] y typ
  let ret_expr  = I.Var ret_var typ
  let ret_expr' = foldr lfolder ret_expr expr_bin

  return $ I.Let bins' ret_expr' typ
-- | placeholder for other exprs
insertDropExpr expr = return expr
