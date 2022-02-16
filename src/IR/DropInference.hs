{-# LANGUAGE DerivingVia #-}


module IR.DropInference
  ( insertDropsProgram
  ) where


import qualified Common.Compiler               as Compiler
import           Common.Identifiers
import qualified IR.IR                         as I

import qualified IR.Types.Poly                 as Poly
import           IR.Types.TypeSystem            ( collectArrow
                                                , dearrow
                                                )
--import           Control.Monad                  ( forM_ )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                --, modify
                                                )
import qualified Data.Set                      as S
--import IR.IR (Program(programEntry))
import           Data.Maybe                     ( catMaybes
                                                , mapMaybe
                                                )
import           Data.Bifunctor

{-
inferDrops :: I.Expr t -> I.Expr t
inferDrops = error "todo"
-}

-- | Inserting State
data InsertState = InsertState
  { globScope :: S.Set I.VarId
  , currScope :: S.Set I.VarId
  , newDrops  :: [(I.VarId, I.Expr Poly.Type)]
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
  InsertState { globScope = S.empty, currScope = S.empty, newDrops = [] }

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
  vars' <- mapM (\(v, _) -> insertDropLet (v, typ)) bins
  bins' <- mapM (\(v, d) -> insertDropExpr d >>= (\d' -> return (v, d'))) bins
  expr' <- insertDropExpr expr
  let bins'' = bins' ++ ((Nothing, expr') : init vars')
  let expr'' = snd $ last vars'
  return $ I.Let bins'' expr'' typ
-- | placeholder
insertDropExpr expr = return expr

-- | Entry-point to let bindings traversal.
insertDropLet
  :: (Maybe I.VarId, Poly.Type) -> InsertFn (I.Binder, I.Expr Poly.Type)
insertDropLet (v, t) = do
  let v'   = head $ catMaybes [v]
  let drop = (Nothing, I.Prim I.Drop [I.Var v' t] t)
  return drop
-- | placeholder
