{-# LANGUAGE DerivingVia #-}


module IR.DropInference (
    insertDropsProgram
) where


import qualified Common.Compiler               as Compiler
import qualified IR.IR                         as I
import qualified IR.Types.Poly                 as Poly
--import           Control.Monad                  ( forM_ )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                --, modify
                                                )
import qualified Data.Set                      as S
import IR.IR (Program(programEntry))

{-
inferDrops :: I.Expr t -> I.Expr t
inferDrops = error "todo"
-}

-- | Inserting State
data InsertState = InsertState
  { globScope  :: S.Set I.VarId,
    currScope  :: S.Set I.VarId,
    newDrops   :: [(I.VarId, I.Expr Poly.Type)]
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
runInsertFn (InsertFn m) = evalStateT m InsertState { 
    globScope  = S.empty,
    currScope  = S.empty,
    newDrops   = []
}

-- | Entry-point to insert dup/drops.
insertDropsProgram :: 
    I.Program Poly.Type -> 
    Compiler.Pass (I.Program Poly.Type)
insertDropsProgram program = runInsertFn $ do
  let programDefs = I.programDefs program
  defs <- mapM insertDropTop programDefs
  return $ program { 
      I.programDefs = defs,
      I.programEntry = I.programEntry program,
      I.typeDefs = I.typeDefs program
   }

-- | Entry point for top-level expressions.
insertDropTop :: 
    (I.VarId, I.Expr Poly.Type) -> 
    InsertFn (I.VarId, I.Expr Poly.Type)
insertDropTop (var, expr) = do
    insertDropExpr (var, expr)

-- | Entry-point to inserting into expressions.
insertDropExpr :: 
    (I.VarId, I.Expr Poly.Type) -> 
    InsertFn (I.VarId, I.Expr Poly.Type)
insertDropExpr (var, I.Let bins expr typ) = do 
    insertedLets    <- mapM insertDropLet bins
    return (var, I.Let (concat insertedLets) expr typ)
-- | placeholder
insertDropExpr (var, expr) = return (var, expr)

-- | Entry-point to let bindings traversal.
insertDropLet ::
    (I.Binder, I.Expr Poly.Type) -> 
    InsertFn [(I.Binder, I.Expr Poly.Type)]
insertDropLet (v, I.Var var t) = do 
    let dup = (Nothing, I.Prim I.Dup [I.Var var t] t)
    return $ (v, I.Var var t) : [dup]
-- | placeholder
insertDropLet (v, e) = do return [(v, e)]
