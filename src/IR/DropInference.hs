{-# LANGUAGE DerivingVia #-}


module IR.DropInference
  ( insertDropsProgram
  ) where


import qualified Common.Compiler               as Compiler
import           Common.Identifiers
import qualified IR.IR                         as I
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                , gets
                                                , modify
                                                , forM
                                                )
import qualified IR.Types.Poly                 as Poly
import           Data.Maybe                     ( isNothing
                                                , fromJust
                                                )
import qualified Data.Set                      as S


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
  InsertState { globScope = S.empty
              , currScope = S.empty
              , newDrops  = []
              , anonCount = 0
              }


-- | Top level helper functions

-- | Get a fresh variable name
getFresh :: String -> InsertFn I.VarId
getFresh str = do
  curCount <- gets anonCount
  modify $ \st -> st { anonCount = anonCount st + 1 }
  return $ fromString $ ("anon" <> show curCount) ++ str

-- | Make a drop primitive with unit type
makeDrop :: I.Expr Poly.Type -> (Maybe a, I.Expr Poly.Type)
makeDrop e = (Nothing, I.Prim I.Drop [e] $ Poly.TBuiltin Poly.Unit)

-- | Given @a@ and @b@, construct @a ; b@ (i.e., @let _ = a ; b@). 
seqExpr :: (Maybe I.VarId, I.Expr a) -> I.Expr a -> I.Expr a
seqExpr a b = I.Let [a] b (I.extract b)

-- | Given @[a1 .. an]@ and @b@, construct @a1 ; .. ; an ; b@. 
seqExprs :: [(Maybe I.VarId, I.Expr a)] -> I.Expr a -> I.Expr a
seqExprs as b = foldr seqExpr b as


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

-- | Inserting drops into Let bindings
insertDropExpr (I.Let bins expr typ) = do
  retVar <- getFresh "_let"
  bins'  <- forM bins $ \(v, d) -> do
    temp <- getFresh "_let_underscore"
    d'   <- insertDropExpr d
    return (if isNothing v then Just temp else v, d')
  let makeDrops = \b -> makeDrop $ I.Var (fromJust $ fst b) (I.extract $ snd b)
      exprBins  = (Just retVar, expr) : map makeDrops bins'
      retExpr   = seqExprs exprBins $ I.Var retVar typ
  return $ I.Let bins' retExpr typ

-- | Inserting drops into lambda functions
insertDropExpr (I.Lambda var expr typ) = do
  retVar <- getFresh "_lambda"
  expr'  <- insertDropExpr expr
  let typArg (Poly.TBuiltin(Poly.Arrow l _)) = l
      typArg t = t
  let varDrop  = makeDrop $ I.Var (fromJust var) (typArg typ)
      exprBins = (Just retVar, expr') : [varDrop]
      retExpr  = seqExprs exprBins $ I.Var retVar (typArg typ)
  return $ I.Lambda var retExpr typ

-- | Inserting drops into pattern-matching
insertDropExpr (I.Match expr alts typ) = do
  alts' <- forM alts $ \(v, e) -> do
    insertDropAlt (v, e) expr
  return $ I.Match expr alts' typ

-- | placeholder for other exprs
insertDropExpr expr = return expr


-- | Entry-point to inserting into pattern-matching arms.
insertDropAlt
  :: (I.Alt, I.Expr Poly.Type)
  -> I.Expr Poly.Type
  -> InsertFn (I.Alt, I.Expr Poly.Type)

-- | Inserting drops into AltDefault Binder
insertDropAlt (I.AltDefault var, exprReturn) exprMatch = do
  retVar      <- getFresh "_alt_default"
  exprReturn' <- insertDropExpr exprReturn
  let varDrop  = makeDrop $ I.Var (fromJust var) (I.extract exprMatch)
      exprBins = (Just retVar, exprReturn') : [varDrop]
      retExpr  = seqExprs exprBins $ I.Var retVar (I.extract exprReturn')
  return (I.AltDefault var, retExpr)

-- | TODO: type is currently wrong and is only a placeholder
-- | to fix, will need to enumerate through all expr constructors
-- | makes more sense to do when inserting dups
insertDropAlt (I.AltData dcon vars, exprReturn) exprMatch = do
  retVar      <- getFresh "_alt_data"
  exprReturn' <- insertDropExpr exprReturn
  let makeDrops = \v -> makeDrop $ I.Var (fromJust v) (I.extract exprMatch)
      exprBins  = (Just retVar, exprReturn') : map makeDrops vars
      retExpr   = seqExprs exprBins $ I.Var retVar (I.extract exprReturn')
  return (I.AltData dcon vars, retExpr)

insertDropAlt (I.AltLit l, exprReturn) _ = do
  return (I.AltLit l, exprReturn)
