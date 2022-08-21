{-# LANGUAGE DerivingVia #-}

module IR.DropInference
 ( insertDropsProgram
 ) where

import qualified Common.Compiler               as Compiler
import           Common.Identifiers
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                , forM
                                                , gets
                                                , modify
                                                )
import           Data.Maybe                     ( catMaybes
                                                , fromJust
                                                )
import qualified Data.Set                      as S
import qualified IR.IR                         as I
import qualified IR.Types.Poly                 as Poly
import           IR.Types.TypeSystem            ( collectArrow
                                                )

-- | Inserting State.
data InsertState = InsertState
  { globScope :: S.Set I.VarId
  , currScope :: S.Set I.VarId
  , newDrops  :: [(I.VarId, I.Expr Poly.Type)]
  , anonCount :: Int
  }

-- | Insert Monad.
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

-- | Get a fresh variable name.
getFresh :: String -> InsertFn I.VarId
getFresh str = do
  curCount <- gets anonCount
  modify $ \st -> st { anonCount = anonCount st + 1 }
  return $ fromString $ ("anon" <> show curCount) ++ str

-- | Make a drop primitive with unit type.
makeDrop :: I.Expr Poly.Type -> I.Expr Poly.Type -> (Maybe a, I.Expr Poly.Type)
makeDrop e r = (Nothing, I.Prim I.Drop [e, r] $ I.extract e)

-- | Make a dup primitive with actual type.
makeDup :: I.Expr Poly.Type -> (Maybe a, I.Expr Poly.Type)
makeDup e = (Nothing, I.Prim I.Dup [e] $ I.extract e)

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

-- Inserting dup/drops into function application with arg.
-- FIXME: Should we be adding a dup to the argument?
insertDropExpr (I.App fun arg typ) = do
  arg' <- insertDropExpr arg
  return $ I.App fun arg' typ

-- Skip let bindings that have unit type.
--insertDropExpr (I.Let bins expr typ@(Poly.TBuiltin Poly.Unit)) = do
--  return $ I.Let bins expr typ

-- Don't bother dropping let-bound value let _ = expr1 in expr2
-- FIXME: This is naive, and might be better done as an optimization
{- insertDropExpr (I.Let [(Nothing, expr1)] expr2 typ) = do
  expr1' <- insertDropExpr expr1
  expr2' <- insertDropExpr expr2
  return $ I.Let [(Nothing, expr1')] expr2' typ
-}

-- Inserting drops into let bindings.
-- FIXME: No point in generating a new variable and returning it if the
-- body returns unit.  An optimization?
insertDropExpr (I.Let bins expr typ) = do
  retVar <- getFresh "_let"
  expr'  <- insertDropExpr expr
  bins'  <- forM bins droppedBinder
  let makeFun f = \b -> f $ I.Var (fromJust $ fst b) (I.extract $ snd b)
      dropBins = map (makeFun makeDrop) bins'
      exprBins = (Just retVar, expr') : dropBins
      retExpr  = I.Var retVar typ
      retExpr' = seqExprs exprBins retExpr
  return $ I.Let bins' retExpr' typ
  where
    droppedBinder (Nothing, d) = do
      temp <- getFresh "_underscore"
      d' <- insertDropExpr d
      return (Just temp, d')
    droppedBinder (v, d) = do
      d' <- insertDropExpr d
      return (v, d')

-- Handle nested lambdas by collecting them into a single expression
-- with multiple arguments, adding a local result variable,
-- drops for each of the arguments, and return value
insertDropExpr lam@(I.Lambda _ _ typ) = do
  retVar <- getFresh "_result"
  
  let (args, body) = I.collectLambda lam
      (argTypes, retType) = collectArrow typ
      typedArgs = zip args argTypes

      -- FIXME: fromJust may fail here: how should that be handled?
      -- See, e.g., hello10.ssl
      dropExprs = map (\(v, t) -> makeDrop $ I.Var (fromJust v) t) typedArgs

  body' <- insertDropExpr body

  let exprBins = (Just retVar, body') : dropExprs
      retExpr = I.Var retVar retType
      retExpr' = seqExprs exprBins retExpr
  
  return $ I.makeLambdaChain typedArgs retExpr'

-- Inserting drops into pattern-matching.
insertDropExpr (I.Match expr alts typ) = do
  alts' <- forM alts $ \(v, e) -> do
    insertDropAlt (v, e) expr
  return $ I.Match expr alts' typ

-- Inserting dup/drops into primitive's arguments.
insertDropExpr (I.Prim p es typ) = do
  es' <- mapM insertDropExpr es
  return $ I.Prim p es' typ

-- placeholder for other exprs.
insertDropExpr expr = return expr


-- | Entry-point to inserting into pattern-matching arms.
insertDropAlt
  :: (I.Alt, I.Expr Poly.Type)
  -> I.Expr Poly.Type
  -> InsertFn (I.Alt, I.Expr Poly.Type)

-- Inserting dup/drops into AltDefault arms.
insertDropAlt (I.AltDefault var, exprReturn) exprMatch = do
  retVar      <- getFresh "_alt_default"
  exprReturn' <- insertDropExpr exprReturn
  let varDup   = makeDup exprMatch
      varDrop  = makeDrop exprMatch
      exprBins = varDup : (Just retVar, exprReturn') : [varDrop]
      retExpr  = I.Var retVar (I.extract exprReturn')
      retExpr' = seqExprs exprBins retExpr
  return (I.AltDefault var, retExpr')

-- Inserting dup/drops into AltData arms.
-- TODO: type info is currently unavailable and is only a placeholder.
insertDropAlt (I.AltData dcon vars, exprReturn) _ = do
  retVar      <- getFresh "_alt_data"
  exprReturn' <- insertDropExpr exprReturn
  let retTyp = I.extract exprReturn'
      makeFun f = \v -> f $ I.Var v retTyp
      dupBins  = map (makeFun makeDup) $ catMaybes vars
      dropBins = map (makeFun makeDrop) $ catMaybes vars
      exprBins = dupBins ++ (Just retVar, exprReturn') : dropBins
      retExpr  = I.Var retVar retTyp
      retExpr' = seqExprs exprBins retExpr
  return (I.AltData dcon vars, retExpr')

-- Inserting dup/drops into other pattern match arms.
insertDropAlt (I.AltLit l, exprReturn) _ = do
  exprReturn' <- insertDropExpr exprReturn
  return (I.AltLit l, exprReturn')
