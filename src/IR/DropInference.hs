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
import           Data.Maybe                     ( fromJust
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
makeDrop :: I.Expr Poly.Type -> I.Expr Poly.Type -> I.Expr Poly.Type
makeDrop r e = I.Prim I.Drop [e, r] $ I.extract e

-- | Make a dup primitive with actual type.
makeDup :: I.Expr Poly.Type -> I.Expr Poly.Type
makeDup e = I.Prim I.Dup [e] $ I.extract e

{-
-- | Given @a@ and @b@, construct @a ; b@ (i.e., @let _ = a ; b@). 
seqExpr :: (Maybe I.VarId, I.Expr a) -> I.Expr a -> I.Expr a
seqExpr a b = I.Let [a] b (I.extract b)

-- | Given @[a1 .. an]@ and @b@, construct @a1 ; .. ; an ; b@. 
seqExprs :: [(Maybe I.VarId, I.Expr a)] -> I.Expr a -> I.Expr a
seqExprs as b = foldr seqExpr b as
-}

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
insertDropExpr app@(I.App _ _ _ ) = do
  let (fun, args) = I.unzipApp app
  args' <- mapM insertDropTypedExpr args
  fun' <- insertDropLeft fun
  return $ I.zipApp fun' args'
  where
     insertDropTypedExpr (e, t) = do
       e' <- insertDropExpr e
       return (e', t)
     insertDropLeft e@(I.Prim {})  = return e
     insertDropLeft e@(I.Var{})    = return e  -- Don't dup f in (f e1 e2)
     insertDropLeft e@(I.Data{})   = return e
     insertDropLeft e@(I.Lambda{}) = return e
     insertDropLeft e = insertDropExpr e

-- Inserting drops into let bindings.
insertDropExpr (I.Let bins expr typ) = do
  bins' <- forM bins droppedBinder

  expr' <- insertDropExpr expr

  let retExpr' = foldr makeDrop expr' $ map varFromBind bins'
  return $ I.Let bins' retExpr' typ
  where
    varFromBind (v, d) = I.Var (fromJust v) (I.extract d)
    
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
  let (args, body) = I.collectLambda lam
      (argTypes, _) = collectArrow typ
  
  args' <- forM args $ maybe (getFresh "_arg") return -- handle _ arguments
  let typedArgs = zipWith (\a b -> (Just a, b)) args' argTypes 
      argVars = zipWith I.Var args' argTypes
      
  body' <- insertDropExpr body
  
  return $ I.makeLambdaChain typedArgs $ foldr makeDrop body' argVars

-- Inserting drops into pattern-matching.

{-
insertDropExpr match@(I.Match expr alts typ) = do
  alts' <- forM alts $ \(v, e) -> do
    insertDropAlt (v, e) expr
  return $ I.Match expr' alts' typ
-}

insertDropExpr (I.Match expr alts typ) = do
   expr' <- insertDropExpr expr
   alts' <- forM alts insertDropAlt
   return $ I.Match expr' alts' typ
   

-- Inserting dup/drops into primitive's arguments.
insertDropExpr (I.Prim p es typ) = do
  es' <- mapM insertDropExpr es
  return $ I.Prim p es' typ

insertDropExpr var@(I.Var _ _) = do
  return $ makeDup var

insertDropExpr dcon@(I.Data _ _) = do
  return dcon

-- FIXME: catchall
insertDropExpr e = return e

-- | Rewrite an arm of a Match construct
insertDropAlt :: (I.Alt, I.Expr Poly.Type) -> InsertFn (I.Alt, I.Expr Poly.Type)

insertDropAlt (I.AltDefault v, e) = do
  e' <- insertDropExpr e
  return (I.AltDefault v, e')

insertDropAlt (I.AltLit l, e) = do
  e' <- insertDropExpr e
  return (I.AltLit l, e')

insertDropAlt (I.AltData dcon binds, e) = do
  e' <- insertDropExpr e
  return (I.AltData dcon binds, e')


{-
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

-}
