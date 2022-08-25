{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}
-- | Insert reference counting primitives.
module IR.InsertRefCounting
  ( insertRefCounting
  ) where

import qualified Common.Compiler               as Compiler
import           Common.Identifiers
import           Control.Monad.State.Lazy       ( MonadState(..)
                                                , StateT(..)
                                                , evalStateT
                                                , forM
                                                , modify
                                                )
import           Data.Maybe                     ( fromJust )
import qualified IR.IR                         as I
import qualified IR.Types.Poly                 as Poly
import           IR.Types.TypeSystem            ( collectArrow
                                                , unit
                                                )

type Fresh = StateT Int Compiler.Pass

-- | Get a fresh variable name.
getFresh :: String -> Fresh I.VarId
getFresh str = do
  curCount <- get
  modify (+ 1)
  return $ fromString $ ("anon" <> show curCount) ++ str

-- | Make a drop primitive with unit type.
makeDrop :: I.Expr Poly.Type -> I.Expr Poly.Type -> I.Expr Poly.Type
makeDrop r e = I.Prim I.Drop [e, r] unit

-- | Make a dup primitive with actual type.
makeDup :: I.Expr Poly.Type -> I.Expr Poly.Type
makeDup e = I.Prim I.Dup [e] $ I.extract e

-- | Entry-point to insert dup/drops.
insertRefCounting :: I.Program Poly.Type -> Compiler.Pass (I.Program Poly.Type)
insertRefCounting program = (`evalStateT` 0) $ do
  let programDefs = I.programDefs program
  defs <- mapM insertTop programDefs
  return $ program { I.programDefs  = defs
                   , I.programEntry = I.programEntry program
                   , I.typeDefs     = I.typeDefs program
                   }


-- | Insert referencing counting for top-level expressions.
insertTop :: (I.VarId, I.Expr Poly.Type) -> Fresh (I.VarId, I.Expr Poly.Type)
insertTop (var, expr) = (var, ) <$> insertExpr expr

-- | Insert reference counting into expressions.
insertExpr :: I.Expr Poly.Type -> Fresh (I.Expr Poly.Type)
insertExpr (I.App f x t) = I.App <$> insertExpr f <*> insertExpr x <*> pure t
insertExpr (I.Let bins expr typ) = do
  bins' <- forM bins droppedBinder
  expr' <- insertExpr expr
  return $ I.Let bins' (foldr (makeDrop . varFromBind) expr' bins') typ
 where
  varFromBind (v, d) = I.Var (fromJust v) (I.extract d)
  droppedBinder (Nothing, d) = do
    temp <- getFresh "_underscore"
    d'   <- insertExpr d
    return (Just temp, d')
  droppedBinder (v, d) = do
    d' <- insertExpr d
    return (v, d')
insertExpr lam@(I.Lambda _ _ typ) = do
  -- Handle nested lambdas by collecting them into a single expression
  -- with multiple arguments, adding a local result variable,
  -- drops for each of the arguments, and return value
  let (args    , body) = I.collectLambda lam
      (argTypes, _   ) = collectArrow typ
  args' <- forM args $ maybe (getFresh "_arg") return -- handle _ arguments
  let typedArgs = zipWith (\a b -> (Just a, b)) args' argTypes
      argVars   = zipWith I.Var args' argTypes
  body' <- insertExpr body
  return $ I.makeLambdaChain typedArgs $ foldr makeDrop body' argVars

insertExpr (I.Match v@I.Var{} alts typ) = do
  -- Note that we do not need to do anything for the scrutinee;
  -- we just recurse on the alts.
  alts' <- forM alts insertAlt
  return $ I.Match v alts' typ
insertExpr (I.Match scrutExpr alts typ) = do
  -- Desugar scrutinee into a variable so we can duplicate it as appropriate
  --
  -- @@
  --   match e
  --     alt1 =
  --     alt2 =
  -- @@
  --
  -- into
  --
  -- @@
  --   let s = e
  --   match s
  --     alt1 =
  --     alt2 =
  -- @@
  scrutVar <- getFresh "_scrutinee"
  insertExpr $ I.Let [(Just scrutVar, scrutExpr)]
                     (I.Match (I.Var scrutVar $ I.extract scrutExpr) alts typ)
                     typ
insertExpr (I.Prim p es typ) = I.Prim p <$> mapM insertExpr es <*> pure typ
insertExpr var@I.Var{}       = return $ makeDup var
insertExpr dcon@I.Data{}     = return dcon
insertExpr lit@I.Lit{}       = return lit

-- | Insert ref counting into pattern match arms.
insertAlt :: (I.Alt, I.Expr Poly.Type) -> Fresh (I.Alt, I.Expr Poly.Type)
insertAlt (I.AltDefault v      , e   ) = (I.AltDefault v, ) <$> insertExpr e
insertAlt (I.AltLit     l      , e   ) = (I.AltLit l, ) <$> insertExpr e
insertAlt (I.AltData dcon binds, body) = do
  body' <- insertExpr body
  return (I.AltData dcon binds, foldr dropDupLet body' binds)
 where
  dropDupLet Nothing  e = e
  dropDupLet (Just v) e = makeDrop
    varExpr
    (I.Let [(Nothing, makeDup varExpr)] e (I.extract e))
    where varExpr = I.Var v (Poly.TBuiltin Poly.Unit) -- FIXME: how do I get its type?
