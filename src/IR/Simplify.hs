{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{- | Lift lambda definitions into the global scope.

This pass is responsible for moving nested lambda definitions into the global
scope and performing necessary callsite adjustments.
-}
module IR.Simplify
  ( simplifyProgram
  ) where

-- import qualified IR.Types                      as I
import qualified Common.Compiler               as Compiler
-- import           Common.Identifiers
import qualified IR.IR                         as I

-- import           Control.Monad                  ( forM_ )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                , gets
                                                , modify
                                                -- , unless
                                                )

-- import           Data.Bifunctor                 ( first )
-- import           Data.List                      ( intersperse )
import qualified Data.Map                      as M
-- import           Data.Maybe                     ( catMaybes )
-- import qualified Data.Set                      as S

-- | Occurrence Information for each binding
data OccInfo = Dead
             | LoopBreaker -- TBD
             | OnceSafe
             | MultiSafe
             | OnceUnsafe
             | MultiUnsafe
             | Never

-- | Simplifier Environment
data SimplEnv = SimplEnv
  { occInfo :: M.Map I.VarId OccInfo
  {- ^ 'occInfo' maps an identifier to its occurence category -}
  , runs    :: Int
  {- ^ 'runs' stores how many times the simplifier has run so far -}
  }

-- | Simplifier Monad
newtype SimplFn a = SimplFn (StateT SimplEnv Compiler.Pass a)
  deriving Functor                      via (StateT SimplEnv Compiler.Pass)
  deriving Applicative                  via (StateT SimplEnv Compiler.Pass)
  deriving Monad                        via (StateT SimplEnv Compiler.Pass)
  deriving MonadFail                    via (StateT SimplEnv Compiler.Pass)
  deriving (MonadError Compiler.Error)  via (StateT SimplEnv Compiler.Pass)
  deriving (MonadState SimplEnv)        via (StateT SimplEnv Compiler.Pass)

-- | Run a SimplFn computation.
runSimplFn :: SimplFn a -> Compiler.Pass a
--evalStateT runs function in context and returns final value, without context
  -- take state transformer monad and an initial state, and gives the initial state to the transformer monad. (and the transformer monad holds our monadic program)
      -- runSimplFun takes in the Compiler.Pass wrapped inside a SimplFn
      -- at the moment of evalStateT taking in m and SimplCtx, m has Compiler.Pass (from do) but not SimplCtx yet
        -- but so you have m that doesn't have SimplCtx yet?? (IDK) -- look at stackoverflow post
          -- state transformer: function that takes in state and returns final value (Compiler.Pass)
  -- m is state transformer monad (result of do inside simplifyProgram)
  -- SimplCtx is initial state
runSimplFn (SimplFn m) = evalStateT m SimplEnv { occInfo = M.empty, runs = 0 }

-- | Add a binder to occInfo with category Dead by default
updateOccVar :: I.VarId -> SimplFn ()
updateOccVar binder = do
  m <- gets occInfo
  let m' = case M.lookup binder m of
        Nothing   -> error "Should have seen this binder before!"
        Just Dead -> M.insert binder OnceSafe m -- we only handle oncesafe currently
        _         -> M.insert binder Never m
  modify $ \st -> st { occInfo = m' }

addOccVar :: I.VarId -> SimplFn ()
addOccVar binder = do
  m <- gets occInfo
  let m' = case M.lookup binder m of
        Nothing -> M.insert binder Dead m
        _       -> error "Should never have never seen this binder before!"
  modify $ \st -> st { occInfo = m' }



{- | Entry-point to Simplifer.

Maps over top level definitions to create a new simplified Program
Do we ever want to inline top-level definitions?
-}
simplifyProgram :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
simplifyProgram p = runSimplFn $ do -- everything in do expression will know about simplifier environment
                                    -- run this do expression and give it the knowledge of my simplifier environment
  -- run the occurrence analyzer
  let defs = I.programDefs p
  simplifiedProgramDefs <- mapM simplTop defs
  return $ p { I.programDefs = simplifiedProgramDefs } -- this whole do expression returns a Compiler.Pass

-- | Simplify a top-level definition
simplTop :: (I.VarId, I.Expr I.Type) -> SimplFn (I.VarId, I.Expr I.Type)
simplTop (v, e) = (,) v <$> simplExpr e

{-
data Program t = Program
  { programEntry :: VarId
  , cDefs        :: String
  , externDecls  :: [(VarId, Type)]
  , programDefs  :: [(VarId, Expr t)]
  , typeDefs     :: [(TConId, TypeDef)]
  }
  deriving (Eq, Show, Typeable, Data, Functor)
  User{name=name, favoriteFood=Just food}

  f a = a

  main cin cout = 
    let x = 4
    let y = f x
    ()

  [("f", I.Lambda a -> a)
  ,("main", "cin cout = 
    let x = 4
    let y = f x
    ()" )]
  case x of 
    RGB 1 2 _ = 
    ...
  let x = 4
      y = 3
      _ = 8
  IN
  y + x
  
-}

runOccAnal :: I.Program I.Type -> SimplFn (I.Program I.Type)
runOccAnal p@I.Program { I.programDefs = defs } = do

--(map . second) occAnalExpr defs

  return p

occAnalExpr :: I.Expr I.Type -> SimplFn (I.Expr I.Type)
occAnalExpr (I.Let binders body _) = do
  mapM_
    (\(binder, _) -> case binder of
      (Just nm) -> do
        addOccVar nm
      Nothing -> pure ()
    )
    binders
  occAnalExpr body
occAnalExpr var@(I.Var v _) = do updateOccVar v
                                 return var

-- --occAnalExpr (I.Var varId) = -- first time you see it, dead -> oncesafe
occAnalExpr e = pure e


{-
main cin cout =
let x = (\a -> a + 1)
specialFunc (RHS of x) -- this only updates binders, and we ignore a
IN
let y = x 4
specialFunc (RHS of y) -- this only updates binders, so x is updated
IN
let y = (\a -> a + 1 + (x 4)) -- this only updates binders, and we ignore a and update x
IN
y
  | Data DConId t // don't care
  {- ^ @Data d t@ is a data constructor named @d@ of type @t@. -}
  | Lit Literal t // don't care
  {- ^ @Lit l t@ is a literal value @l@ of type @t@. -}

    App x 4
  | App (Expr t) (Expr t) t 
  {- ^ @App f a t@ applies function @f@ to argument @a@, producing a value of
  type @t@.
  -}

  The bindings list may only be of length greater than 1 for a set of mutually
  co-recursive functions.
--   -}
--   | Lambda Binder (Expr t) t
--   {- ^ @Lambda v b t@ constructs an anonymous function of type @t@ that binds
--   a value to parameter @v@ in its body @b@.
--   -}
--   | Match (Expr t) [(Alt, Expr t)] t
--   {- ^ @Match s alts t@ pattern-matches on scrutinee @s@ against alternatives
--   @alts@, each producing a value of type @t@.
--   -}
--   | Prim Primitive [Expr t] t
--   {- ^ @Prim p es t@ applies primitive @p@ arguments @es@, producing a value
--   of type @t@.
--   -}
-- -}



{- | Recursively simplify IR expressions.

Probably want more documentation here eventually.

-}
simplExpr :: I.Expr I.Type -> SimplFn (I.Expr I.Type)
simplExpr e = do
  addOccVar "test"
  return e -- what a stub.

{-
thoughts:
Decouple occurence analyzer from simplExpr

have occurence analyzer fill in SimplEnv and just feed it into runSimplFn; 

runSimplFn will purely care about inlining
-}

{-
Example run of Ocurrence Analyzer
main cin cout =
let x = (\a -> a + 1)
IN
let y = x 4
IN
y

Result is wrong:
(x, Dead)
(y, Dead) -> (y, Oncesafe)

We want this result:
(x, Dead) -> (x, OnceSage)
(y, Dead) -> (y, Oncesafe)

Solution:
main cin cout =
let x = (\a -> a + 1)
specialFunc (RHS of x) -- this only updates binders, so we ignore a
IN
let y = x 4
specialFunc (RHS of y) -- this only updates binders, so x is updated
IN
y

Plan:
add specialFunc
-}
