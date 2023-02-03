{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
{- | Simple Inlining Optimization Pass

Performs preinline and postinline unconditionally.
-}
module IR.Simplify
  ( simplifyProgram
  ) where

-- import qualified IR.Types                      as I
import qualified Common.Compiler               as Compiler
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                , gets
                                                , modify
                                                )
-- import           Common.Identifiers
import qualified IR.IR                         as I

import           Data.Bifunctor                 ( second )
-- import Data.Text.Prettyprint.Doc.Render.String (renderShowS)
-- import Common.Compiler (Error)
import           Data.Generics                  ( Typeable )
-- import           Data.List                      ( intersperse )
import qualified Data.Map                      as M
import qualified Data.Maybe                    as Ma
import           IR.IR                          ( unfoldLambda )
-- import           Data.Generics                  ( everywhere
--                                                 , mkT,
--                                                 everywhereM,
--                                                 mkM,
--                                                 Typeable
--                                                 )

-- import qualified GHC.IO.Exception              as Compiler
-- import           Data.Maybe                     ( catMaybes )
-- import qualified Data.Set                      as S

type InVar = I.VarId
type OutVar = I.VarId
type InExpr = I.Expr I.Type
type OutExpr = I.Expr I.Type
type InScopeSet = String
type Context = String

type Subst = M.Map InVar SubstRng

data SubstRng = DoneEx OutExpr | SuspEx InExpr Subst
 deriving Typeable
 deriving Show

{-  | Occurrence Information for each binding

Dead: Does not appear at all
OnceSafe: Appears once, NOT inside a lambda
MultiSafe: The binder occurs at most ONCE in each of several distinct case branches; 
           NONE of these ocurrences is inside a lambda
OnceUnsafe: Binder occurs exactly once, but inside a lambda. 
MultiUnsafe: Binder may occur many times, including inside lambdas. 
           Variables exported from the module are also makred MultiUnsafe.
LoopBreaker: Chosen to break dependency between mutually recursive defintions. 
Never: Never inline; we use this to develop our inliner incrementally.
-}
data OccInfo = Dead
             | LoopBreaker -- TBD
             | OnceSafe
             | MultiSafe
             | OnceUnsafe
             | MultiUnsafe
             | Never
  deriving Show
  deriving Typeable

-- | Simplifier Environment
data SimplEnv = SimplEnv
  { occInfo     :: M.Map I.VarId OccInfo
  {- ^ 'occInfo' maps an identifier to its occurence category -}
  , subst       :: M.Map InVar SubstRng
  {- ^ 'subst' maps an identifier to its substitution -}
  , runs        :: Int
  {- ^ 'runs' stores how many times the simplifier has run so far -}
  , countLambda :: Int
  {- ^ 'countLambda' how many lambdas the occurence analyzer is inside -}
  , countMatch  :: Int
  {- ^ 'countLambda' how many matches the occurence analyzer is inside -}
  }
  deriving Show
  deriving Typeable

-- | Simplifier Monad
newtype SimplFn a = SimplFn (StateT SimplEnv Compiler.Pass a)
  deriving Functor                      via (StateT SimplEnv Compiler.Pass)
  deriving Applicative                  via (StateT SimplEnv Compiler.Pass)
  deriving Monad                        via (StateT SimplEnv Compiler.Pass)
  deriving MonadFail                    via (StateT SimplEnv Compiler.Pass)
  deriving (MonadError Compiler.Error)  via (StateT SimplEnv Compiler.Pass)
  deriving (MonadState SimplEnv)        via (StateT SimplEnv Compiler.Pass)
  deriving Typeable

-- | Run a SimplFn computation.
runSimplFn :: SimplFn a -> Compiler.Pass a
-- evalStateT runs function in context and returns final value, without context
-- take state transformer monad and an initial state, and gives the initial state to the transformer monad. (and the transformer monad holds our monadic program)
-- runSimplFn takes in the Compiler.Pass wrapped inside a SimplFn
-- at the moment of evalStateT taking in m and SimplCtx, m has Compiler.Pass (from do) but not SimplCtx yet
-- but so you have m that doesn't have SimplCtx yet?? (IDK) -- look at stackoverflow post
-- state transformer: function that takes in state and returns final value (Compiler.Pass)
-- m is state transformer monad (result of do inside simplifyProgram)
-- SimplCtx is initial state
runSimplFn (SimplFn m) = evalStateT
  m
  SimplEnv { occInfo     = M.empty
           , runs        = 0
           , countLambda = 0
           , countMatch  = 0
           , subst       = M.empty
           }

-- | Add a binder to occInfo with category Dead by default
addOccVar :: I.VarId -> SimplFn ()
addOccVar binder = do
  m <- gets occInfo
  let m' = case M.lookup binder m of
        Nothing -> M.insert binder Dead m
        _       -> error
          (  "ADD: Should never have seen this binder "
          ++ show binder
          ++ " before!"
          ++ "\n"
          ++ "occInfo: "
          ++ show m
          )
  modify $ \st -> st { occInfo = m' }

-- | Update occInfo for the binder since we just spotted it
updateOccVar :: I.VarId -> SimplFn ()
updateOccVar binder = do
  m       <- gets occInfo
  insidel <- insideLambda
  insidem <- insideMatch
  let m' = case M.lookup binder m of
        Nothing -> error
          (  "UDPATE: We should already know about this binder "
          ++ show binder
          ++ " :"
          ++ show m
          ++ "!"
          )
        Just Dead -> do
          -- we only handle OnceSafe currently
          -- if we're inside a lambda, binder is NOT OnceSafe (in fact, it's OnceUnsafe...)
          if insidel || insidem
            then M.insert binder Never m
            else M.insert binder OnceSafe m
        _ -> M.insert binder Never m
  modify $ \st -> st { occInfo = m' }

insertSubst :: I.VarId -> SubstRng -> SimplFn ()
insertSubst binder rng = do
  m <- gets subst
  let m' = M.insert binder rng m
  modify $ \st -> st { subst = m' }

-- | Record that the ocurrence analyzer is looking inside a lambda
recordEnteringLambda :: SimplFn ()
recordEnteringLambda = do
  curCount <- gets countLambda
  modify $ \st -> st { countLambda = curCount + 1 }

-- | Record that the ocurrence analyzer is no longer looking inside a lambda
recordExitingLambda :: SimplFn ()
recordExitingLambda = do
  curCount <- gets countLambda
  modify $ \st -> st { countLambda = curCount - 1 }

-- | Returns whether ocurrence analyzer is currently looking inside a lambda
insideLambda :: SimplFn Bool
insideLambda = do
  curCount <- gets countLambda
  return (curCount /= 0)

-- | Record that the ocurrence analyzer is looking inside a match
recordEnteringMatch :: SimplFn ()
recordEnteringMatch = do
  curCount <- gets countMatch
  modify $ \st -> st { countMatch = curCount + 1 }

-- | Record that the ocurrence analyzer is no longer looking inside a match
recordExitingMatch :: SimplFn ()
recordExitingMatch = do
  curCount <- gets countMatch
  modify $ \st -> st { countMatch = curCount - 1 }

-- | Returns whether ocurrence analyzer is currently looking inside a match
insideMatch :: SimplFn Bool
insideMatch = do
  curCount <- gets countMatch
  return (curCount /= 0)

{- | Entry-point to Simplifer.

Maps over top level definitions to create a new simplified Program
Do we ever want to inline top-level definitions?
-}
simplifyProgram :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
simplifyProgram p = runSimplFn $ do -- everything in do expression will know about simplifier environment
                                    -- run this do expression and give it the knowledge of my simplifier environment
  _                     <- runOccAnal p -- run the occurrence analyzer
  -- fail and print out the results of the occurence analyzer
  -- info <- runOccAnal p
  -- _ <- Compiler.unexpected $ show info
  simplifiedProgramDefs <- mapM simplTop (I.programDefs p)
  return $ p { I.programDefs = simplifiedProgramDefs } -- this whole do expression returns a Compiler.Pass

-- | Simplify a top-level definition
simplTop :: (I.VarId, I.Expr I.Type) -> SimplFn (I.VarId, I.Expr I.Type)
simplTop (v, e) = do
  (,) v <$> simplExpr M.empty "inscopeset" e "context"

{- | Simplify an IR expression.

Probably want more documentation here eventually.
For now we ignore the in scope set and context args.

- Var VarId t                           DONE (except callsite inline)
- Data DConId t                         Default case             
- Lit Literal t                         Default case
- App (Expr t) (Expr t) t               DONE
- Let [(Binder, Expr t)] (Expr t) t     DONE (except callsite inline)
- Lambda Binder (Expr t) t              DONE 
- Match (Expr t) [(Alt, Expr t)] t      DONE  
- Prim Primitive [Expr t] t             DONE

-}
simplExpr :: Subst -> InScopeSet -> InExpr -> Context -> SimplFn OutExpr
{- | Simplify Primitive Expression

  More Intuitive Version:
  let partiallyApplied = (map (simplExpr sub ins) args)
  let fullyApplied = map ($ cont) partiallyApplied
  unwrappedAndRecollected <- sequence fullyApplied
  pure (I.Prim prim unwrappedAndRecollected t)
-}
simplExpr sub ins (I.Prim prim args t) cont = do
  args' <- mapM (($ cont) . simplExpr sub ins) args
  pure (I.Prim prim args' t)

-- Simplify Match Expression
simplExpr sub ins (I.Match scrutinee arms t) cont = do
  scrutinee' <- simplExpr sub ins scrutinee cont
  let (pats, rhss) = unzip arms
  rhss' <- sequence $ mapM (simplExpr sub ins) rhss cont
  let results = zip pats rhss'
  return (I.Match scrutinee' results t)

-- Simplify Application Expression
simplExpr sub ins (I.App lhs rhs t) cont = do
  lhs' <- simplExpr sub ins lhs cont
  rhs' <- simplExpr sub ins rhs cont
  return (I.App lhs' rhs' t)

-- Simplify Lambda Expression
simplExpr sub ins (I.Lambda binder body t) cont = do
  body' <- simplExpr sub ins body cont
  pure (I.Lambda binder body' t)

-- Simplify Variable Expression
simplExpr _ ins var@(I.Var v _) cont = do
  m <- gets subst
  case M.lookup v m of
    Nothing           -> pure var -- callsite inline, future work
    Just (SuspEx e s) -> simplExpr s ins e cont
    Just (DoneEx e  ) -> simplExpr M.empty ins e cont

-- Simplify Let Expressions
simplExpr sub ins (I.Let binders body t) cont = do
  simplified <- mapM simplBinder binders
  let (simplBinders, subs) = unzip simplified
  let binders'             = Ma.catMaybes simplBinders
  let subs'                = foldr1 (<>) subs
  body' <- simplExpr subs' ins body cont
  if null binders' then pure body' else pure (I.Let binders' body' t)
 where
  simplBinder
    :: (I.Binder, I.Expr I.Type)
    -> SimplFn (Maybe (I.Binder, I.Expr I.Type), Subst)
  simplBinder (binder, rhs) = do
    m <- gets occInfo
    case binder of
      (Just v) -> case M.lookup v m of
        (Just Dead    ) -> pure (Nothing, sub)  -- get rid of this dead binding
        (Just OnceSafe) -> do -- preinline test PASSES
          -- bind x to E singleton :: k -> a -> Map k a
          insertSubst v $ SuspEx rhs M.empty
          let sub' = M.singleton v (SuspEx rhs sub)
          pure (Nothing, sub')
        _ -> do -- preinline test FAILS, so do post inline unconditionally
          e' <- simplExpr sub ins rhs cont -- process the RHS
          case e' of
            -- x goes here. 
            (I.Lit _ _) -> do
              insertSubst v $ DoneEx e'
              pure (Nothing, M.empty) -- PASSES postinline
            (I.Var _ _) -> do
              insertSubst v $ DoneEx e'
              pure (Nothing, M.empty) -- PASSES postinline
            _ -> pure (Just (binder, rhs), sub) -- FAILS postinline; someday callsite inline
      _ -> do
        e' <- simplExpr sub ins rhs cont
        pure (Just (binder, e'), sub) -- can't inline wildcards

-- for all other expressions, don't do anything
simplExpr _ _ e _ = pure e




{- | Take in a top level function, and "swallows" its arguments

"Swallow" means to add the argument to our occurrence info state.
It returns a top level function without curried arguments; just the body.
unfoldLambda :: Expr t -> ([Binder], Expr t)
-}
swallowArgs :: (I.VarId, I.Expr t) -> SimplFn (I.VarId, I.Expr t)
swallowArgs (funcName, l@(I.Lambda _ _ _)) = do
  addOccs (Just funcName) -- HACKY
  let (args, body) = unfoldLambda l
  mapM_ addOccs args
  pure (funcName, body)
 where
  addOccs (Just nm) = addOccVar nm
  addOccs Nothing   = pure ()

swallowArgs (name, e) = pure (name, e)


runOccAnal :: I.Program I.Type -> SimplFn (String)
runOccAnal p@I.Program { I.programDefs = defs } = do
  {-
  stack build
  cd regression-tests
  ./runtests.sh -k tests/a-inlining-ex-1.ssl
  stack exec sslc -- --dump-ir tests/a-inlining-ex-1.ssl
  stack exec sslc -- --dump-ir-typed tests/a-inlining-ex-1.ssl
  stack exec sslc -- --dump-ir-lifted tests/a-inlining-ex-1.ssl
  -}
  -- a hacky way to see the occurence info for each top def
  let getOccInfoForDef
        :: (I.VarId, SimplFn (I.Expr I.Type, String)) -> SimplFn String
      getOccInfoForDef (v, tpl) = do
        (_, occinfo) <- tpl
        pure
          $  "START topdef "
          ++ show v
          ++ " has OccInfo: "
          ++ show occinfo
          ++ " END"
  defs' <- mapM swallowArgs defs
  x     <- mapM (getOccInfoForDef . second occAnalExpr) defs'
  --y <- snd $ second occAnalExpr (head defs)
  --error $ show x
 -- addOccVar "yo"
 -- m <- gets occInfo
 -- error (show m)
  return (show x)



-- | Run the Ocurrence Analyzer over an IR expression node
occAnalExpr :: I.Expr I.Type -> SimplFn (I.Expr I.Type, String)

-- let expressions
occAnalExpr l@(I.Let binders body _) = do
  mapM_
    (\(binder, rhs) -> do
      _ <- occAnalExpr rhs
      case binder of
        (Just nm) -> addOccVar nm
        _         -> pure ()
    )
    binders
  _ <- occAnalExpr body
  m <- gets occInfo
  pure (l, show m)

-- variables
occAnalExpr var@(I.Var v _) = do
  updateOccVar v
  m <- gets occInfo
  return (var, show m)

-- lambdas
occAnalExpr l@(I.Lambda Nothing b _) = do
  recordEnteringLambda
  _ <- occAnalExpr b
  recordExitingLambda
  m <- gets occInfo
  pure (l, show m)

occAnalExpr l@(I.Lambda (Just binder) b _) = do
  recordEnteringLambda
  addOccVar binder
  _ <- occAnalExpr b
  recordExitingLambda
  m <- gets occInfo
  pure (l, show m)

-- application
-- App (Expr t) (Expr t) t
{-
 let r = q + 1 
 App (App "+" q) 1
-}
occAnalExpr a@(I.App lhs rhs _) = do
  _ <- occAnalExpr lhs
  _ <- occAnalExpr rhs
  m <- gets occInfo
  pure (a, show m)

-- primitive operations
{-
  | Prim Primitive [Expr t] t
  ^ @Prim p es t@ applies primitive @p@ arguments @es@, producing a value
  of type @t@.
  
  data Primitive
  = New
  | PrimOp PrimOp
  {- ^ Inlined C expression code. -}
  | CQuote String
  {- ^ Primitive operator. -}
  | CCall CSym
  {- ^ Direct call to arbitrary C function (NOTE: HACKY). -}
  | FfiCall VarId
  {- ^ Call to well-typed extern symbol. -}
  deriving (Eq, Show, Typeable, Data)

  when primitive is
-}

-- primitives
occAnalExpr p@(I.Prim _ args _) = do
  mapM_ occAnalExpr args
  m <- gets occInfo
  pure (p, show m)

-- match
occAnalExpr p@(I.Match scrutinee arms _) = do
  recordEnteringMatch
  _ <- occAnalExpr scrutinee
  --let (alts, rhss) = unzip arms
  mapM_ (occAnalExpr . snd) arms
  --let x = everywhereM (mkM addOccExprNever) p
 -- let x = everywhereM (mkM addOccExprNever) p
  --addOccVarNever scrutinee
  recordExitingMatch
  m <- gets occInfo
  pure (p, show m)

-- data constructors
{-
According to section 2.3 in the paper, the trivial-constructor-argument-invariant says that
if we have
  x = (f y, g y)
  h = \ z -> case x of 
                 (a,b) -> ... 
Normally we would want to inline x so we can could cancel the case statement, BUT
because the arguments to the tuple data constructor are FUNCTIONS, this could duplicate work.
Because if h appears in a bunch of places, we are redo-ing the work for f y and g y a bunch of times.
trivial-constructor-argument-invariant: It's okay to inline a binder whose RHS 
is an application of a data constructor IF the arguments to the data constructors
are variable, literals, or type applications.

let x = RGB (let y = 5 in y + 2) 4 5 //dumb but possible


data Color =
  RGB Int Int Int
  Black
  White
-}

-- occAnalExpr p@(I.Data args _) = do 
--   pure ()

-- match expressions???? 

-- everything else
occAnalExpr e = do
  m <- gets occInfo
  pure (e, show m)

{-
data Expr t
  = Var VarId t                                DONE
  | Data DConId t                              DONE
  | Lit Literal t                              DONE
  | App (Expr t) (Expr t) t                    DONE
  | Let [(Binder, Expr t)] (Expr t) t
  | Lambda Binder (Expr t) t
  | Match (Expr t) [(Alt, Expr t)] t
  | Prim Primitive [Expr t] t
-}

-- -- | Add a binder to occInfo with category Never
-- addOccExprNever :: I.Expr t-> SimplFn ()
-- addOccExprNever e@(I.Var v _) = do
--   m <- gets occInfo
--   let m' = M.insert v Never m
--   modify $ \st -> st {occInfo = m'}
--   pure ()
-- addOccExprNever (I.Lit _ _) = pure ()
-- addOccExprNever (I.Data _ _) = pure ()
-- -- nullary data constructor
-- addOccExprNever a@(I.App rhs lhs t) = do
--   _ <- addOccExprNever lhs
--   _ <- addOccExprNever rhs
--   pure ()
-- addOccExprNever a@(I.Let bindings@[(binder, rhs)] body t) = do
--   _ <- addOccExprNever body
--   pure ()
-- addOccExprNever _ = pure () -- to get rid of warnings; delete later

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

{- | Given the RHS of a top level function, return first expr inside it that isn't a Lambda

For each curried lambda expr I find, add binders to occurence info.
Basically, "swallow up" the arguments to my top-level function.
-}
-- swallowArgs :: I.Expr I.Type -> SimplFn (I.Expr I.Type)
-- swallowArgs (I.Lambda Nothing b _) = swallowArgs b
-- swallowArgs (I.Lambda (Just arg) b _)= do
--   addOccVar arg 
--   swallowArgs b
-- swallowArgs e = pure e


{-
Mututally Recursive:
g a = f a


f x = match x 
            5 = 5
            _ = g (x-1)

main cin cout = 
  let x = 5
      y = x + 2
      


Not mutually Recursive
main cin cout =
  let x = 5
      y = 3
      x + y
-}
