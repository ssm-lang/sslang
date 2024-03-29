{-# LANGUAGE DerivingVia #-}

{- | Simple Inlining Optimization Pass

Performs preinline and postinline unconditionally.
TODO: Callsite Inline
-}
module IR.Simplify (
  simplifyProgram,
) where

import qualified Common.Compiler as Compiler
import Control.Monad.Except (MonadError (..))
import Control.Monad.State.Lazy (
  MonadState,
  StateT (..),
  evalStateT,
  gets,
  modify,
 )
import Data.Bifunctor (second)
import Data.Generics (Typeable)
import qualified Data.Map as M
import qualified Data.Maybe as Ma
import IR.IR (unfoldLambda)
import qualified IR.IR as I


type InVar = I.VarId


-- | type OutVar = I.VarId -- used by callsite inline
type InExpr = I.Expr I.Type


type OutExpr = I.Expr I.Type
type InScopeSet = String
type Context = String


type Subst = M.Map InVar SubstRng
data SubstRng = DoneEx OutExpr | SuspEx InExpr Subst
  deriving (Typeable)
  deriving (Show)


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
data OccInfo
  = Dead
  | LoopBreaker -- TBD
  | OnceSafe
  | MultiSafe
  | OnceUnsafe
  | MultiUnsafe
  | Never
  | ConstructorFunc
  deriving (Show)
  deriving (Typeable)


-- | Simplifier Environment
data SimplEnv = SimplEnv
  { occInfo :: M.Map I.VarId OccInfo
  -- ^ 'occInfo' maps an identifier to its occurence category
  , subst :: M.Map InVar SubstRng
  -- ^ 'subst' maps an identifier to its substitution
  , runs :: Int
  -- ^ 'runs' stores how many times the simplifier has run so far
  , countLambda :: Int
  -- ^ 'countLambda' how many lambdas the occurence analyzer is inside
  , countMatch :: Int
  -- ^ 'countLambda' how many matches the occurence analyzer is inside
  }
  deriving (Show)
  deriving (Typeable)


-- | Simplifier Monad
newtype SimplFn a = SimplFn (StateT SimplEnv Compiler.Pass a)
  deriving (Functor) via (StateT SimplEnv Compiler.Pass)
  deriving (Applicative) via (StateT SimplEnv Compiler.Pass)
  deriving (Monad) via (StateT SimplEnv Compiler.Pass)
  deriving (MonadFail) via (StateT SimplEnv Compiler.Pass)
  deriving (MonadError Compiler.Error) via (StateT SimplEnv Compiler.Pass)
  deriving (MonadState SimplEnv) via (StateT SimplEnv Compiler.Pass)
  deriving (Typeable)


-- | Run a SimplFn computation.
runSimplFn :: SimplFn a -> Compiler.Pass a
runSimplFn (SimplFn m) =
  evalStateT
    m
    SimplEnv
      { occInfo = M.empty
      , runs = 0
      , countLambda = 0
      , countMatch = 0
      , subst = M.empty
      }


-- | Add a binder to occInfo with category Dead by default
addOccVar :: I.VarId -> SimplFn ()
addOccVar binder = do
  m <- gets occInfo
  let m' = case M.lookup binder m of
        Nothing -> M.insert binder Dead m
        _ -> M.insert binder ConstructorFunc m
  modify $ \st -> st{occInfo = m'}


-- | Update occInfo for the binder since we just spotted it
updateOccVar :: I.VarId -> SimplFn ()
updateOccVar binder = do
  m <- gets occInfo
  insidel <- insideLambda
  insidem <- insideMatch
  let m' = case M.lookup binder m of
        Nothing ->
          error
            ( "UDPATE: We should already know about this binder "
                ++ show binder
                ++ " :"
                ++ show m
                ++ "!"
            )
        Just Dead ->
          -- we only handle OnceSafe currently
          -- if we're inside a lambda, binder is NOT OnceSafe (in fact, it's OnceUnsafe...)
          if insidel || insidem
            then M.insert binder Never m
            else M.insert binder OnceSafe m
        _ -> M.insert binder Never m
  modify $ \st -> st{occInfo = m'}


{- | Add substitution to the substitution set

Suppose we want to replace x with y.
Then we call insertSubst x (SuspEx y {})
-}
insertSubst :: I.VarId -> SubstRng -> SimplFn ()
insertSubst binder rng = do
  m <- gets subst
  let m' = M.insert binder rng m
  modify $ \st -> st{subst = m'}


-- | Record that the ocurrence analyzer is looking inside a lambda
recordEnteringLambda :: SimplFn ()
recordEnteringLambda = do
  curCount <- gets countLambda
  modify $ \st -> st{countLambda = curCount + 1}


-- | Record that the ocurrence analyzer is no longer looking inside a lambda
recordExitingLambda :: SimplFn ()
recordExitingLambda = do
  curCount <- gets countLambda
  modify $ \st -> st{countLambda = curCount - 1}


-- | Returns whether ocurrence analyzer is currently looking inside a lambda
insideLambda :: SimplFn Bool
insideLambda = do
  curCount <- gets countLambda
  return (curCount /= 0)


-- | Record that the ocurrence analyzer is looking inside a match
recordEnteringMatch :: SimplFn ()
recordEnteringMatch = do
  curCount <- gets countMatch
  modify $ \st -> st{countMatch = curCount + 1}


-- | Record that the ocurrence analyzer is no longer looking inside a match
recordExitingMatch :: SimplFn ()
recordExitingMatch = do
  curCount <- gets countMatch
  modify $ \st -> st{countMatch = curCount - 1}


-- | Returns whether ocurrence analyzer is currently looking inside a match
insideMatch :: SimplFn Bool
insideMatch = do
  curCount <- gets countMatch
  return (curCount /= 0)


{- | Entry-point to Simplifer.

Maps over top level definitions to create a new simplified Program
-}
simplifyProgram :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
simplifyProgram p = runSimplFn $ do
  _ <- runOccAnal p -- run the occurrence analyzer
  -- fail and print out the results of the occurence analyzer
  -- info <- runOccAnal p
  -- _ <- Compiler.unexpected $ show info
  simplifiedProgramDefs <- mapM simplTop (I.programDefs p)
  return $ p{I.programDefs = simplifiedProgramDefs} -- this whole do expression returns a Compiler.Pass


-- | Simplify a top-level definition
simplTop :: (I.Binder I.Type, I.Expr I.Type) -> SimplFn (I.Binder I.Type, I.Expr I.Type)
simplTop (v, e) = do
  (,) v <$> simplExpr M.empty "inscopeset" e "context"


{- | Simplify an IR expression.

Probably want more documentation here eventually.
For now we ignore the in scope set and context args.

How do we handle each node?
- Var VarId t                           DONE (except callsite inline)
- Data DConId t                         Default case
- Lit Literal t                         Default case
- App (Expr t) (Expr t) t               DONE
- Let [(Binder, Expr t)] (Expr t) t     DONE (except callsite inline)
- Lambda Binder (Expr t) t              DONE
- Match (Expr t) [(Alt, Expr t)] t      DONE (TODO: match arm elimination)
- Prim Primitive [Expr t] t             DONE
-}
simplExpr :: Subst -> InScopeSet -> InExpr -> Context -> SimplFn OutExpr


{- | Simplify Primitive Expression

  Simplify each of the arguments to prim
  For ex. 5 + g + h + (6 + v)
  Is the primitive "plus" followed by args 5, g, h, and (6 + v)
  SimpleExpr calls itself on each of "plus"'s arguments, then
  returns the result wrapped back up in an I.Prim IR node.
-}
simplExpr sub ins (I.Prim prim args t) cont = do
  args' <- mapM (($ cont) . simplExpr sub ins) args
  pure (I.Prim prim args' t)

-- \| Simplify Match Expression
simplExpr sub ins (I.Match scrutinee arms t) cont = do
  scrutinee' <- simplExpr sub ins scrutinee cont
  let (pats, rhss) = unzip arms
  rhss' <- sequence $ mapM (simplExpr sub ins) rhss cont
  let results = zip pats rhss'
  return (I.Match scrutinee' results t)

-- \| Simplify Application Expression
simplExpr sub ins (I.App lhs rhs t) cont = do
  lhs' <- simplExpr sub ins lhs cont
  rhs' <- simplExpr sub ins rhs cont
  return (I.App lhs' rhs' t)

-- \| Simplify Lambda Expression
simplExpr sub ins (I.Lambda binder body t) cont = do
  body' <- simplExpr sub ins body cont
  pure (I.Lambda binder body' t)

-- \| Simplify Variable Expression
simplExpr _ ins var@(I.Var v _) cont = do
  m <- gets subst
  case M.lookup v m of
    Nothing -> pure var -- callsite inline, future work
    Just (SuspEx e s) -> simplExpr s ins e cont
    Just (DoneEx e) -> simplExpr M.empty ins e cont

-- \| Simplify Let Expressions
simplExpr sub ins (I.Let binders body t) cont = do
  simplified <- mapM simplBinder binders
  let (simplBinders, subs) = unzip simplified
  let binders' = Ma.catMaybes simplBinders
  let subs' = foldr1 (<>) subs
  body' <- simplExpr subs' ins body cont
  if null binders' then pure body' else pure (I.Let binders' body' t)
 where
  simplBinder
    :: (I.Binder t, I.Expr I.Type)
    -> SimplFn (Maybe (I.Binder t, I.Expr I.Type), Subst)
  simplBinder (binder, rhs) = do
    m <- gets occInfo
    case binder of
      (I.BindVar v _) -> case M.lookup v m of
        (Just Dead) -> pure (Nothing, sub) -- get rid of this dead binding
        (Just OnceSafe) -> do
          -- preinline test PASSES
          -- bind x to E singleton :: k -> a -> Map k a
          insertSubst v $ SuspEx rhs M.empty
          let sub' = M.singleton v (SuspEx rhs sub)
          pure (Nothing, sub')
        _ -> do
          -- preinline test FAILS, so do post inline unconditionally
          e' <- simplExpr sub ins rhs cont -- process the RHS
          case e' of
            -- x goes here.
            (I.Lit _ _) -> do
              insertSubst v $ DoneEx e'
              pure (Nothing, M.empty) -- PASSES postinline
            (I.Var _ _) -> do
              insertSubst v $ DoneEx e'
              pure (Nothing, M.empty) -- PASSES postinline
            _ -> do
              rhs' <- simplExpr sub ins rhs cont -- we won't inline x, but still possible to simplify e (RHS)
              pure (Just (binder, rhs'), sub) -- FAILS postinline; someday callsite inline
      _ -> do
        e' <- simplExpr sub ins rhs cont
        pure (Just (binder, e'), sub) -- can't inline wildcards

-- \| for all other expressions, don't do anything
simplExpr _ _ e _ = pure e


{- | Run occurrence analyser over each top level function

Returns logging info as a string.
-}
runOccAnal :: I.Program I.Type -> SimplFn String
runOccAnal I.Program{I.programDefs = defs} = do
  defs' <- mapM swallowArgs defs
  info <- mapM (getOccInfoForDef . second occAnalExpr) defs'
  return (show info)
 where
  {- Take in a top level function, "swallows" its args, and return its body.

  "Swallow" means to add the argument to our occurrence info state.
  It returns a top level function without curried arguments; just the body.
  -}
  swallowArgs :: (I.Binder t, I.Expr t) -> SimplFn (I.Binder t, I.Expr t)
  swallowArgs (funcName, l@I.Lambda{}) = do
    addOccs $ I.binderToVar funcName
    let (args, body) = unfoldLambda l
    mapM_ (addOccs . I.binderToVar) args
    pure (funcName, body)
   where
    addOccs (Just nm) = addOccVar nm
    addOccs Nothing = pure ()
  swallowArgs (name, e) = pure (name, e)

  -- a hacky way to see the occurence info for each top def
  getOccInfoForDef :: (I.Binder I.Type, SimplFn (I.Expr I.Type, String)) -> SimplFn String
  getOccInfoForDef (v, tpl) = do
    (_, occinfo) <- tpl
    pure $
      "START topdef "
        ++ show v
        ++ " has OccInfo: "
        ++ show occinfo
        ++ " END"


{- | Run the Ocurrence Analyzer over an IR expression node

How do we handle each node?
- Var VarId t                           DONE
- Data DConId t                         Default case (TODO: trivial constructor argument invariant)
- Lit Literal t                         Default case
- App (Expr t) (Expr t) t               DONE
- Let [(Binder, Expr t)] (Expr t) t     DONE
- Lambda Binder (Expr t) t              DONE
- Match (Expr t) [(Alt, Expr t)] t      DONE (TODO: analyze patterns / LHS of arms?)
- Prim Primitive [Expr t] t             DONE
-}
occAnalExpr :: I.Expr I.Type -> SimplFn (I.Expr I.Type, String)


-- | Occurrence Analysis over Let Expression
occAnalExpr l@(I.Let nameValPairs body _) = do
  mapM_
    ( \(binder, rhs) -> do
        _ <- occAnalExpr rhs
        case I.binderToVar binder of
          (Just nm) -> addOccVar nm
          _ -> pure ()
    )
    nameValPairs
  _ <- occAnalExpr body
  m <- gets occInfo
  pure (l, show m)

-- \| Occurrence Analysis over Variable Expression
occAnalExpr var@(I.Var v _) = do
  updateOccVar v
  m <- gets occInfo
  return (var, show m)

-- \| Occurrence Analysis over Lambda Expression
occAnalExpr l@(I.Lambda (I.BindVar v _) b _) = do
  recordEnteringLambda
  addOccVar v
  _ <- occAnalExpr b
  recordExitingLambda
  m <- gets occInfo
  pure (l, show m)
occAnalExpr l@(I.Lambda _ b _) = do
  recordEnteringLambda
  _ <- occAnalExpr b
  recordExitingLambda
  m <- gets occInfo
  pure (l, show m)

-- \| Occurrence Analysis over Application Expression
occAnalExpr a@(I.App lhs rhs _) = do
  _ <- occAnalExpr lhs
  _ <- occAnalExpr rhs
  m <- gets occInfo
  pure (a, show m)

-- \| Occurrence Analysis over Primitve Expression
occAnalExpr p@(I.Prim _ args _) = do
  mapM_ occAnalExpr args
  m <- gets occInfo
  pure (p, show m)

-- \| Occurrence Analysis over Match Expression
occAnalExpr p@(I.Match scrutinee arms _) = do
  recordEnteringMatch
  _ <- occAnalExpr scrutinee
  -- let (alts, rhss) = unzip arms
  mapM_ (occAnalAlt . fst) arms
  mapM_ (occAnalExpr . snd) arms
  recordExitingMatch
  m <- gets occInfo
  pure (p, show m)

-- \| for all other expressions, don't do anything
occAnalExpr e = do
  m <- gets occInfo
  pure (e, show m)


-- | Run Ocurrence Analyser Over a Match Arm
occAnalAlt :: I.Alt t -> SimplFn (I.Alt t, String)
occAnalAlt alt@(I.AltBinder binder) = do
  case I.binderToVar binder of
    (Just nm) -> addOccVar nm
    _ -> pure ()
  m <- gets occInfo
  pure (alt, show m)
occAnalAlt alt@(I.AltData _ alts _) = do
  mapM_ occAnalAlt alts
  m <- gets occInfo
  pure (alt, show m)
occAnalAlt lit = do
  m <- gets occInfo
  pure (lit, show m)
