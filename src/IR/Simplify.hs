{-# LANGUAGE DerivingVia #-}

{- | Simple Inlining Optimization Pass

Performs preinline and postinline unconditionally.
TODO: Mutual recursion in let bindings
TODO: Callsite Inline
-}
module IR.Simplify (
  simplifyProgram,
) where

import qualified Common.Compiler as Compiler
{-foldLet,-}

import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.State.Lazy (
  MonadState (put),
  State,
  StateT (..),
  evalStateT,
  execState,
  gets,
  modify,
 )
import Data.Bifunctor (second)
import Data.Generics (Typeable, everywhere, everywhereM, mkM, mkT)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Maybe as Ma
import IR.IR (extract, unfoldApp, unfoldLambda, foldLet)
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
ConstructorFuncArg: We encountered a duplicate binder, which means it must be
          from an auto-generated constructor function argument.
-}
data OccInfo
  = Dead
  | LoopBreaker -- TODO: mutual recursion
  | OnceSafe
  | MultiSafe
  | OnceUnsafe
  | MultiUnsafe
  | Never
  | ConstructorFuncArg
  deriving (Show)
  deriving (Typeable)


-- | Simplifier Environment
data SimplEnv = SimplEnv
  { -- | 'occInfo' maps an identifier to its occurence category
    occInfo :: M.Map I.VarId OccInfo
  , -- | 'subst' maps an identifier to its substitution
    subst :: M.Map InVar SubstRng
  , -- | 'runs' stores how many times the simplifier has run so far
    runs :: Int
  , -- | 'countLambda' how many lambdas the occurence analyzer is inside
    countLambda :: Int
  , -- | 'mutualRecursion' whether the occurence analyzer encountered mutual recursion
    mutualRecursion :: Bool
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
      , subst = M.empty
      , mutualRecursion = False
      }


-- | Add a binder to occInfo with category Dead by default
addOccVar :: I.VarId -> SimplFn ()
addOccVar binder = do
  m <- gets occInfo
  let m' = case M.lookup binder m of
        Nothing -> M.insert binder Dead m
        {- since we have name mangling, the only time we will add
           a binder that is already present in occInfo is when we
           encounter a constructor function argument. -}
        Just _ -> M.insert binder ConstructorFuncArg m
  modify $ \st -> st{occInfo = m'}


-- | Update occInfo for the binder since we just spotted it
updateOccVar :: I.VarId -> SimplFn ()
updateOccVar binder = do
  m <- gets occInfo
  insidel <- insideLambda
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
          if insidel
            then M.insert binder Never m
            else M.insert binder OnceSafe m
        Just _ -> M.insert binder ConstructorFuncArg m
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


-- | Record that mutual recursion was encountered
recordMutualRecursion :: SimplFn ()
recordMutualRecursion = modify $ \st -> st{mutualRecursion = True}


-- | Returns whether ocurrence analyzer detected mutualRecursion
foundMutualRecursion :: SimplFn Bool
foundMutualRecursion = gets mutualRecursion


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


{- | Entry-point to Simplifer.

Maps over top level definitions to create a new simplified Program
-}
simplifyProgram :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
simplifyProgram p = runSimplFn $ do
  _ <- runOccAnal p -- run the occurrence analyzer
  -- fail and print out the results of the occurence analyzer
  -- info <- runOccAnal p
  -- _ <- Compiler.unexpected $ show info
  invalidResults <- foundMutualRecursion -- TODO: handle mutual recursion
  if invalidResults
    then pure p
    else do
      simplifiedProgramDefs <- mapM simplTop (I.programDefs p)
      let fewerMatches = everywhere (mkT elimMatchArms) simplifiedProgramDefs
      return $ p{I.programDefs = fewerMatches} -- this whole do expression returns a Compiler.Pass


-- | Simplify a top-level definition
simplTop :: (I.Binder I.Type, I.Expr I.Type) -> SimplFn (I.Binder I.Type, I.Expr I.Type)
simplTop (v, e) = do
  (,) v <$> simplExpr M.empty "inscopeset" e "context"


{- | Simplify an IR expression.

Binders marked Oncesafe are inlined provided their RHS is pure.
In all other cases, the RHS of the binder is simplified.
If the simplified RHS is a variable or literal, the binder is inlined.
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
  simplBinder ::
    (I.Binder t, I.Expr I.Type) ->
    SimplFn (Maybe (I.Binder t, I.Expr I.Type), Subst)
  simplBinder (binder, rhs) = do
    m <- gets occInfo
    case binder of
      (I.BindVar v _) -> case M.lookup v m of
        (Just Never) -> do
          e' <- simplExpr sub ins rhs cont
          pure (Just (binder, e'), sub) -- never inline something marked never, but still possible to simplify RHS
        (Just ConstructorFuncArg) -> do
          e' <- simplExpr sub ins rhs cont
          pure (Just (binder, e'), sub) -- never inline something marked ConstructorFuncArg, but still possible to simplify RHS
        (Just Dead) -> pure (Nothing, sub) -- get rid of this dead binding
        (Just OnceSafe) -> do
          if checkPure rhs
            then do
              insertSubst v $ SuspEx rhs M.empty
              let sub' = M.singleton v (SuspEx rhs sub)
              pure (Nothing, sub')
            else do
              e' <- simplExpr sub ins rhs cont
              pure (Just (binder, e'), sub)
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
              pure (Just (binder, e'), sub) -- FAILS postinline; someday callsite inline
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
  -- swallowArgs :: (I.VarId, I.Expr t) -> SimplFn (I.VarId, I.Expr t)
  -- swallowArgs (funcName, l@I.Lambda{}) = do
  --   addOccs (Just funcName) -- HACKY
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


-- | Run the Ocurrence Analyzer over an IR expression node
occAnalExpr :: I.Expr I.Type -> SimplFn (I.Expr I.Type, String)
-- \| Occurrence Analysis over Let Expression
occAnalExpr l@(I.Let nameValPairs body _) = do
  mapM_
    ( \(binder, rhs) -> do
        case I.binderToVar binder of
          (Just nm) -> do
            addOccVar nm -- add name before processing RHS; allows for local recursive funcs
            occAnalExpr rhs
          _ -> occAnalExpr rhs
    )
    nameValPairs
  _ <- occAnalExpr body
  -- if the let has more than one binder, record we've encountered mutual recursion
  when (length nameValPairs > 1) recordMutualRecursion
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
  _ <- occAnalExpr scrutinee
  mapM_ (occAnalAlt . fst) arms
  mapM_ (occAnalExpr . snd) arms
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


type IsPure = Bool
type CheckPure = State IsPure


{- | checkPure predicate to account for side effects when inlining

Return true when IR node is pure, false otherwise.
- Any operation on a reference type is considered impure
- Function Application is considered impure (for now)
- Variables are pure
- Literals are pure
- PrimOps with only pure arguments are pure
- Data Constructors with only pure arguments are pure
-}
checkPure :: I.Expr I.Type -> IsPure
checkPure e = runComp $ do
  everywhereM (mkM isPure) e
 where
  runComp :: CheckPure a -> IsPure
  runComp a = execState a True


-- | recursively determines if an IR node is pure
isPure :: I.Expr I.Type -> CheckPure (I.Expr I.Type)
isPure e@I.App{} =
  case unfoldApp e of
    (I.Data _ _, _) -> pure e
    (I.Lambda{}, _) -> pure e
    _ -> setNotPure e -- functions
isPure e@(I.Prim I.New _ _) = setNotPure e
isPure e@(I.Prim I.Dup _ _) = setNotPure e
isPure e@(I.Prim I.Drop _ _) = setNotPure e
isPure e@(I.Prim I.Deref _ _) = setNotPure e
isPure e@(I.Prim I.Assign _ _) = setNotPure e
isPure e@(I.Prim I.After _ _) = setNotPure e
isPure e@(I.Prim I.Par _ _) = setNotPure e
isPure e@(I.Prim I.Wait _ _) = setNotPure e
isPure e@(I.Prim I.Loop _ _) = setNotPure e
isPure e@(I.Prim I.Break _ _) = setNotPure e
isPure e@(I.Prim I.Now _ _) = setNotPure e
isPure e@(I.Prim (I.CQuote _) _ _) = setNotPure e
isPure e@(I.Prim (I.CCall _) _ _) = setNotPure e
isPure e@(I.Prim (I.FfiCall _) _ _) = setNotPure e
isPure e = pure e


-- | helper function for isPure
setNotPure :: I.Expr I.Type -> CheckPure (I.Expr I.Type)
setNotPure e = do put False; return e


-- | Reduce a match expression to a let expression (or just the RHS of an arm) if possible
elimMatchArms :: I.Expr I.Type -> I.Expr I.Type
elimMatchArms e@(I.Match scrut@I.App{} arms _) =
  let unfoldedScrut = unfoldApp scrut
      filtered = filter (checkForDConMatch unfoldedScrut . fst) arms
   in if null filtered
        then e -- we hit this case when scrut is a function application
        else case head filtered of -- otherwise, scrut is a data constructor application
          (I.AltData _ patargs _, rhs) ->
            let letBinders = mapMaybe isMaybeLetBinder $ zip patargs dconargs
                dconargs = fst <$> snd unfoldedScrut
                isMaybeLetBinder :: (I.Alt I.Type, I.Expr I.Type) -> Maybe (I.Binder I.Type, I.Expr I.Type)
                isMaybeLetBinder (I.AltBinder b@(I.BindVar _ _) , rhs') = Just (b, rhs')
                isMaybeLetBinder _ = Nothing
             in if null letBinders
                  then rhs
                  else foldLet letBinders rhs -- turn match arm pattern into nested let expression
          (_, rhs) -> rhs -- match arm pattern is a single variable or wild card
elimMatchArms (I.Match scrut@(I.Lit (I.LitIntegral _) _) arms _) =
  -- wildcard always matches, so the filtered list is always non-empty
  case head $ filter (checkForLitMatch scrut) arms of
    -- if variable, then turn into let
    (I.AltBinder b@(I.BindVar _ _), rhs) -> I.Let [(b, scrut)] rhs (extract rhs)
    -- if literal or catchall, just return the right hand side of the arm
    (_, rhs) -> rhs
elimMatchArms e = e

-- | Check if Literal scrutinee matches given arm
checkForLitMatch :: I.Expr I.Type -> (I.Alt I.Type, I.Expr I.Type) -> Bool
checkForLitMatch (I.Lit (I.LitIntegral v) _) (arm, _) =
  case arm of
    (I.AltLit (I.LitIntegral d) _) -> d == v
    (I.AltBinder _) -> True
    _ -> False
checkForLitMatch _ _ = False -- we should never hit this case

-- | Check if Data Constructor scrutinee matches given arm
checkForDConMatch :: (I.Expr I.Type, [(I.Expr I.Type, I.Type)]) -> I.Alt I.Type -> Bool
checkForDConMatch (I.Data dcon _, args) arm =
  case arm of
    (I.AltData dconPat argPats _) -> 
   {-
    // first check if arm has a matching DCon
     match RGB 1 2 3
       Black = ... 
       CMYK 1 2 3 4 = ...
       _ = ... // picks this arm since no matching DCon
    -}
      dcon == dconPat 
   {-
    // once we have an arm with a matching Dcon, make sure the DCon args match the rest of the arm
      match RGB 1 2 3
       Black = ... 
       CMYK 1 2 3 4 = ...
       RGB 3 3 3 = ... // this arm has matching DCon, but does NOT have matching DCon args
       RGB 1 2 x = ... // pick this arm because both its DCon and DCon arguments match
       _ = ...         // wildcard not picked because earlier arm matched
    -}
      && compareArm argPats (fst <$> args)
    {-
    // a variable or wildward always matches a scrutinee!
     match RGB 1 2 3
       RGB 8 8 8 = ... 
       x = ... // picks this arm
    -}
    (I.AltBinder _) -> True
    _ -> False
 where
  -- \| compare a match arm's argument patterns to the arguments of the scrutinee
  compareArm :: [I.Alt I.Type] -> [I.Expr I.Type] -> Bool
  compareArm [] [] = True
  compareArm (pat@I.AltData{} : tl) (app@I.App{} : tl2) = -- nested patterns
    checkForDConMatch (unfoldApp app) pat && compareArm tl tl2
  compareArm ((I.AltLit v _) : tl) (I.Lit v2 _ : tl2) = v == v2 && compareArm tl tl2
  compareArm ((I.AltBinder _) : tl) (_ : tl2) = compareArm tl tl2
  compareArm _ _ = False
checkForDConMatch _ _ = False