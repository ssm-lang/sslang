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
import Data.Bifunctor (second, bimap)
import Data.Generics (Typeable, everywhereM, mkM, everywhere, mkT)
import qualified Data.Map as M
import qualified Data.Maybe as Ma
import IR.IR (unfoldLambda, extract, unfoldApp, foldLet)
import qualified IR.IR as I
import Data.Maybe(mapMaybe, catMaybes)


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
  , -- | 'countLambda' how many matches the occurence analyzer is inside
    countMatch :: Int
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
        Just _ -> M.insert binder ConstructorFuncArg m
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
  let fewerArms = everywhere (mkT elimMatchArms) simplifiedProgramDefs
  return $ p{I.programDefs = fewerArms} -- this whole do expression returns a Compiler.Pass

--everywhereM (mkM dataToApp) defs
-- Match (Expr t) [(Alt, Expr t)] t
-- Lit Literal t_
-- unfoldApp :: Expr t -> (Expr t, [(Expr t, t)])
-- unfoldApp (App lhs rhs t) =
--   let (fn, args) = unfoldApp lhs in (fn, args ++ [(rhs, t)])
-- unfoldApp e = (e, [])

--  App (Expr t) (Expr t) t
-- | An alternative in a pattern-match.
-- data Alt
--   = AltData DConId [Alt]
--   -- ^ @AltData d vs@ matches data constructor @d@, and recursive patterns @alts@.
--   | AltLit Literal
--   -- ^ @AltLit l@ matches against literal @l@, producing expression @e@.
--   | AltBinder Binder
--   -- ^ @AltBinder v@ matches anything, and bound to name @v@.
--   deriving (Eq, Show, Typeable, Data)
-- type Binder = Maybe VarId
-- check for numbers, then check for vars (Just VarId), then if still checking, we know it's the catch-all.
-- check for catchall . check for var. check for number
{-
match 5 
  5 = 45
  _ = 

match 5 
  4 = 900
  8 = 45
  x = x + 2
  _ = 
-}

-- check if literal scrut matches given arm
checkForLitMatch :: I.Expr I.Type -> (I.Alt, I.Expr I.Type) -> Bool
checkForLitMatch (I.Lit (I.LitIntegral v) _) (arm, _) =
  case arm of
    (I.AltLit (I.LitIntegral d)) -> d == v
    (I.AltBinder _) -> True
    _ -> False
checkForLitMatch _ _ = False -- we should never hit this case

-- (App (App RGB 4) 5)
-- (App (App RGB 4) 5) :: Color
-- (App RGB 4) :: int -> Color
-- (RGB) :: int -> int -> Color
-- (RGB, int->int->Color) [(4 int, int-> Color), (5 int, Color)]
-- check if dcon scrut matches given arm
checkForDConMatch :: (I.Expr I.Type, [(I.Expr I.Type, I.Type)]) -> I.Alt -> Bool
checkForDConMatch (I.Data dcon _, args) arm =
  case arm of
    (I.AltData dconPat argPats) -> dcon == dconPat && compareArm argPats (fst <$> args)
    (I.AltBinder _) -> True
    _ -> False
  where compareArm [] [] = True
        compareArm (pat@(I.AltData _ _):t) (app@I.App {}:t2) =
          checkForDConMatch (unfoldApp app) pat && compareArm t t2
        compareArm ((I.AltLit v ):t) (I.Lit v2 _:t2) = v == v2 && compareArm t t2
        compareArm ((I.AltBinder _):t) (_: t2)= compareArm t t2
        compareArm _ _ = False
checkForDConMatch _ _ = False

{-
match RGB 4 x
  RGB 4 5 = 4
  RGB 4 g = 7
  _ = 9

reduces to 7

match Cons 4 Nil 
  Cons 4 Nil = 4 
  Cons 4 g = 7
  _ = 9

reduces to 7

match Cons 4 Nil 
  Cons 4 (Con 2 Nil) = 4 
  Cons 4 g = 7
  _ = 9

reduces to 7

-}

elimMatchArms :: I.Expr I.Type -> I.Expr I.Type
elimMatchArms e@(I.Match scrut@(I.App e1 e2 _) arms _) =
  let unfoldedScrut = unfoldApp scrut -- the second argument of y is args
      filtered = filter (checkForDConMatch unfoldedScrut . fst) arms
  in
    if null filtered then e -- we hit this case when scrut is a function application
    else
  case head filtered of
    -- Let [(Binder, Expr t)] (Expr t) t
    -- traverse list of constructor stuff and get out binders that we need to turn into lets
    -- so basically, we'll write a helper function that takes an alt and if it is a binder, return True
    -- throw that into filter
    -- list of binders 
    -- zip up pats and args such that
    -- (pat, arg) if pat is a binder, we Just (pat, arg) == Just (binder, expr)
    -- i not, return Nothing
    -- catMaybes :: [Maybe a] -> [a]
    -- mapMaybe :: (a -> Maybe b) -> [a] -> [b]
    (I.AltData _ patargs, rhs) ->
      let result =
            if null letBinders
              then rhs
              else foldLet letBinders rhs 
          letBinders = mapMaybe isMaybeLetBinder zippedUp
          dconargs = fst <$> snd unfoldedScrut
          zippedUp = zip patargs dconargs
          isMaybeLetBinder :: (I.Alt, I.Expr I.Type) -> Maybe (I.Binder, I.Expr I.Type)
          isMaybeLetBinder (I.AltBinder b@(Just _), rhs') = Just (b,rhs')
          isMaybeLetBinder _ = Nothing
      in result

      -- need to convert bindings in arm to let
      -- z = mapMaybe isMaybeLetBinder $ zip (snd x) (snd y)
      -- a@(I.AltData dconPat argPats) = head $ filter (checkForDConMatch y . fst) arms
      {-
        match RGB 1 2
           RGB 1 x = x
           _ = 5
        We have the arm: RGB 1 x = x
        zip up args and pat args
        patArgs = [I.AltLit 1, I.AltBinder x]
        [(I.AltLit 1,1), (I.AltBinder x, 2)]
      -}
      -- map isMaybeLetBinder (zip up stuff)

    (_, rhs) -> rhs
elimMatchArms (I.Match scrut@(I.Lit (I.LitIntegral _) _) arms _) =
  let x = head $ filter (checkForLitMatch scrut) arms in
  case x of
    -- if variable, then turn into let
    (I.AltBinder b@(Just _), rhs) -> I.Let [(b,scrut)] rhs (extract rhs)
    -- if literal or catchall, just return expr
    (_, rhs) -> rhs
--unfoldApp :: Expr t -> (Expr t, [(Expr t, t)])
 {-
 a = 65 //ex 1 (a not getting inlined)
 a = some mutable thing //ex 2
 match RGB a 67 =
  RGB 65 _ = rhs1
  _ = rhs2

rhs2 // ex1
rhs2 // ex2


 match RGB 65 67 =
  RGB 65 x = using RGB x whatever
  RGB f _ = rhs2
  _ = rhs3

  let x = 67 in using RGB x whatever

  if we match on arm and it has variables, turn into let
  if wildcard or literals, just rhs
 -}

 {-
 match 5 =
  x = x + 2 --> (AltBinder (Just x), x + 2)

 let x = 5 in x + 2 --> I.Let [(x, 5)] x + 2
 -}
  -- if literal or catchall, just return expr

elimMatchArms e = e

-- | Simplify a top-level definition
simplTop :: (I.VarId, I.Expr I.Type) -> SimplFn (I.VarId, I.Expr I.Type)
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

-- | Simplify Match Expression
simplExpr sub ins (I.Match scrutinee arms t) cont = do
  scrutinee' <- simplExpr sub ins scrutinee cont
  let (pats, rhss) = unzip arms
  rhss' <- sequence $ mapM (simplExpr sub ins) rhss cont
  let results = zip pats rhss'
  return (I.Match scrutinee' results t)

-- | Simplify Application Expression
simplExpr sub ins (I.App lhs rhs t) cont = do
  lhs' <- simplExpr sub ins lhs cont
  rhs' <- simplExpr sub ins rhs cont
  return (I.App lhs' rhs' t)

-- | Simplify Lambda Expression
simplExpr sub ins (I.Lambda binder body t) cont = do
  body' <- simplExpr sub ins body cont
  pure (I.Lambda binder body' t)

-- | Simplify Variable Expression
simplExpr _ ins var@(I.Var v _) cont = do
  m <- gets subst
  case M.lookup v m of
    Nothing -> pure var -- callsite inline, future work
    Just (SuspEx e s) -> simplExpr s ins e cont
    Just (DoneEx e) -> simplExpr M.empty ins e cont

-- | Simplify Let Expressions
simplExpr sub ins (I.Let binders body t) cont = do
  simplified <- mapM simplBinder binders
  let (simplBinders, subs) = unzip simplified
  let binders' = Ma.catMaybes simplBinders
  let subs' = foldr1 (<>) subs
  body' <- simplExpr subs' ins body cont
  if null binders' then pure body' else pure (I.Let binders' body' t)
 where
  simplBinder ::
    (I.Binder, I.Expr I.Type) ->
    SimplFn (Maybe (I.Binder, I.Expr I.Type), Subst)
  simplBinder (binder, rhs) = do
    m <- gets occInfo
    case binder of
      (Just v) -> case M.lookup v m of
        (Just Never) -> do
          e' <- simplExpr sub ins rhs cont
          pure (Just (binder, e'), sub) -- never inline something marked never, but still possible to simplify RHS
        (Just ConstructorFuncArg) -> do
          e' <- simplExpr sub ins rhs cont
          pure (Just (binder, e'), sub) -- never inline something marked ConstructorFuncArg, but still possible to simplify RHS
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
      Nothing -> do
        e' <- simplExpr sub ins rhs cont
        pure (Just (binder, e'), sub) -- can't inline wildcards, but still possible to simplify e (RHS)

-- | for all other expressions, don't do anything
simplExpr _ _ e _ = pure e


{- | Run occurrence analyser over each top level function

Returns logging info as a string.
-}
runOccAnal :: I.Program I.Type -> SimplFn String
runOccAnal I.Program{I.programDefs = defs} = do
  -- a hacky way to see the occurence info for each top def
  let getOccInfoForDef ::
        (I.VarId, SimplFn (I.Expr I.Type, String)) -> SimplFn String
      getOccInfoForDef (v, tpl) = do
        (_, occinfo) <- tpl
        pure $
          "START topdef "
            ++ show v
            ++ " has OccInfo: "
            ++ show occinfo
            ++ " END"
  defs' <- mapM swallowArgs defs
  info <- mapM (getOccInfoForDef . second occAnalExpr) defs'
  return (show info)
 where
  {- Take in a top level function, "swallows" its args, and return its body.

  "Swallow" means to add the argument to our occurrence info state.
  It returns a top level function without curried arguments; just the body.
  -}
  swallowArgs :: (I.VarId, I.Expr t) -> SimplFn (I.VarId, I.Expr t)
  swallowArgs (funcName, l@(I.Lambda _ _ _)) = do
    addOccs (Just funcName) -- HACKY
    let (args, body) = unfoldLambda l
    mapM_ addOccs args
    pure (funcName, body)
   where
    addOccs (Just nm) = addOccVar nm
    addOccs Nothing = pure ()
  swallowArgs (name, e) = pure (name, e)


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
        case binder of
          (Just nm) -> addOccVar nm
          _ -> pure ()
    )
    nameValPairs
  _ <- occAnalExpr body
  m <- gets occInfo
  pure (l, show m)

-- | Occurrence Analysis over Variable Expression
occAnalExpr var@(I.Var v _) = do
  updateOccVar v
  m <- gets occInfo
  return (var, show m)

-- | Occurrence Analysis over Lambda Expression
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

-- | Occurrence Analysis over Application Expression
occAnalExpr a@(I.App lhs rhs _) = do
  _ <- occAnalExpr lhs
  _ <- occAnalExpr rhs
  m <- gets occInfo
  pure (a, show m)

-- | Occurrence Analysis over Primitve Expression
occAnalExpr p@(I.Prim _ args _) = do
  mapM_ occAnalExpr args
  m <- gets occInfo
  pure (p, show m)

-- | Occurrence Analysis over Match Expression
occAnalExpr p@(I.Match scrutinee arms _) = do
  recordEnteringMatch
  _ <- occAnalExpr scrutinee
  -- let (alts, rhss) = unzip arms
  mapM_ (occAnalAlt . fst) arms
  mapM_ (occAnalExpr . snd) arms
  recordExitingMatch
  m <- gets occInfo
  pure (p, show m)

-- | for all other expressions, don't do anything
occAnalExpr e = do
  m <- gets occInfo
  pure (e, show m)


-- | Run Ocurrence Analyser Over a Match Arm
occAnalAlt :: I.Alt -> SimplFn (I.Alt, String)
occAnalAlt alt@(I.AltBinder binder) = do
  case binder of
    (Just nm) -> addOccVar nm
    _ -> pure ()
  m <- gets occInfo
  pure (alt, show m)
occAnalAlt alt@(I.AltData _ alts) = do
  mapM_ occAnalAlt alts
  m <- gets occInfo
  pure (alt, show m)
occAnalAlt lit = do
  m <- gets occInfo
  pure (lit, show m)
