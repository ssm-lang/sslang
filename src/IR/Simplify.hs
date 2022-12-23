{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use record patterns" #-}
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
-- import           Control.Monad                  ( forM_ )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.State.Lazy       ( MonadState
                                                , StateT(..)
                                                , evalStateT
                                                , gets
                                                , modify
                                                -- , unless
                                                )

import           Data.Bifunctor                 ( second )
-- import           Data.List                      ( intersperse )
import qualified Data.Map                      as M
import qualified Data.Maybe                    as Ma
import           IR.IR                          ( unfoldLambda )
-- import qualified GHC.IO.Exception              as Compiler
-- import           Data.Maybe                     ( catMaybes )
-- import qualified Data.Set                      as S

{-  | Occurrence Information for each binding

Dead: Does not appear at all
OnceSafe: Appears once, NOT inside a lambda
MultiSafe: The binder occurs at most ONCE in each of several distinct case branches; 
           NONE of these ocurrences is inside a lambda
OnceUnsafe: Binder occurs exactly once, but inside a lambda. 
Multisafe: Binder may occur many times, including inside lambdas. 
           Variables exported from the module are also makred MultiUnsafe.
LoopBreaker: Chosen to break dependency between mutually recursive defintions. 
Never: Never inline; we use this to develop our inliner incrementally.
-}

type InVar = I.VarId
type OutVar = I.VarId
type InExpr = I.Expr I.Type
type OutExpr = I.Expr I.Type
type InScopeSet = String
type Context = String

type Subst = M.Map InVar SubstRng
data SubstRng = DoneEx OutExpr | SuspEx InExpr Subst

data OccInfo = Dead
             | LoopBreaker -- TBD
             | OnceSafe
             | MultiSafe
             | OnceUnsafe
             | MultiUnsafe
             | Never
  deriving Show

-- | Simplifier Environment
data SimplEnv = SimplEnv
  { occInfo     :: M.Map I.VarId OccInfo
  {- ^ 'occInfo' maps an identifier to its occurence category -}
  , runs        :: Int
  {- ^ 'runs' stores how many times the simplifier has run so far -}
  , countLambda :: Int
  {- ^ 'countLambda' how many lambdas the occurence analyzer is inside -}
  }
  deriving Show

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
runSimplFn (SimplFn m) =
  evalStateT m SimplEnv { occInfo = M.empty, runs = 0, countLambda = 0 }

-- | Update occInfo for the binder since we just spotted it
updateOccVar :: I.VarId -> SimplFn ()
updateOccVar binder = do
  m      <- gets occInfo
  inside <- insideLambda
  let m' = case M.lookup binder m of
        Nothing ->
          error
            (  "UDPATE: We should already know about this binder "
            ++ show m
            ++ "!"
            )
        Just Dead -> do
          -- we only handle OnceSafe currently
          -- if we're inside a lambda, binder is NOT OnceSafe (in fact, it's OnceUnsafe...)
          if inside then M.insert binder Never m else M.insert binder OnceSafe m
        _ -> M.insert binder Never m
  modify $ \st -> st { occInfo = m' }

-- | Add a binder to occInfo with category Dead by default
addOccVar :: I.VarId -> SimplFn ()
addOccVar binder = do
  m <- gets occInfo
  let m' = case M.lookup binder m of
        Nothing -> M.insert binder Dead m
        _ ->
          error
            (  "ADD: Should never have seen this binder "
            ++ show binder
            ++ " before!"
            )
  modify $ \st -> st { occInfo = m' }

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

{- | Entry-point to Simplifer.

Maps over top level definitions to create a new simplified Program
Do we ever want to inline top-level definitions?
-}
simplifyProgram :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
simplifyProgram p = runSimplFn $ do -- everything in do expression will know about simplifier environment
                                    -- run this do expression and give it the knowledge of my simplifier environment
  -- run the occurrence analyzer
  (_, info) <- runOccAnal p
  -- fail and print out the results of the occurence analyzer
  -- _ <- Compiler.unexpected $ show info
  -- I should always have at least one top-level func, main.
  -- let's see how many top level funcs I have!

  let defs = I.programDefs p
  simplifiedProgramDefs <- mapM simplTop defs
  return $ p { I.programDefs = simplifiedProgramDefs } -- this whole do expression returns a Compiler.Pass

{- | Check if defintion is a top-level function

Every top level defintion is either
1) a top level function of the form (VarId, Lambda v b t)
2) a global variable of the form (VarId, some kind of expr that isn't a Lambda)
-}
-- topLevelFunc :: (I.VarId, I.Expr I.Type) -> Maybe I.VarId -- get rid of me someday
-- topLevelFunc (v, I.Lambda _ _ _) = Just v 
-- topLevelFunc _                   = Nothing


-- | Simplify a top-level definition
simplTop :: (I.VarId, I.Expr I.Type) -> SimplFn (I.VarId, I.Expr I.Type)
simplTop (v, e) = do
  (,) v <$> simplExpr M.empty "inscopeset" e "context"

{- | Recursively simplify IR expressions.
Probably want more documentation here eventually.

WIP WIP WIP WIP WIPPPPPPPPPPPP
simpleExpr :: Subst -> InScopeSet
           -> InExpr -> Context
           -> OutExpr

-}

simplExpr :: Subst -> InScopeSet -> InExpr -> Context -> SimplFn OutExpr
simplExpr sub ins (I.Lambda binder body t) cont = do
  body' <- simplExpr sub ins body cont
  return (I.Lambda binder body' t)

{- 
p: q + 1
sub: {Map q (SuspEx 5 {})}

lookup args in the substitution

-}
simplExpr sub ins p@(I.Prim prim args t) cont = do
--  let test = mapM (simplExpr sub ins) args cont 
  -- [SimplFn OutExpt] --> SimplFn [OutExpr]
  -- simplified <- mapM (simplExpr sub ins) args cont
  -- sequence test
  args' <- sequence $ mapM (simplExpr sub ins) args cont 
  pure (I.Prim prim args' t)
  -- where
   -- f :: [SimplFn OutExpr] -> SimplFn [OutExpr]
    


simplExpr sub ins var@(I.Var v _) cont = case M.lookup v sub of
  Nothing           -> pure var -- callsite inline, future work
  Just (SuspEx e s) -> simplExpr s ins e cont
  Just (DoneEx e  ) -> simplExpr M.empty ins e cont

simplExpr sub ins (I.Let binders body t) cont = do
  simplified <- mapM simplBinder binders
  let (simplBinders, subs) = unzip simplified
  let binders'             = Ma.catMaybes simplBinders
  let subs'                = foldr1 (<>) subs
  body' <- simplExpr subs' ins body cont
  -- if binders is all Nothing, then get rid of this Let statement
  if null binders' then pure body'
  else pure (I.Let binders' body' t)

  -- level 1: let q = 5
   -- simplBinders =  [Nothing]
   -- subs =  [Map q (SuspEx 5 {}))]

  -- level 2: let r = q + 1
  -- simplBinders =  [Nothing]
  -- subs = [Map r (SuspEx q+1 {Map q (SuspEx 5 {})}))]

  -- level 3: r , with substitution map [Map r (SuspEx q+1 {Map q (SuspEx 5 {})}))]
  -- We call simplExpr on r, which is a varId
  -- -> matches on Just (SuspEx q+1 {Map q (SuspEx 5 {})}) -> simplExpr <same thing> q+1 cont
  --    -> no pattern match for q+1 so go to catch all 

  -- recurse on body
  -- return a Let containing our new binders and the simplified body??

  -- let f = (\(binder, rhs) -> case binder of
  --            (Just v) -> case M.lookup v m of
  --             Just Dead -> 
  --         )
 -- mapM_ 
--  type Subst = M.Map InVar SubstRng
-- data SubstRng = DoneEx OutExpr | SuspEx InExpr Subst

  --pure l
 where
    {-
    let x = 5
        y = 7
        z = x + 4
        in 
          x
    
    
    -}
    --filterM :: Applicative m => (a -> m Bool) -> [a] -> m [a]
    -- where the initial value of the accumulator is ([],sub)
    -- f :: (I.Binder, I.Expr I.Type) -> SimplFn (Maybe (I.Binder, I.Expr I.Type), Subst)
    -- **************************
    -- let q = 5
    -- binders: [(Nothing, Map q (SuspEx 5 {}))]

    -- *******
    -- let r = q + 1 in r
    -- binders: [(Nothing, Map r (SuspEx q+1 {Map q (SuspEx 5 {})}))]
  simplBinder
    :: (I.Binder, I.Expr I.Type)
    -> SimplFn (Maybe (I.Binder, I.Expr I.Type), Subst)
  simplBinder (binder, rhs) = do
    m <- gets occInfo
    case binder of
      (Just v) -> case M.lookup v m of
        (Just Dead    ) -> pure (Nothing, M.empty)  -- get rid of this binding
        (Just OnceSafe) -> do -- preinline test PASSES
          -- bind x to E singleton :: k -> a -> Map k a
          let sub' = M.singleton v (SuspEx rhs sub)
          pure (Nothing, sub')
        _ -> do -- preinline test FAILS, so do post inline unconditionally
          e' <- simplExpr sub ins rhs cont -- process the RHS
          case e' of
            (I.Lit _ _) -> pure (Nothing, M.singleton v (DoneEx e')) -- PASSES postinline
            (I.Var _ _) -> pure (Nothing, M.singleton v (DoneEx e'))  -- PASSES postinline
            _           -> pure (Just (binder, rhs), sub) -- FAIL postinline; someday callsite inline
      _ -> pure (Nothing, M.empty) -- can't inline wildcards

-- catch all
--simplExpr _ _ e _ = pure (I.Var (I.VarId "catch all simplExpr") (I.extract e))
simplExpr _ _ e _ = pure e 


{-
--   m <- gets occInfo
--   mapM_
--     (\(binder, rhs) -> case binder of
--       (Just something) -> case M.lookup something m of
--         Just Dead -> get rid of this binding
--         -- inline x unconditionally in B
--         -- -> binds x to E substitution, discard binding completely, simplifies B using this extended substitution
--         Just OnceSafe -> get rid of binding, add substitution and that's it
--         _ ->  do -- post inline unconditionallly
              E' <- simplExpr rhs 
              if E' == literal/value:
                bind x to E' substitution, discard binding completely 
              else
                do nothing
--       Nothing -> pure ()
--     )
--     binders
If post inline unconditionally fails, we don't do anything. 
Someday, we will add the binder to the inscope instead of the substitution
-- let expressions
occAnalExpr l@(I.Let binders body _) = do
  mapM_
    (\(binder, rhs) -> case binder of
      (Just nm) -> do
        addOccVar nm
        _ <- occAnalExpr rhs -- in case there is a let in the RHS
        pure ()
      Nothing -> pure ()
    )
    binders
  _ <- occAnalExpr body
  m <- gets occInfo
  pure (l, show m)

-}
-- simplExpr _ _ e _ = pure e -- catch all case; delete later
-- simplExpr l@(I.Let binders body _) = do
--   m <- gets occInfo
--   mapM_
--     (\(binder, rhs) -> case binder of
--       (Just something) -> case M.lookup something m of
--         Just Dead -> pure ()
--         -- inline x unconditionally in B
--         -- -> binds x to E in current substitution, discard binding completely, simplifies B using this extended substitution
--         Just OnceSafe -> pure ()
--         _ -> pure ()
--       Nothing -> pure ()
--     )
--     binders
--   -- _ :: (Maybe VarId, Expr Type) -> SimplFn ()
-- --  mapM_
-- --    ( \(binder, rhs) -> case binder of
-- --        Just something -> pure (binder, rhs)
-- --    )
--     -- (\(binder, rhs) -> case M.lookup binder m of 
--       -- Just Dead -> pure ()
--       -- Just OnceSafe -> pure rhs
--       -- _ -> pure (binder, rhs)
--     -- )
-- --  binders
--   pure l

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

{-
main cin cout = // has type int -> int -> int
let x = +5
IN
let y = x 4
IN
y

FYI: Lambda Binder (Expr t) t
FYI: Binder is a (Maybe VarId)
Let [(Binder, Expr t)] (Expr t) t
5 + 5 ==> App (App "Add" 5) 5
+5 ==> (App "Add" 5)

'foldApp' is the inverse of 'unfoldApp'.
foldApp :: Expr t -> [(Expr t, t)] -> Expr t
foldApp = foldr $ \(a, t) f -> App f a t

("main", Lambda cin -- FoldApp
  (Lambda cout 
    Lambda something something
    (Let [(x, (App "+" 5))] (Let [(y, App "x" 4)] (y)))
  )
)

("main", (Let [(x, (App "+" 5))] (Let [(y, App "x" 4)] (y)))) )
where in our occurence info state monad thing, we have
(cin, DEAD)
(cout, DEAD) 

main cin cout = // has type int -> int -> (int -> int)
\z -> 
let z' = z + 5 + 2 (\j -> j * 2)

result of unfolding this will be: main [cin, cout, z] with type int -> int -> int -> int
Does a top a top level function with two args that returns a lambda have the SAME TYPE
as a top level function with three arguments?


-}

{-
1) {x: Dead }
2) {x: Dead, y: Dead}
3) {}
-}

--unfoldLambda :: Expr t -> ([Binder], Expr t)

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

--                  this is body> 
--FYI: Lambda Binder (Expr t) t
--FYI: Binder is a (Maybe VarId)
-- 1)  _ <- blah -- compiler knows you want to throw away your result
-- 2)    blah -- get warning

{- | Take in a top level function, and "swallows" its arguments

"Swallow" means to add the argument to our occurrence info state.
It returns a top level function without curried arguments; just the body.
unfoldLambda :: Expr t -> ([Binder], Expr t)
-}
swallowArgs :: (I.VarId, I.Expr t) -> SimplFn (I.VarId, I.Expr t)
swallowArgs (funcName, l@(I.Lambda _ _ _)) = do
  let (args, body) = unfoldLambda l
  mapM_ addOccs args
  pure (funcName, body)
 where
  addOccs (Just nm) = addOccVar nm
  addOccs Nothing   = pure ()

swallowArgs (name, e) = pure (name, e)


runOccAnal :: I.Program I.Type -> SimplFn (I.Program I.Type, String)
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
  return (p, show x)



-- | Run the Ocurrence Analyzer over an IR expression node
occAnalExpr :: I.Expr I.Type -> SimplFn (I.Expr I.Type, String)

-- let expressions
occAnalExpr l@(I.Let binders body _) = do
  mapM_
    (\(binder, rhs) -> case binder of
      (Just nm) -> do
        addOccVar nm
        _ <- occAnalExpr rhs -- in case there is a let in the RHS
        pure ()
      Nothing -> pure ()
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

occAnalExpr p@(I.Prim _ args _) = do
  mapM_ occAnalExpr args
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
  = Var VarId t
  {- ^ @Var n t@ is a variable named @n@ of type @t@. -}
  | Data DConId t
  {- ^ @Data d t@ is a data constructor named @d@ of type @t@. -}
  | Lit Literal t
  {- ^ @Lit l t@ is a literal value @l@ of type @t@. -}
  | App (Expr t) (Expr t) t
  {- ^ @App f a t@ applies function @f@ to argument @a@, producing a value of
  type @t@.
  -}
  | Let [(Binder, Expr t)] (Expr t) t
  {- ^ @Let [(n, v)] b t@ binds value @v@ to variable @v@ in its body @b@.

  The bindings list may only be of length greater than 1 for a set of mutually
  co-recursive functions.
  -}
  | Lambda Binder (Expr t) t
  {- ^ @Lambda v b t@ constructs an anonymous function of type @t@ that binds
  a value to parameter @v@ in its body @b@.
  -}
  | Match (Expr t) [(Alt, Expr t)] t
  {- ^ @Match s alts t@ pattern-matches on scrutinee @s@ against alternatives
  @alts@, each producing a value of type @t@.
  -}
  | Prim Primitive [Expr t] t
  {- ^ @Prim p es t@ applies primitive @p@ arguments @es@, producing a value
  of type @t@.
  -}
  -}

-- --occAnalExpr (I.Var varId) = -- first time you see it, dead -> oncesafe



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
