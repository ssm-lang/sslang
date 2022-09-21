{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}

{-| Description : Insert reference counting primitives

This inserts @dup@ and @drop@ primitives according to a caller @dup@,
callee @drop@ policy.  A function returning a value should @dup@ that value.

The @dup : a -> a@ primitive behaves like the identity function,
evaluating and returning its first argument and increasing the
reference count on the result.  It is meant to be wrapped around
function arguments.

The @drop : a -> b -> a@ primitive evaluates and returns its first
argument.  It decrements the reference count to its second argument
after it has evaluated its first argument.  It is meant to be wrapped
around function bodies that need to use and then de-reference their
arguments.

Thus, something like

> add a b = a + b

becomes

@
add a b =
  drop
    (drop
       ((dup a) + (dup b))
       b)
    a
@

Arguments @a@ and @b@ to the @+@ primitive are duplicated and the
result of @+@ is duplicated internally, so @add@ does not need to
duplicate its result.  Both arguments @a@ and @b@ are dropped.

Try running @sslc --dump-ir-final@ on an example to see the inserted
@dup@ and @drop@ constructs.

Our approach was inspired by Perceus
<https://www.microsoft.com/en-us/research/publication/perceus-garbage-free-reference-counting-with-reuse/>

-}
module IR.InsertRefCounting where

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
import qualified IR.Types                      as I


-- * The external interface

-- $external

-- | Insert dup and drop primitives throughout a program
--
-- Applies `insertTop` to the program's definitions
insertRefCounting :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
insertRefCounting program = (`evalStateT` 0) $ do
  let programDefs = I.programDefs program
  defs <- mapM insertTop programDefs
  return $ program { I.programDefs  = defs
                   , I.programEntry = I.programEntry program
                   , I.typeDefs     = I.typeDefs program
                   }


-- * Module internals, not intended for use outside this module
--
-- $internal


-- | Monad for creating fresh variables: add an Int to the pass
type Fresh = StateT Int Compiler.Pass

-- | Create a fresh variable name with the given suffix
--
-- Creates a new variable ID of the form "anon1," "anon2,"
-- etc. followed by the supplied suffix
getFresh :: String -> Fresh I.VarId
getFresh str = do
  curCount <- get
  modify (+ 1)
  return $ fromString $ ("anon" <> show curCount) ++ str

-- | Make a dup primitive that returns the type of its argument
makeDup :: I.Expr I.Type -- ^ The variable to duplicate and return
        -> I.Expr I.Type -- ^ The @dup@ call
makeDup e = I.Prim I.Dup [e] $ I.extract e

-- | Make a drop primitive with unit type
makeDrop :: I.Expr I.Type -- ^ The expression to evaluate and return
         -> I.Expr I.Type -- ^ The variable to drop afterwards
         -> I.Expr I.Type -- ^ The @drop@ call
makeDrop r e = I.Prim I.Drop [e, r] I.Unit



-- $internal

-- | Insert referencing counting for top-level expressions
--
-- Applies `insertExpr` to a top-level delcaration
insertTop :: (I.VarId, I.Expr I.Type) -> Fresh (I.VarId, I.Expr I.Type)
insertTop (var, expr) = (var, ) <$> insertExpr expr

{- | Insert reference counting into an expression

This is the main workhorse of this module.

* __Literals__ are unchanged, e.g., @42@ -> @42@

* __Data constructors__ are unchanged, e.g., @True@ -> @True@ because they
  are functions whose results are referenced
  
* A __variable reference__ becomes a call to dup because it introduces
  another reference to the named object,

> v

becomes

> dup v

* __Application__ inserts dups and drops on both the function being
  applied and its argument, e.g., @foo x@ -> @(dup foo) (dup x)@

* __Primitive__ function application inserts dups and drops on its
  arguments, e.g.,

> (+) x y

becomes

> (+) (dup x) (dup y)

* __Let__ introduces new names whose values are dropped after the body
  is evaluated; @let _ =@ are given names so they can be dropped.


* Nested __Lambda__ expressions are handled by collecting them into a
  single expression with multiple arguments, adding dups and drops to the body,
  and adding drops around the body for each argument.

> add a b = a + b

desugars to

> add = fun a (fun b (a + b))

becomes

@
add = fun a (
        fun b (
           drop (
              drop (
                (dup a) + (dup b)
              ) b
           ) a
@

* __Matches__ that operate on a variable are modified by inserting
 dups and drops into the arms (but not the scrutinee)

* __Matches__ that scrutinize an expression lift the scrutinee
  into a @let@ then insert dups and drops on the whole thing

@
  match e
    alt1 =
    alt2 =
@

becomes

@
  let anon42_scrutinee = e
  match s
    alt1 =
    alt2 =
@

-}

--
insertExpr :: I.Expr I.Type -> Fresh (I.Expr I.Type)

insertExpr dcon@I.Data{}     = return dcon

insertExpr lit@I.Lit{}       = return lit

insertExpr var@I.Var{}       = return $ makeDup var

insertExpr (I.App f x t) = I.App <$> insertExpr f <*> insertExpr x <*> pure t

insertExpr (I.Prim p es typ) = I.Prim p <$> mapM insertExpr es <*> pure typ

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
  let (args    , body) = I.unfoldLambda lam
      (argTypes, _   ) = I.unfoldArrow typ
  args' <- forM args $ maybe (getFresh "_arg") return -- handle _ arguments
  let typedArgs = zipWith (\a b -> (Just a, b)) args' argTypes
      argVars   = zipWith I.Var args' argTypes
  body' <- insertExpr body
  return $ I.foldLambda typedArgs $ foldr makeDrop body' argVars

insertExpr (I.Match v@I.Var{} alts typ) = do
  alts' <- forM alts insertAlt
  return $ I.Match v alts' typ
  
insertExpr (I.Match scrutExpr alts typ) = do
  scrutVar <- getFresh "_scrutinee"
  insertExpr $ I.Let [(Just scrutVar, scrutExpr)]
                     (I.Match (I.Var scrutVar $ I.extract scrutExpr) alts typ)
                     typ

-- | Insert ref counting into pattern match arms.
insertAlt :: (I.Alt, I.Expr I.Type) -> Fresh (I.Alt, I.Expr I.Type)
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
    where varExpr = I.Var v I.Unit -- FIXME: how do I get its type?
