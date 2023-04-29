{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

{- | Description : Insert reference counting primitives

This inserts @dup@ and @drop@ primitives according to a caller @dup@,
callee @drop@ policy.  The value returned by a function should be
passed back referenced (ownership transfers from the callee back to
the caller).

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
module IR.InsertRefCounting (insertRefCounting) where

import qualified Common.Compiler as Compiler
import Common.Identifiers
import Control.Monad.State.Lazy (
  MonadState (..),
  StateT (..),
  forM,
  modify,
 )
import qualified IR.MangleNames as I
import qualified IR.IR as I
import qualified IR.Types as I
import qualified Data.Map as M


-- * The external interface


-- \$external

{- | Insert dup and drop primitives throughout a program

 Applies `insertTop` to the program's definitions
-}
insertRefCounting :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
insertRefCounting p@I.Program{I.symTable = symTable, I.programDefs = defs} = do
  (defs', symTable') <- runStateT (mapM insertTop defs) symTable
  return $ p{ I.symTable = symTable', I.programDefs = defs' }

-- * Module internals, not intended for use outside this module


--
-- \$internal

-- | Monad for creating fresh variables: add an Int to the pass
type Fresh = StateT (I.SymTable I.Type) Compiler.Pass


{- | Create a fresh variable name with the given name seed.

Uses the symbol table to determine name uniqueness, and keeps it up to date.
-}
getFresh :: String -> I.Type -> Fresh I.VarId
getFresh seed t = do
  symTable <- get
  let str = fromString $ "__dupdrop_anon_" <> seed
      i = I.pickId symTable str
  modify $ M.insert i $ I.SymInfo{ I.symOrigin = str, I.symType = t }
  return i


-- | Make a dup primitive that returns the type of its argument
makeDup
  :: I.Expr I.Type
  -- ^ The variable to duplicate and return
  -> I.Expr I.Type
  -- ^ The @dup@ call
makeDup e = I.Prim I.Dup [e] $ I.extract e


-- | Make a drop primitive with unit type
makeDrop
  :: I.Expr I.Type
  -- ^ The expression to evaluate and return
  -> I.Expr I.Type
  -- ^ The variable to drop afterwards
  -> I.Expr I.Type
  -- ^ The @drop@ call
makeDrop r e = I.Prim I.Drop [e, r] I.Unit


-- \$internal

{- | Insert referencing counting for top-level expressions

 Applies `insertExpr` to a top-level delcaration
-}
insertTop :: (I.Binder I.Type, I.Expr I.Type) -> Fresh (I.Binder I.Type, I.Expr I.Type)
insertTop (var, expr) = (var,) <$> insertExpr expr


{- | Insert reference counting into an expression

This is the main workhorse of this module.

* __Literals__ are unchanged, e.g.,

> 42

remains

> 42

* __Data constructors__ are unchanged, e.g.,

> True

remains

> True

because they are functions whose results are returned with an existing reference

* A __variable reference__ becomes a call to dup because it introduces
  another reference to the named object, e.g.,

> v

becomes

> dup v

* __Application__ recurses (inserts dups and drops) on both the
  function being applied and its argument e.g.,

> add x y

becomes

> (dup add) (dup x) (dup y)

* __Primitive__ function application inserts dups and drops on its
  arguments, e.g.,

> (+) x y

becomes

> (+) (dup x) (dup y)

* __Let__ introduces new names whose values are dropped after the body
  is evaluated; @let _ =@ are given names so they can be dropped.

> let a = Foo 42
>     _ = a
> 17

becomes

> let a = Foo 42
> drop
>   (let anon1_underscore = dup a
>    drop
>      17
>      anon1_underscore)
>   a

* Nested __Lambda__ expressions are handled by collecting them into a
  single expression with multiple arguments, adding dups and drops to
  the body, and adding drops around the body for each argument (which
  the caller should have duped)

> add a b = a + b

desugars to

> add = fun a (fun b (a + b))

and becomes

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

@
match v
  Foo x = x + 1
  Bar = 42
@

desugars to

@
match v
  Foo pat_anon0 = let x = pat_anon0
                    x + 1
  Bar = 42
@

then becomes

@
match v
  Foo pat_anon0 = drop
                    dup pat_anon0
                    let x = dup pat_anon0
                    drop (
                       dup x + 1
                    ) x
                  ) pat_anon0
  Bar = 42
@

* __Matches__ that scrutinize an expression lift the scrutinee
  into a @let@ then insert dups and drops on the whole thing

@
  match add x y
    10 = 5
    _ = 3
@

becomes

@
  let anon0_scrutinee = (dup add) (dup x) (dup y)
  drop (
    match anon0_scrutinee
      10 = 5
      _ = 3
  ) anon0_scruitinee
@
-}

--
insertExpr :: I.Expr I.Type -> Fresh (I.Expr I.Type)
insertExpr dcon@I.Data{} = return dcon
insertExpr lit@I.Lit{} = return lit
insertExpr var@I.Var{} = return $ makeDup var
insertExpr (I.App f x t) = I.App <$> insertExpr f <*> insertExpr x <*> pure t
insertExpr (I.Prim p es typ) = I.Prim p <$> mapM insertExpr es <*> pure typ
insertExpr (I.Let bins expr typ) = do
  bins' <- forM bins droppedBinder
  expr' <- insertExpr expr
  return $ I.Let (map defFromBind bins') (foldr (makeDrop . varFromBind) expr' bins') typ
 where
  varFromBind (v, t, _) = I.Var v t
  defFromBind (v, t, d) = (I.BindVar v t, d)

  droppedBinder (I.BindAnon t, d) = do
    temp <- getFresh "underscore" t
    d' <- insertExpr d
    return (temp, t, d')
  droppedBinder (I.BindVar v t, d) = do
    d' <- insertExpr d
    return (v, t, d')
insertExpr lam@I.Lambda{} = do
  let (args, body) = I.unfoldLambda lam
  args' <- forM args $ \case
    I.BindAnon t -> do
      v <- getFresh "arg" t
      return (v, t)
    I.BindVar v t -> return (v, t)
  let argBinders = uncurry I.BindVar <$> args' -- zipWith (\(v, t) b -> (I.BindVar v t, b)) args' argTypes
      argVars = uncurry I.Var <$> args'
  body' <- insertExpr body
  return $ I.foldLambda argBinders $ foldr makeDrop body' argVars
insertExpr (I.Match v@I.Var{} alts typ) = do
  alts' <- forM alts insertAlt
  return $ I.Match v alts' typ
insertExpr (I.Match scrutExpr alts typ) = do
  let scrutType = I.extract scrutExpr
  scrutVar <- getFresh "scrutinee" scrutType
  insertExpr $
    I.Let
      [(I.BindVar scrutVar scrutType, scrutExpr)]
      (I.Match (I.Var scrutVar scrutType) alts typ)
      typ
insertExpr e@(I.Exception _ _) = return e


{- | Insert dups and drops into pattern match arms

The body of default and literal patterns is simply recursed upon.

Every named variable in a pattern is duped and dropped, e.g.,

@
match v
  Foo x = expr
@

becomes

@
match v
  Foo _anon1 = drop
                 (let x = dup _anon1
                  expr
               ) x
@
-}
insertAlt :: (I.Alt I.Type, I.Expr I.Type) -> Fresh (I.Alt I.Type, I.Expr I.Type)
insertAlt (I.AltBinder v, e) = (I.AltBinder v,) <$> insertExpr e
insertAlt (I.AltLit l t, e) = (I.AltLit l t,) <$> insertExpr e
insertAlt (I.AltData dcon binds t, body) = do
  body' <- insertExpr body
  -- NOTE: we don't recurse here because we assume alts are already desguared i.e., flat
  return (I.AltData dcon binds t, foldr dropDupLet body' $ concatMap I.altBinders binds)
 where
  dropDupLet (I.BindAnon _) e = e
  dropDupLet (I.BindVar v t') e =
    let varExpr = I.Var v t'
        dupExpr = makeDup varExpr
     in makeDrop varExpr (I.Let [(I.BindAnon $ I.extract dupExpr, dupExpr)] e (I.extract e))
