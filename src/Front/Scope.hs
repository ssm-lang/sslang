{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{- | Check scoping rules and naming conventions for identifiers.

Here, we ensure that all identifiers that appear in the AST only appear after
they are previously declared or defined.

Identifiers can be segregated into two categories: data identifiers, which
produce expressions, and type identifiers, which produce types (more accurately,
type expressions). These inhabit separate namespaces and are distinguished by
the different contexts in which they are used.

Of each category, there are two kinds of identifiers: constructors and
variables. Constructors must begin with an upper case letter or a colon (@:@);
all other identifiers are variables (see 'isCons' and 'isVar'). For instance,
data constructors name the variants of an algebraic data type, while data
variables name values bound by a let-binding, a pattern-match, or a lambda.
Meanwhile, type constructors are points in the type system defined by the user,
while type variables are universally quantified in each type expression.

Consider the following example:

@@
type Bool =
  True
  False

type Either t u =
  Left t
  Right u

liftEither b x y: Bool -> a -> b -> Either a b =
  match b
    True  = Left x
    False = Right y
@@

Data variables are @switch@, @b@, @x@, and @y@; data constructors are @True@,
@False@, @Left@, and @Right@. Type variables are @t@, @u@, @a@, and @b@; type
constructors are @Bool@ and @Either@.

The grammar as it appears in the parser does not actually distinguish between
any of these kinds of identifiers, so it is the responsibility of this module to
check that, for example, a data constructor does not appear where a data
variable is expected, e.g., @let F x = e@, or vice versa, e.g., @let f X = e@.
-}
module Front.Scope
  ( scopeProgram
  ) where

import qualified Front.Ast                     as A
import           Front.Identifiers              ( DataInfo(dataKind)
                                                , IdKind(User)
                                                , TypInfo
                                                , builtinData
                                                , builtinTypes
                                                )

import           Common.Compiler                ( Error(..)
                                                , ErrorMsg
                                                , MonadError(..)
                                                , MonadWriter
                                                , Pass(..)
                                                , Warning(..)
                                                , fromString
                                                , warn
                                                )
import           Common.Default                 ( Default(def) )

import           Common.Identifiers             ( Identifiable(..)
                                                , Identifier(..)
                                                , isCons
                                                , isVar
                                                )
import           Control.Monad                  ( forM_
                                                , unless
                                                , when
                                                )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                , asks
                                                )
import           Data.List                      ( group
                                                , sort
                                                )
import qualified Data.Map                      as M
import           Data.Maybe                     ( isJust )

-- | Report 'Identifier' for error reporting.
showId :: Identifier -> ErrorMsg
showId s = "'" <> fromString (ident s) <> "'"

-- | Scoping environment.
data ScopeCtx = ScopeCtx
  { dataMap :: M.Map Identifier DataInfo
  , typeMap :: M.Map Identifier TypInfo
  }

-- | Scoping monad.
newtype ScopeFn a = ScopeFn (ReaderT ScopeCtx Pass a)
  deriving Functor                      via (ReaderT ScopeCtx Pass)
  deriving Applicative                  via (ReaderT ScopeCtx Pass)
  deriving Monad                        via (ReaderT ScopeCtx Pass)
  deriving MonadFail                    via (ReaderT ScopeCtx Pass)
  deriving (MonadError Error)           via (ReaderT ScopeCtx Pass)
  deriving (MonadWriter [Warning])      via (ReaderT ScopeCtx Pass)
  deriving (MonadReader ScopeCtx)       via (ReaderT ScopeCtx Pass)

-- | Run a ScopeFn computation.
runScopeFn :: ScopeFn a -> Pass a
runScopeFn (ScopeFn m) =
  runReaderT m ScopeCtx { dataMap = builtinData, typeMap = builtinTypes }

-- | Add a list of data identifiers to the scope.
withDataScope :: [(Identifier, DataInfo)] -> ScopeFn a -> ScopeFn a
withDataScope is = do
  local $ \ctx -> ctx { dataMap = foldr (uncurry M.insert) (dataMap ctx) is }

-- | Check that an 'Identifier' is not an empty string.
ensureNonempty :: Identifier -> ScopeFn ()
ensureNonempty i =
  when (null $ ident i) $ throwError $ UnexpectedError "Empty identifier"

-- | Check that a set of bindings does not define overlapping 'Identifier'.
ensureUnique :: [Identifier] -> ScopeFn ()
ensureUnique ids = do
  forM_ (group $ sort ids) $ \case
    []    -> throwError $ UnexpectedError "unique should not be empty"
    [ _ ] -> return ()
    i : _ -> throwError $ ScopeError $ "Defined more than once: " <> showId i

-- | Check that a constructor 'Identifier' has the right naming convention.
ensureCons :: Identifier -> ScopeFn ()
ensureCons i = do
  ensureNonempty i
  unless (isCons i) $ throwError nameErr
 where
  nameErr =
    NameError
      $  showId i
      <> " should begin with upper case or begin and end with ':'"

-- | Check that a variable 'Identifier' has the right naming convention.
ensureVar :: Identifier -> ScopeFn ()
ensureVar i = do
  ensureNonempty i
  unless (isVar i) $ throwError nameErr
 where
  nameErr =
    NameError
      $  showId i
      <> " should begin with upper case or begin and end with ':'"

{- | Validate a declaration of a data 'Identifier'.

This includes binding to the left of the @=@ in a let-binding or pattern match,
and in the argument of lambdas. Does not include type definitions, where data
constructors are defined.
-}
dataDecl :: Identifier -> ScopeFn ()
dataDecl i = do
  info <- asks $ M.lookup i . dataMap

  -- A data constructor cannot be defined inside of a pattern.
  when (isCons i && not (inScope info)) $ do
    throwError $ ScopeError $ "Data constructor is out of scope: " <> showId i

  -- A data variable can usually be shadowed, but we want warn about it.
  when (isVar i && inScope info) $ if canShadow info
    then warn $ NameWarning $ "shadowing variable: " <> showId i
    else
      throwError $ NameError $ "Cannot bind identifier shadowing: " <> showId i

 where
  inScope   = isJust
  canShadow = maybe True ((== User) . dataKind)

-- | Validate a reference to a data 'Identifier'.
dataRef :: Identifier -> ScopeFn ()
dataRef i = do
  inScope <- asks $ M.member i . dataMap
  unless inScope $ throwError $ ScopeError $ "Not in scope: " <> showId i

-- | Validate a reference to a type 'Identifier'.
typeRef :: Identifier -> ScopeFn ()
typeRef i = do
  inScope <- asks $ M.member i . typeMap
  when (not inScope && isCons i) $ do
    throwError $ ScopeError $ "Type constructor is out of scope: " <> showId i
  -- Type variables are implicitly quantified, so we always allow any.

-- | Check the scoping of a 'A.Program'.
scopeProgram :: A.Program -> Pass ()
scopeProgram (A.Program ds) = runScopeFn $ scopeDefs ds $ return ()

-- | Check the scoping of a set of parallel (co-recursive) 'A.Definition'.
scopeDefs :: [A.Definition] -> ScopeFn () -> ScopeFn ()
scopeDefs ds k = do
  corecs <- concat <$> mapM scopeCorec ds
  ensureUnique $ map fst corecs
  mapM_ (scopeDef corecs) ds
  withDataScope corecs k
 where
  -- | Retrieve the 'Identifier' list exposed to corecursive siblings.
  scopeCorec :: A.Definition -> ScopeFn [(Identifier, DataInfo)]
  scopeCorec (A.DefFn f _ps t _e) = do
    scopeTypeFn t
    ensureVar f
    dataDecl f
    return [(f, def)]
  scopeCorec (A.DefPat p _) = do
    ids <- scopePat p
    -- Not strictly necessary here, but gives a more precise error message.
    ensureUnique $ map fst ids
    return ids

  -- | Given a corecursive environment, each 'A.Definition' (and its 'A.Expr').
  scopeDef :: [(Identifier, DataInfo)] -> A.Definition -> ScopeFn ()
  scopeDef corecs (A.DefFn _f ps _t e) = do
    -- NOTE: corecs should already contain _f
    ids <- (corecs ++) . concat <$> mapM scopePat ps
    ensureUnique $ map fst ids
    withDataScope ids $ scopeExpr e
  scopeDef corecs (A.DefPat _p e) = do
    -- NOTE: corecs should already contain _p's identifiers
    withDataScope corecs $ scopeExpr e

-- | Check the scoping of an 'A.Expr'.
scopeExpr :: A.Expr -> ScopeFn ()
scopeExpr (A.Id i      ) = dataRef i
scopeExpr (A.Match s as) = do
  scopeExpr s
  forM_ as $ \(p, b) -> do
    ids <- scopePat p
    ensureUnique $ map fst ids
    withDataScope ids $ scopeExpr b
scopeExpr (A.Lambda as b) = do
  args <- concat <$> mapM scopePat as
  ensureUnique $ map fst args
  withDataScope args $ scopeExpr b
scopeExpr (A.Let        ds b) = scopeDefs ds $ scopeExpr b
scopeExpr (A.Constraint e  t) = scopeType t >> scopeExpr e
scopeExpr (A.Apply      f  a) = scopeExpr f >> scopeExpr a
scopeExpr (A.While      c  b) = scopeExpr c >> scopeExpr b
scopeExpr (A.Loop b         ) = scopeExpr b
scopeExpr (A.Par  es        ) = mapM_ scopeExpr es
scopeExpr (A.IfElse c i e   ) = mapM_ scopeExpr [c, i, e]
scopeExpr (A.After  d l r   ) = mapM_ scopeExpr [d, l, r]
scopeExpr (A.Assign l r     ) = mapM_ scopeExpr [l, r]
scopeExpr (A.Wait es        ) = mapM_ scopeExpr es
scopeExpr (A.Seq e e'       ) = mapM_ scopeExpr [e, e']
scopeExpr (A.Return e       ) = scopeExpr e
scopeExpr (A.Lit    _       ) = return ()
scopeExpr A.Break             = return ()
scopeExpr (A.OpRegion e o) =
  throwError
    $  UnexpectedError
    $  "OpRegion should not be reachable: "
    <> fromString (show e)
    <> " "
    <> fromString (show o)
scopeExpr A.NoExpr =
  throwError $ UnexpectedError "NoExpr should not be reachable"

{- | Check 'A.Pat' and retrieve variable identifiers defined therein.

Not responsible for ensuring data identifiers are in scope, though it will call
'scopeType' to check type identifiers.
-}
scopePat :: A.Pat -> ScopeFn [(Identifier, DataInfo)]
scopePat (A.PatId i) = do
  -- NOTE: here, i may be either a data constructor or a variable name.
  dataDecl i
  return $ [ (i, def) | isVar i ]
scopePat (A.PatAs i p) = do
  -- When we have v@p, v should always be a variable identifier.
  ensureVar i
  dataDecl i
  ([ (i, def) | isVar i ] ++) <$> scopePat p
scopePat (A.PatTup pats)
  | length pats < 2 = throwError
  $ UnexpectedError "PatTup should have arity greater than 2"
  | otherwise = concat <$> mapM scopePat pats
scopePat (A.PatApp []) =
  throwError $ UnexpectedError "PatApp should not be empty"
scopePat (A.PatApp [_]) =
  throwError $ UnexpectedError "PatApp should not be singleton"
scopePat (A.PatApp pats@(A.PatId i : _)) = do
  ensureCons i
  concat <$> mapM scopePat pats
scopePat (A.PatApp _) = do
  -- We encountered something like @let f ((x y) z) = ...@
  throwError
    $ PatternError "Head of destructuring pattern must be a data constructor"
scopePat (A.PatAnn typ pat) = scopeType typ >> scopePat pat
scopePat (A.PatLit _      ) = return []
scopePat A.PatWildcard      = return []

-- | Check scoping for a 'A.TypFn' annotation.
scopeTypeFn :: A.TypFn -> ScopeFn ()
scopeTypeFn (A.TypReturn typ) = scopeType typ
scopeTypeFn (A.TypProper typ) = scopeType typ
scopeTypeFn A.TypNone         = return ()

-- | Check scoping for a 'A.Typ' annotation.
scopeType :: A.Typ -> ScopeFn ()
scopeType (A.TCon i    ) = typeRef i
scopeType (A.TApp f a  ) = mapM_ scopeType [f, a]
scopeType (A.TTuple ts ) = mapM_ scopeType ts
scopeType (A.TArrow a r) = mapM_ scopeType [a, r]
