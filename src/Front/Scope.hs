{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
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
check that, a data constructor does not appear where a data variable is
expected, e.g., @let F x = e@, or vice versa, e.g., @let f (x Y) = e@.
-}
module Front.Scope
  ( scopeProgram
  ) where

import qualified Front.Ast                     as A
import           Front.Identifiers              ( DataInfo(..)
                                                , IdKind(..)
                                                , TypInfo(..)
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
import           Data.Maybe                     ( isJust
                                                , mapMaybe
                                                )

-- | Report 'Identifier' for error reporting.
showId :: Identifier -> ErrorMsg
showId s = "'" <> fromString (ident s) <> "'"

{- | Scoping environment, maintained during the scoping pass.

In type expressions that act as type annotations, type variables are implicitly
qualified at the top-level, so any type variable is always in scope. So, an
annotation like @id : a -> a@ is legal, and means @id : forall a. a -> a@.

However, in the context of type definitions, type variables must be quantified.
So something like @type T a = D a b@ is illegal because the type variable @b@
does not appear as a parameter of unary type constructor @T@.

To account for this discrepancy, the 'implicitScheme' field of the scoping
environment is used to keep track of this context.
-}
data ScopeCtx = ScopeCtx
  { dataMap        :: M.Map Identifier DataInfo -- ^ Map of in-scope data ids
  , typeMap        :: M.Map Identifier TypInfo  -- ^ Map of in-scope type ids
  , implicitScheme :: Bool                      -- ^ Allow implicit type vars
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
runScopeFn (ScopeFn m) = runReaderT
  m
  ScopeCtx { dataMap        = builtinData
           , typeMap        = builtinTypes
           , implicitScheme = True
           }

-- | Add a list of data identifiers to the scope.
withTypeScope :: [(Identifier, TypInfo)] -> ScopeFn a -> ScopeFn a
withTypeScope is =
  local $ \ctx -> ctx { typeMap = foldr (uncurry M.insert) (typeMap ctx) is }

-- | Add a list of data identifiers to the scope.
withDataScope :: [(Identifier, DataInfo)] -> ScopeFn a -> ScopeFn a
withDataScope is =
  local $ \ctx -> ctx { dataMap = foldr (uncurry M.insert) (dataMap ctx) is }

withExplicitScheme :: [Identifier] -> ScopeFn a -> ScopeFn a
withExplicitScheme is = local $ \ctx -> ctx
  { typeMap        = foldr (uncurry M.insert)
                           (typeMap ctx)
                           (zip is $ repeat TypInfo { typKind = User })
  , implicitScheme = False
  }

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
  let _         = print (showId i)
  let isDupDrop = ident i == "dup" || ident i == "drop"
  inScope <- asks $ M.member i . dataMap
  unless (inScope || isDupDrop)
    $  throwError
    $  ScopeError
    $  "Not in scope: "
    <> showId i

-- | Validate a reference to a type 'Identifier'.
typeRef :: Identifier -> ScopeFn ()
typeRef i = do
  inScope       <- asks $ M.member i . typeMap
  allowImplicit <- asks implicitScheme
  when (not inScope && isCons i) $ do
    throwError $ ScopeError $ "Type constructor is out of scope: " <> showId i
  when (not inScope && isVar i && not allowImplicit) $ do
    throwError $ ScopeError $ "Type variable is not defined: " <> showId i

-- | Check the scoping of a 'A.Program'.
scopeProgram :: A.Program -> Pass ()
scopeProgram (A.Program ds) = runScopeFn $ do
  scopeTypeDefs tds $ scopeExterns eds $ scopeDefs dds $ return ()
 where
  eds = mapMaybe A.getTopExtern ds
  tds = mapMaybe A.getTopTypeDef ds
  dds = mapMaybe A.getTopDataDef ds

-- | Check the scoping of a set of type definitions.
scopeTypeDefs :: [A.TypeDef] -> ScopeFn () -> ScopeFn ()
scopeTypeDefs tds k = do
  -- Check and collect all type constructors, which are mutually recursive.
  tcons <- mapM scopeTCons tds
  ensureUnique $ map fst tcons
  withTypeScope tcons $ do
    -- Then, check and collect all data constructors.
    dcons <- concat <$> mapM scopeDCons tds
    ensureUnique $ map fst dcons
    -- Continuation k is checked with all type and data constructors in scope
    withDataScope dcons k

-- | Check the scoping of a user-defined type constructor.
scopeTCons :: A.TypeDef -> ScopeFn (Identifier, TypInfo)
scopeTCons A.TypeDef { A.typeName = tn } = do
  ensureCons tn
  return (tn, TypInfo { typKind = User })

-- | Check the scoping of the data constructors of a type definition.
scopeDCons :: A.TypeDef -> ScopeFn [(Identifier, DataInfo)]
scopeDCons A.TypeDef { A.typeVariants = tvs, A.typeParams = tps } = do
  mapM_ ensureVar tps
  ensureUnique tps
  withExplicitScheme tps $ do
    mapM scopeDcon tvs

-- | Check the scoping of the data constructor a single data variant.
scopeDcon :: A.TypeVariant -> ScopeFn (Identifier, DataInfo)
scopeDcon (A.VariantUnnamed dcon ts) = do
  ensureCons dcon
  mapM_ scopeType ts
  return (dcon, DataInfo { dataKind = User })

scopeExterns :: [A.ExternDecl] -> ScopeFn () -> ScopeFn ()
scopeExterns ds k = do
  mapM_ scopeType xTypes
  withDataScope (map (, def) xIds) k
  where (xIds, xTypes) = unzip $ map (\(A.ExternDecl i t) -> (i, t)) ds

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
scopeExpr (A.Lit _          ) = return ()
scopeExpr A.Break             = return ()
scopeExpr (A.OpRegion e o) =
  throwError
    $  UnexpectedError
    $  "OpRegion should not be reachable: "
    <> fromString (show e)
    <> " "
    <> fromString (show o)
scopeExpr (A.CQuote _  ) = return ()
scopeExpr (A.CCall _ es) = mapM_ scopeExpr es
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
