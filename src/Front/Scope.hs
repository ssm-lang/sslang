{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
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
                                                , Pass(..)
                                                , fromString
                                                )
import           Common.Default                 ( Default(def) )

import           Common.Identifiers             ( Identifiable(..)
                                                , Identifier(..)
                                                , hasRepeated
                                                , isCons
                                                , isVar
                                                )
import           Control.Monad                  ( forM_
                                                , unless
                                                , when
                                                )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                , asks
                                                )
import qualified Data.Map                      as M
import           Data.Maybe                     ( isJust )

showId :: Identifier -> String
showId s = "'" ++ ident s ++ "'"

-- | Scoping environment
data ScopeCtx = ScopeCtx
  { dataMap :: M.Map Identifier DataInfo
  , typeMap :: M.Map Identifier TypInfo
  }

-- | Scoping monad
newtype ScopeFn a = ScopeFn (ReaderT ScopeCtx Pass a)
  deriving Functor                      via (ReaderT ScopeCtx Pass)
  deriving Applicative                  via (ReaderT ScopeCtx Pass)
  deriving Monad                        via (ReaderT ScopeCtx Pass)
  deriving MonadFail                    via (ReaderT ScopeCtx Pass)
  deriving (MonadError Error)           via (ReaderT ScopeCtx Pass)
  deriving (MonadReader ScopeCtx)       via (ReaderT ScopeCtx Pass)

-- | Run a ScopeFn computation.
runScopeFn :: ScopeFn a -> Pass a
runScopeFn (ScopeFn m) =
  runReaderT m ScopeCtx { dataMap = builtinData, typeMap = builtinTypes }

withDataScope :: [(Identifier, DataInfo)] -> ScopeFn a -> ScopeFn a
withDataScope is = do
  local $ \ctx -> ctx { dataMap = foldr (uncurry M.insert) (dataMap ctx) is }

ensureNonempty :: Identifier -> ScopeFn ()
ensureNonempty i =
  when (null $ ident i) $ throwError $ UnexpectedError "Empty identifier"

ensureCons :: Identifier -> ScopeFn ()
ensureCons i = do
  ensureNonempty i
  unless (isCons i) $ throwError nameErr
 where
  nameErr =
    NameError
      $  fromString
      $  showId i
      ++ " should begin with upper case or begin and end with ':'"

ensureVar :: Identifier -> ScopeFn ()
ensureVar i = do
  ensureNonempty i
  unless (isVar i) $ throwError nameErr
 where
  nameErr =
    NameError
      $  fromString
      $  showId i
      ++ " should begin with upper case or begin and end with ':'"

dataDecl :: Identifier -> ScopeFn ()
dataDecl i = do
  info <- asks $ M.lookup i . dataMap

  -- A data constructor cannot be defined inside of a pattern.
  when (isCons i && not (inScope info)) $ do
    throwError
      $  ScopeError
      $  fromString
      $  "Data constructor is out of scope: "
      ++ showId i

  when (isVar i && inScope info) $ if canShadow info
    then
      -- TODO: warn about shadowing
         return ()
    else
      throwError
      $  NameError
      $  fromString
      $  "Cannot bind identifier shadowing : "
      ++ showId i

 where
  inScope   = isJust
  canShadow = not . maybe False ((== User) . dataKind)

dataRef :: Identifier -> ScopeFn ()
dataRef i = do
  inScope <- asks $ M.member i . dataMap
  unless inScope
    $  throwError
    $  ScopeError
    $  fromString
    $  "Not in scope: "
    ++ showId i

typeRef :: Identifier -> ScopeFn ()
typeRef i = do
  inScope <- asks $ M.member i . typeMap
  when (not inScope && isCons i) $ do
    throwError
      $  ScopeError
      $  fromString
      $  "Type constructor is out of scope: "
      ++ showId i
  -- NOTE: type variables are implicitly quantified, so we always allow any.

scopeProgram :: A.Program -> Pass ()
scopeProgram (A.Program ds) = runScopeFn $ scopeDefs ds $ return ()

scopeDefs :: [A.Definition] -> ScopeFn () -> ScopeFn ()
scopeDefs ds k = do
  corecs <- concat <$> mapM scopeCorec ds
  mapM_ (scopeDef corecs) ds
  withDataScope corecs k
 where
  scopeCorec :: A.Definition -> ScopeFn [(Identifier, DataInfo)]
  scopeCorec (A.DefFn f _ps t _e) = do
    scopeTypeFn t
    ensureVar f
    dataDecl f
    return [(f, def)]
  scopeCorec (A.DefPat p _) = do
    ids <- scopePat p
    when (hasRepeated $ map fst ids) $ do
      -- Not strictly necessary here, but gives a more precise error message.
      throwError $ ScopeError "Overlapping names in pattern"
    return ids

  scopeDef :: [(Identifier, DataInfo)] -> A.Definition -> ScopeFn ()
  scopeDef corecs (A.DefFn _f ps _t e) = do
    -- NOTE: corecs should already contain _f
    ids <- (corecs ++) . concat <$> mapM scopePat ps
    when (hasRepeated $ map fst ids) $ do
      throwError $ ScopeError "Overlapping identifiers"
    withDataScope ids $ scopeExpr e
  scopeDef corecs (A.DefPat _p e) = do
    -- NOTE: corecs should already contain _p's identifiers
    withDataScope corecs $ scopeExpr e

scopeExpr :: A.Expr -> ScopeFn ()
scopeExpr (A.Id i      ) = dataRef i
scopeExpr (A.Match s as) = do
  scopeExpr s
  forM_ as $ \(p, b) -> do
    ids <- scopePat p
    when (hasRepeated $ map fst ids) $ do
      throwError $ ScopeError "Overlapping variable names"
    withDataScope ids $ scopeExpr b
scopeExpr (A.Lambda as b) = do
  args <- concat <$> mapM scopePat as
  when (hasRepeated $ map fst args) $ do
    throwError $ ScopeError "Overlapping variable names"
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
    $  fromString
    $  "OpRegion should not be reachable: "
    ++ show e
    ++ " "
    ++ show o
scopeExpr A.NoExpr =
  throwError $ UnexpectedError "NoExpr should not be reachable"

-- | Retrieve new variable identifiers defined in a pattern.
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

scopeTypeFn :: A.TypFn -> ScopeFn ()
scopeTypeFn (A.TypReturn typ) = scopeType typ
scopeTypeFn (A.TypProper typ) = scopeType typ
scopeTypeFn A.TypNone         = return ()

scopeType :: A.Typ -> ScopeFn ()
scopeType (A.TCon i    ) = typeRef i
scopeType (A.TApp f a  ) = mapM_ scopeType [f, a]
scopeType (A.TTuple ts ) = mapM_ scopeType ts
scopeType (A.TArrow a r) = mapM_ scopeType [a, r]
