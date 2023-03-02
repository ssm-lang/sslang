{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE ViewPatterns #-}
{- | Lower the representation of a sslang Ast into sslang IR.

This pass expects prior desugaring passes to ensure that:

- Op regions are unflatted into proper applications.
- Patterns in definitions consist of only (annotated) identifiers or wildcards.
-}
module IR.LowerAst
  ( lowerProgram
  ) where

import qualified Common.Compiler               as Compiler
import           Common.Identifiers             ( TVarId(..)
                                                , fromId
                                                , fromString
                                                , isCons
                                                , isVar
                                                )
import qualified Front.Ast                     as A
import qualified IR.IR                         as I
import qualified IR.Types                      as I

import           Data.Bifunctor                 ( Bifunctor(..) )
import IR.Types.Type (tempTupleId)

-- | Unannotated terms appear as an empty stack.
untyped :: I.Annotations
untyped = mempty

-- | Construct a single annotation.
ann :: I.Annotation -> I.Annotations
ann t = I.Annotations [t]

-- | Lower an AST 'Program' into IR.
lowerProgram :: A.Program -> Compiler.Pass (I.Program I.Annotations)
lowerProgram (A.Program ds) = do
  let (tds, cds, xds, dds) = A.getTops ds
  tds' <- mapM lowerTypeDef tds
  xds' <- mapM lowerExternDecl xds
  dds' <- mapM lowerDataDef dds
  return $ I.Program { I.programEntry = fromString "main" -- TODO: don't hardcode
                     , I.programDefs  = dds'
                     , I.externDecls  = xds'
                     , I.typeDefs     = tds'
                     , I.cDefs        = concat cds
                     }
 where
  -- | Lower a top-level data definition into IR.
  lowerDataDef :: A.Definition -> Compiler.Pass (I.VarId, I.Expr I.Annotations)
  lowerDataDef d = lowerDef d >>= \case
    (I.BindVar b _, e) -> return (b, e)
    (_, _) -> Compiler.unexpected "Missing top-level binding"
    -- TODO: This might actually be ok, to allow top-level evaluation

  -- | Lower a top-level type definition into IR.
  lowerTypeDef :: A.TypeDef -> Compiler.Pass (I.TConId, I.TypeDef)
  lowerTypeDef td = do
    tds <- mapM lowerTypeVariant $ A.typeVariants td
    return
      ( fromId $ A.typeName td
      , I.TypeDef { I.targs = map TVarId $ A.typeParams td, I.variants = tds }
      )
   where
    lowerTypeVariant
      :: A.TypeVariant -> Compiler.Pass (I.DConId, I.TypeVariant)
    lowerTypeVariant (A.VariantUnnamed vn ts) =
      (fromId vn, ) . I.VariantUnnamed <$> mapM lowerType ts

  -- | Lower an 'A.ExternDecl' into an identifier/type pair.
  lowerExternDecl :: A.ExternDecl -> Compiler.Pass (I.VarId, I.Type)
  lowerExternDecl (A.ExternDecl i t) = (fromId i, ) <$> lowerType t

-- | Lower an 'A.Definition' into a name and bound expression.
lowerDef :: A.Definition -> Compiler.Pass (I.Binder I.Annotations, I.Expr I.Annotations)
lowerDef (A.DefPat aPat aBody) = do
  n <- lowerPatBinder aPat
  b <- lowerExpr aBody
  return (n, b)
lowerDef (A.DefFn aName aArgs (A.TypProper ty) aBody) = do
  typAnn <- I.AnnType <$> lowerType ty
  b <- lowerCurry aArgs aBody
  return (I.BindVar (fromId aName) (ann typAnn), b)
lowerDef (A.DefFn aName aArgs (A.TypReturn ty) aBody) = do
  b <- lowerCurry aArgs (A.Constraint aBody ty)
  return (I.BindVar (fromId aName) untyped, b)
lowerDef (A.DefFn aName aArgs A.TypNone aBody) = do
  b <- lowerCurry aArgs aBody
  return (I.BindVar (fromId aName) untyped, b)

-- | Curry and lower a list of arguments to a lambda body.
lowerCurry :: [A.Pat] -> A.Expr -> Compiler.Pass (I.Expr I.Annotations)
lowerCurry aPats aBody = go aPats
 where
  go []       = lowerExpr aBody
  go (p : ps) = I.Lambda <$> lowerPatBinder p <*> go ps <*> pure untyped

{- | Lowers an AST expression into an IR expression.

Performs the following desugaring inline:

-   Desugars 'A.IfElse' to 'I.Match'
-   Unrolls 'A.Constraint' to annotate sub-expressions
-}
lowerExpr :: A.Expr -> Compiler.Pass (I.Expr I.Annotations)
lowerExpr (lowerPrim -> Just p) = return $ I.Prim p [] untyped
lowerExpr (A.Id v) | isCons v  = return $ I.Data (fromId v) untyped
                   | otherwise = return $ I.Var (fromId v) untyped
lowerExpr (  A.Lit l    ) = I.Lit <$> lowerLit l <*> pure untyped
lowerExpr a@(A.Apply l r) = case first lowerPrim (A.collectApp a) of
  (Just prim, args) -> I.Prim prim <$> mapM lowerExpr args <*> pure untyped
  (Nothing  , _   ) -> I.App <$> lowerExpr l <*> lowerExpr r <*> pure untyped
lowerExpr (A.Let ds b) =
  I.Let <$> mapM lowerDef ds <*> lowerExpr b <*> pure untyped
lowerExpr (A.Lambda ps b) = do
  lowerCurry ps b
lowerExpr (A.While c b) = do
  body <- lowerExpr (A.IfElse c (A.Lit A.LitEvent) A.Break `A.Seq` b)
  return $ I.Prim I.Loop [body] untyped
lowerExpr (A.Loop b) = do
  body <- lowerExpr b
  return $ I.Prim I.Loop [body] untyped
lowerExpr (A.Par es) = I.Prim I.Par <$> mapM lowerExpr es <*> pure untyped
lowerExpr (A.After delay lhs rhs) =
  I.Prim I.After <$> mapM lowerExpr [delay, lhs, rhs] <*> pure untyped
lowerExpr (A.Assign lhs rhs) =
  I.Prim I.Assign <$> mapM lowerExpr [lhs, rhs] <*> pure untyped
lowerExpr (A.Constraint e ty) = do
  e' <- lowerExpr e
  ty' <- I.AnnType <$> lowerType ty
  return $ I.injectMore (ann ty') e'
lowerExpr (A.Wait exprs) =
  I.Prim I.Wait <$> mapM lowerExpr exprs <*> pure untyped
lowerExpr (A.Seq l r) = do
  l' <- lowerExpr l
  r' <- lowerExpr r
  return $ I.Let [(I.BindAnon $ I.extract l', l')] r' untyped
lowerExpr A.Break          = return $ I.Prim I.Break [] untyped
lowerExpr (A.IfElse c t e) = do
  c' <- lowerExpr c
  t' <- lowerExpr t
  e' <- lowerExpr e
  let altT = (I.AltBinder $ I.BindAnon $ I.extract t', t')
      altE = (I.AltLit (I.LitIntegral 0) $ I.extract e', e')
  return $ I.Match c' [altE, altT] untyped
lowerExpr (A.CQuote s) = return $ I.Prim (I.CQuote $ fromString s) [] untyped
lowerExpr (A.CCall s es) =
  I.Prim (I.CCall $ fromId s) <$> mapM lowerExpr es <*> pure untyped
lowerExpr A.NoExpr = return $ I.Lit I.LitEvent untyped
lowerExpr (A.OpRegion _ _) =
  Compiler.unexpected "lowerExpr: OpRegions should have already been desugared"
lowerExpr (A.Match s ps) =
  I.Match <$> lowerExpr s <*> mapM lowerArm ps <*> pure untyped
  where lowerArm (a, e) = (,) <$> lowerPatAlt a <*> lowerExpr e
lowerExpr (A.Tuple es) =
  apply_recurse (I.Data (I.DConId (tempTupleId $ length es)) untyped) <$> mapM lowerExpr es
 where
  apply_recurse e []       = e
  apply_recurse e (x : xs) = apply_recurse (I.App e x untyped) xs
lowerExpr (A.ListExpr _) =
  Compiler.unexpected "lowerExpr: ListExprs should have already been desugared"

-- | Lower an A.Pat into an I.Alt
lowerPatAlt :: A.Pat -> Compiler.Pass (I.Alt I.Annotations)
lowerPatAlt A.PatWildcard = return $ I.AltBinder $ I.BindAnon untyped
lowerPatAlt (A.PatId i) | isVar i   = return $ I.AltBinder $ I.BindVar (I.VarId i) untyped
                        | otherwise = return $ I.AltData (I.DConId i) [] untyped
lowerPatAlt (A.PatLit l) = I.AltLit <$> lowerLit l <*> pure untyped
lowerPatAlt (A.PatTup ps) =
  I.AltData (I.tempTupleId $ length ps) <$> mapM lowerPatAlt ps <*> pure untyped
lowerPatAlt p@(A.PatApp _) = case A.collectPApp p of
  (A.PatId i, ps) | isCons i -> I.AltData (fromId i) <$> mapM lowerPatAlt ps <*> pure untyped
  _ -> Compiler.unexpected "lowerPatAlt: app head should be a data constructor"
lowerPatAlt (A.PatAnn typ p) = do
  t <- I.AnnType <$> lowerType typ
  a <- lowerPatAlt p
  return $ I.injectMore (ann t) a
lowerPatAlt (A.PatAs _ _) =
  Compiler.todo "lowerPatAlt cannot handle aliases yet"

-- | Lower an AST literal into an IR literal.
lowerLit :: A.Literal -> Compiler.Pass I.Literal
lowerLit (A.LitInt i)   = return $ I.LitIntegral i
lowerLit A.LitEvent     = return I.LitEvent
lowerLit (A.LitChar _c) = Compiler.todo "Char literals are not yet implemented"
lowerLit (A.LitString _s) =
  Compiler.unexpected "lowerLit: LitStrings should have already been desugared"
lowerLit (A.LitRat _r) =
  Compiler.todo "Rational literals are not yet implemented"

-- | Translate an AST identifier into the corresponding IR primitive, if any.
lowerPrim :: A.Expr -> Maybe I.Primitive
lowerPrim (A.Id "new"  ) = Just I.New
lowerPrim (A.Id "deref") = Just I.Deref
lowerPrim (A.Id "now"  ) = Just I.Now
lowerPrim (A.Id "+"    ) = Just $ I.PrimOp I.PrimAdd
lowerPrim (A.Id "-"    ) = Just $ I.PrimOp I.PrimSub
lowerPrim (A.Id "*"    ) = Just $ I.PrimOp I.PrimMul
lowerPrim (A.Id "/"    ) = Just $ I.PrimOp I.PrimDiv
lowerPrim (A.Id "%"    ) = Just $ I.PrimOp I.PrimMod
lowerPrim (A.Id "=="   ) = Just $ I.PrimOp I.PrimEq
lowerPrim (A.Id "!="   ) = Just $ I.PrimOp I.PrimNeq
lowerPrim (A.Id ">="   ) = Just $ I.PrimOp I.PrimGe
lowerPrim (A.Id "<="   ) = Just $ I.PrimOp I.PrimLe
lowerPrim (A.Id ">"    ) = Just $ I.PrimOp I.PrimGt
lowerPrim (A.Id "<"    ) = Just $ I.PrimOp I.PrimLt
lowerPrim _              = Nothing

-- | Extract an optional identifier from an Ast pattern.
lowerPatBinder :: A.Pat -> Compiler.Pass (I.Binder I.Annotations)
lowerPatBinder (A.PatId v)      = return $ I.BindVar (fromId v) untyped
lowerPatBinder A.PatWildcard    = return $ I.BindAnon untyped
lowerPatBinder (A.PatAnn typ pat) = do
  t <- I.AnnType <$> lowerType typ
  b <- lowerPatBinder pat
  return $ I.injectMore (ann t) b
lowerPatBinder p =
  Compiler.todo
    $  "lowerPatBinder: should be desguared into pattern match: "
    ++ show p

-- | Lowers the AST's representation of types into that of the IR.
lowerType :: A.Typ -> Compiler.Pass I.Type
lowerType (A.TCon "Int") = return I.I32
lowerType (A.TCon "()" ) = return I.Unit
lowerType (A.TCon i) | isCons i  = return $ I.TCon (fromId i) []
                     | otherwise = return $ I.TVar (fromId i)
lowerType (  A.TTuple tys    ) = I.tuple <$> mapM lowerType tys
lowerType (  A.TArrow lhs rhs) = I.Arrow <$> lowerType lhs <*> lowerType rhs
lowerType a@(A.TApp   _   _  ) = case A.collectTApp a of
  (A.TCon "&" , [arg])        -> I.Ref <$> lowerType arg
  (A.TCon "[]", [arg])        -> I.List <$> lowerType arg
  (A.TCon i, args) | isCons i -> I.TCon (fromId i) <$> mapM lowerType args
  _ ->
    Compiler.unexpected
      $  "lowerType cannot type application with non-constant head: "
      ++ show a
