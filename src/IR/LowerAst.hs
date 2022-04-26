{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{- | Lower the representation of a sslang Ast into sslang IR.

This pass expects prior desugaring passes to ensure that:

- Op regions are unflatted into proper applications.
- Patterns in definitions consist of only (annotated) identifiers or wildcards.
-}
module IR.LowerAst
  ( lowerProgram
  ) where

import qualified Common.Compiler               as Compiler

import qualified Front.Ast                     as A

import qualified IR.IR                         as I
import qualified IR.Types.Annotated            as I
import qualified IR.Types.TypeSystem           as I

import           Common.Identifiers             ( TVarId(..)
                                                , fromId
                                                , fromString
                                                , isCons
                                                )

import           Control.Comonad                ( Comonad(..) )
import           Data.Bifunctor                 ( Bifunctor(..) )
import           Data.Maybe                     ( mapMaybe )
{- | IR type annotation continuation.

In the AST, type annotations appear as first-class expression/pattern nodes that
wrap other nodes, whereas in the IR, type annotations appear as data that appear
alongside those notes. Thus a common pattern that emerges from this pass is to
accummulate a continuation of type annotations via composition while those AST
annotations are being unwrapped.
-}
type AnnotationK = I.Type -> I.Type

-- | Lowers the AST's representation of types into that of the IR.
lowerType :: A.Typ -> I.Type
lowerType (A.TTuple tys    ) = I.tuple $ map lowerType tys
lowerType (A.TArrow lhs rhs) = lowerType lhs `I.arrow` lowerType rhs
lowerType (A.TCon "Int"    ) = I.int 32
lowerType (A.TCon "()"     ) = I.unit
lowerType (A.TCon i) | isCons i  = I.Type [I.TCon (fromId i) []]
                     | otherwise = I.Type [I.TVar (fromId i)]
lowerType a@(A.TApp _ _) = case A.collectTApp a of
  (A.TCon "&" , [arg] )       -> I.ref $ lowerType arg
  (A.TCon "[]", [_arg])       -> error "list types are not yet implemented"
  (A.TCon i, args) | isCons i -> I.Type [I.TCon (fromId i) $ map lowerType args]
  _ ->
    error $ "Cannot lower type application with non-constant head: " ++ show a

-- | Lower an AST 'Program' into IR.
lowerProgram :: A.Program -> Compiler.Pass (I.Program I.Type)
lowerProgram (A.Program ds) = do
  dds <- mapM lowerDataDef $ mapMaybe A.getTopDataDef ds
  tds <- mapM lowerTypeDef $ mapMaybe A.getTopTypeDef ds
  return $ I.Program { I.programEntry = fromString "main" -- TODO: don't hardcode
                     , I.programDefs  = dds
                     , I.typeDefs     = tds
                     }

-- | Lower a top-level data definition into IR.
lowerDataDef :: A.Definition -> Compiler.Pass (I.VarId, I.Expr I.Type)
lowerDataDef d = case lowerDef d of
  (Just b , e) -> return (b, e)
  (Nothing, _) -> Compiler.unexpected "Missing top-level binding"
  -- TODO: This might actually be ok, to allow top-level evaluation

-- | Lower a top-level type definition into IR.
lowerTypeDef :: A.TypeDef -> Compiler.Pass (I.TConId, I.TypeDef I.Type)
lowerTypeDef A.TypeDef { A.typeName = tn, A.typeParams = tvs, A.typeVariants = tds }
  = return
    ( fromId tn
    , I.TypeDef { I.targs    = map TVarId tvs
                , I.variants = map lowerTypeVariant tds
                }
    )
 where
  lowerTypeVariant (A.VariantUnnamed vn ts) =
    (fromId vn, I.VariantUnnamed $ map lowerType ts)

{- | Lower an 'A.Definition' into a name and bound expression.

Upon encountering a function definition 'A.DefFn', the args are unpacked in
a series of nested anonymous functions 'I.Lambda', bound to the function name in
the IR.
-}
lowerDef :: A.Definition -> (I.Binder, I.Expr I.Type)
lowerDef (A.DefPat aPat aBody) =
  (lowerPatName aPat, lowerExpr aBody (lowerPatType aPat <>))
lowerDef (A.DefFn aName aPats aTy aBody) =
  (Just $ fromId aName, lowerLambda aPats aBody lambdaAnn lambdaRetAnn)
 where
  {- | Where the 'A.TypFn' type annotation should be applied.

  When @let f x y = b@ is desugared into @let f = \x -> \y -> b@
  'A.TypProper' means the type annotation applies to the definition bound to @f@
  (i.e., @\x -> \y -> b@), whereas 'A.TypReturn' means the type annotation
  applies to the inner most function body (i.e., @b@). 'A.TypNone' means no type
  annotations need to be applied.

  Note that while 'A.TypProper' gives us enough information to unpack the
  arrow types and annotate all the intermediate lambdas, we punt that to the
  type checker. That is, @let f x : Int = b@ won't fail here, but it will/should
  be caught by the type checker.
  -}
  lambdaAnn, lambdaRetAnn :: AnnotationK
  (lambdaAnn, lambdaRetAnn) = case aTy of
    A.TypProper ty -> ((lowerType ty <>), id)
    A.TypReturn ty -> (id, (lowerType ty <>))
    A.TypNone      -> (id, id)

{- | Lowers an AST expression into an IR expression.

Performs the following desugaring inline:

- Desugars 'A.IfElse' to 'I.Match'
- Unrolls 'A.Constraint' to annotate sub-expressions

In particular, this function accepts a continuation @k :: (I.Type -> I.Type)@ to
represent type information known by the caller. The callee may apply this
continuation to locally known type information (typically none, i.e.,
'I.untyped') to produce a type annotation that can be embedded in an 'I.Expr'
node, or extend it with further type information (via function composition) to
propogate it elsewhere. This is possible because the IR's type annotations
'I.Type' form a monoid, where 'I.untyped' is the identity element and '(<>)' is
the join operation.
-}
lowerExpr :: A.Expr -> AnnotationK -> I.Expr I.Type
lowerExpr (A.Id v) k | isCons v  = I.Data (fromId v) (k I.untyped)
                     | otherwise = I.Var (fromId v) (k I.untyped)
lowerExpr (  A.Lit l    ) k = I.Lit (lowerLit l) (k I.untyped)
lowerExpr a@(A.Apply l r) k = case first lowerPrim (A.collectApp a) of
  (Just prim, args) -> I.Prim prim (map (`lowerExpr` id) args) (k I.untyped)
  (Nothing  , _   ) -> I.App (lowerExpr l id) (lowerExpr r id) (k I.untyped)
lowerExpr (A.Let ds b) k =
  I.Let (map lowerDef ds) (lowerExpr b id) (k I.untyped)
lowerExpr (A.Lambda ps b) k = lowerLambda ps b k id
lowerExpr (A.While  c  b) k = I.Prim I.Loop [body] (k I.untyped)
  where body = lowerExpr (A.IfElse c A.Break A.NoExpr `A.Seq` b) id
lowerExpr (A.Loop b ) k = I.Prim I.Loop [lowerExpr b id] (k I.untyped)
lowerExpr (A.Par  es) k = I.Prim I.Par (map (`lowerExpr` id) es) (k I.untyped)
lowerExpr (A.After delay lhs rhs) k =
  I.Prim I.After (map (`lowerExpr` id) [delay, lhs, rhs]) (k I.untyped)
lowerExpr (A.Assign lhs rhs) k =
  I.Prim I.Assign (map (`lowerExpr` id) [lhs, rhs]) (k I.untyped)
lowerExpr (A.Constraint e ty) k = lowerExpr e (k . (lowerType ty <>))
lowerExpr (A.Wait exprs) k =
  I.Prim I.Wait (map (`lowerExpr` id) exprs) (k I.untyped)
lowerExpr (A.Seq l r) k =
  I.Let [(Nothing, lowerExpr l id)] (lowerExpr r id) (k I.untyped)
lowerExpr A.Break        k = I.Prim I.Break [] (k I.untyped)
lowerExpr (A.Return e  ) k = I.Prim I.Return [lowerExpr e id] (k I.untyped)
lowerExpr (A.Match s ps) k = I.Match cond (fmap f ps) (k I.untyped)
 where
  cond = lowerExpr s id
  f (a, b) = (lowerAlt a, lowerExpr b id)
lowerExpr (A.IfElse c t e) k = I.Match cond [tArm, eArm] (k I.untyped)
 where
  cond = lowerExpr c id
  tArm = (I.AltLit (I.LitBool True), lowerExpr t id)
  eArm = (I.AltDefault Nothing, lowerExpr e id)
lowerExpr (A.OpRegion _ _) _ = error "Should already be desugared"
lowerExpr A.NoExpr         k = I.Lit I.LitEvent (k I.untyped)

-- | Lower an A.Pat into an I.Alt
lowerAlt :: A.Pat -> I.Alt
lowerAlt A.PatWildcard              = I.AltDefault Nothing
lowerAlt (A.PatId  d              ) = I.AltData (I.DConId d) []
lowerAlt (A.PatLit l              ) = I.AltLit $ lowerLit l
lowerAlt (A.PatTup _) = error "I.Alt for A.PatTup not implemented yet"
lowerAlt (A.PatApp (A.PatId d : t)) = I.AltData (I.DConId d)
                                                (lowerPatArg <$> t)
 where
  lowerPatArg :: A.Pat -> I.Binder
  lowerPatArg (A.PatId arg) = Just . I.VarId $ arg
  lowerPatArg A.PatWildcard = Nothing
  lowerPatArg _ = -- TODO: allow A.PatApp as an argument to A.PatApp
    error "currently only accept identifiers or wildcards as args to a PatApp"
lowerAlt (A.PatApp _  ) = error "this should never happen!"
lowerAlt (A.PatAnn _ p) = lowerAlt p
lowerAlt (A.PatAs  _ p) = lowerAlt p

{- | Unpack a list of patterns into nested (curried) lambdas.

This helper takes two arguments @lambdaAnn, lambdaRetAnn :: I.Type -> I.Type)@,
which represent annotations that should be applied to the whole lambda
expression and only the body of the lambda expression. These roughly correspond
to 'A.TypProper' and 'A.TypReturn' annotations.
-}
lowerLambda :: [A.Pat] -> A.Expr -> AnnotationK -> AnnotationK -> I.Expr I.Type
lowerLambda aPats aBody lambdaAnn lambdaRetAnn = lowerBinds aPats lambdaAnn
 where
  -- | Unpack a list of argument patterns into sequence of nested lambdas.
  lowerBinds :: [A.Pat] -> AnnotationK -> I.Expr I.Type
  lowerBinds (p : ps) k = I.Lambda lVar lBody lType
   where
    lVar  = lowerPatName p
    lBody = lowerBinds ps id
    lType = k $ lowerPatType p `I.arrow` extract lBody
  lowerBinds [] k = lowerExpr aBody (k . lambdaRetAnn)

-- | Lower an AST literal into an IR literal.
lowerLit :: A.Literal -> I.Literal
lowerLit (A.LitInt i)     = I.LitIntegral i
lowerLit A.LitEvent       = I.LitEvent
lowerLit (A.LitChar   _c) = error "Char literals are not yet implemented"
lowerLit (A.LitString _s) = error "String literals are not yet implemented"
lowerLit (A.LitRat    _r) = error "Rational literals are not yet implemented"

-- | Translate an AST identifier into the corresponding IR primitive, if any.
lowerPrim :: A.Expr -> Maybe I.Primitive
lowerPrim (A.Id "new"  ) = Just I.New
lowerPrim (A.Id "deref") = Just I.Deref
lowerPrim (A.Id "+"    ) = Just $ I.PrimOp I.PrimAdd
lowerPrim (A.Id "-"    ) = Just $ I.PrimOp I.PrimSub
lowerPrim (A.Id "*"    ) = Just $ I.PrimOp I.PrimMul
lowerPrim (A.Id "/"    ) = Just $ I.PrimOp I.PrimDiv
lowerPrim (A.Id "=="   ) = Just $ I.PrimOp I.PrimEq
lowerPrim (A.Id "!="   ) = Just $ I.PrimOp I.PrimNeq
lowerPrim (A.Id ">="   ) = Just $ I.PrimOp I.PrimGe
lowerPrim (A.Id "<="   ) = Just $ I.PrimOp I.PrimLe
lowerPrim (A.Id ">"    ) = Just $ I.PrimOp I.PrimGt
lowerPrim (A.Id "<"    ) = Just $ I.PrimOp I.PrimLt
lowerPrim _              = Nothing

-- | Extract an optional identifier from an Ast pattern.
lowerPatName :: A.Pat -> I.Binder
lowerPatName (A.PatId v)      = Just $ fromId v
lowerPatName A.PatWildcard    = Nothing
lowerPatName (A.PatAnn _ pat) = lowerPatName pat
lowerPatName _ = error "pattern should be desguared into pattern match"

-- | Extracts and lowers possible AST type annotation from a binding.
lowerPatType :: A.Pat -> I.Type
lowerPatType (A.PatAs _ b) = lowerPatType b
lowerPatType (A.PatTup bs) = I.tuple $ map lowerPatType bs
-- lowerPatType (A.PatCon _dcon) = error "need to perform DConId lookup"
lowerPatType (A.PatApp _ps) =
  error "FIXME: unsure how to lower applicative patterns"
lowerPatType (A.PatAnn typ p) = lowerType typ <> lowerPatType p
lowerPatType _                = I.untyped
