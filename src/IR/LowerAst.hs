module IR.LowerAst
  ( lowerProgram
  ) where

import qualified Common.Compiler               as Compiler

import qualified Front.Ast                     as A

import qualified IR.IR                         as I
import qualified IR.Types.Annotated            as I
import qualified IR.Types.TypeSystem           as I

import           Common.Identifiers             ( fromString )

import           Control.Comonad                ( Comonad(..) )
import           Data.Bifunctor                 ( Bifunctor(..) )
import           Data.Maybe                     ( fromJust )

-- | Lower an AST 'Program' into IR.
lowerProgram :: A.Program -> Compiler.Pass (I.Program I.Type)
lowerProgram (A.Program ds) = return $ I.Program
  { I.programEntry = fromString "main"
  , I.programDefs  = map (first fromJust . lowerDef) ds
  , I.typeDefs     = [error "Typedefs are not yet implemented"]
  }

-- | Lower a top-level 'Declaration' into triple of name, type, and definition.
lowerDef :: A.Definition -> (I.Binder, I.Expr I.Type)
lowerDef (A.DefPat aPat aBody) =
  (lowerPatName aPat, lowerExpr aBody (lowerPatType aPat <>))
lowerDef (A.DefFn aName aBinds aTy aBody) =
  (Just $ fromString aName, lowerBinds aBinds headAnn)
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
  (headAnn, tailAnn) = case aTy of
    A.TypProper ty -> ((lowerType ty <>), id)
    A.TypReturn ty -> (id, (lowerType ty <>))
    A.TypNone      -> (id, id)

  lowerBinds :: [A.Pat] -> (I.Type -> I.Type) -> I.Expr I.Type
  lowerBinds (p : ps) k = I.Lambda lVar lBody lType
   where
    lVar  = lowerPatName p
    lBody = lowerBinds ps id
    lType = k $ lowerPatType p `I.arrow` extract lBody
  lowerBinds [] k = lowerExpr aBody (k . tailAnn)

lowerPatName :: A.Pat -> I.Binder
lowerPatName (A.PatId v)      = Just $ fromString v
lowerPatName A.PatWildcard    = Nothing
lowerPatName (A.PatAnn _ pat) = lowerPatName pat
lowerPatName _ = error "pattern should be desguared into pattern match"

-- | Extracts and lowers possible AST type annotation from a binding.
lowerPatType :: A.Pat -> I.Type
lowerPatType (A.PatAs _ b     ) = lowerPatType b
lowerPatType (A.PatTup bs     ) = I.tuple $ map lowerPatType bs
lowerPatType (A.PatCon _   _bs) = error "need to perform DConId lookup"
lowerPatType (A.PatAnn typ p  ) = lowerType typ <> lowerPatType p
lowerPatType _                  = mempty

-- | Lowers the AST's representation of types into that of the IR.
lowerType :: A.Typ -> I.Type
lowerType (  A.TCon "Int") = I.int 32
lowerType (  A.TCon "()" ) = I.unit
lowerType (  A.TCon i    ) = I.Type [I.TCon (fromString i) []]
lowerType a@(A.TApp _ _  ) = case A.collectTApp a of
  (A.TCon "&" , [arg] ) -> I.ref $ lowerType arg
  (A.TCon "[]", [_arg]) -> error "list types are not yet implemented"
  (A.TCon i   , args  ) -> I.Type [I.TCon (fromString i) $ map lowerType args]
  _                     -> error $ "Cannot lower higher-kinded type: " ++ show a
lowerType (A.TTuple tys    ) = I.tuple $ map lowerType tys
lowerType (A.TArrow lhs rhs) = lowerType lhs `I.arrow` lowerType rhs

-- | Lowers an AST expression into an IR expression. Performs desguaring inline.
lowerExpr :: A.Expr -> (I.Type -> I.Type) -> I.Expr I.Type
lowerExpr (A.Id  v) k = I.Var (fromString v) (k I.untyped)
lowerExpr (A.Lit l) k = I.Lit (lowerLit l) (k I.untyped)
lowerExpr a@(A.Apply _ _) k | fst (A.collectApp a) == A.Id "new"   = primNew
                            | fst (A.collectApp a) == A.Id "deref" = primDeref
                            | fst (A.collectApp a) == A.Id "(-)"   = primSub
 where
  args      = map (`lowerExpr` id) $ snd $ A.collectApp a
  primNew   = I.Prim I.New args (k I.untyped)
  primDeref = I.Prim I.Deref args (k I.untyped)
  primSub   = I.Prim (I.PrimOp I.PrimSub) args (k I.untyped)
lowerExpr (A.Apply l r) k = I.App lhs rhs (k I.untyped)
  where (lhs, rhs) = (lowerExpr l id, lowerExpr r id)
lowerExpr (A.Let ds b) k = I.Let defs body (k I.untyped)
  where (defs, body) = (map lowerDef ds, lowerExpr b id)
lowerExpr (A.While c b) k = I.Prim I.Loop [body] (k I.untyped)
  where body = lowerExpr (A.IfElse c A.Break A.NoExpr `A.Seq` b) id
lowerExpr (A.Loop b ) k = I.Prim I.Loop [lowerExpr b id] (k I.untyped)
lowerExpr (A.Par  es) k = I.Prim I.Par exprs (k I.untyped)
  where exprs = map (`lowerExpr` id) es
lowerExpr (A.IfElse c t e) k = I.Match cond Nothing [tArm, eArm] (k I.untyped)
 where
  cond = lowerExpr c id
  tArm = (I.AltLit (I.LitBool True), lowerExpr t id)
  eArm = (I.AltDefault, lowerExpr e id)
lowerExpr (A.After delay lhs rhs) k = I.Prim I.After args (k I.untyped)
  where args = map (`lowerExpr` id) [delay, lhs, rhs]
lowerExpr (A.Assign lhs rhs) k = I.Prim I.Assign args (k I.untyped)
  where args = map (`lowerExpr` id) [lhs, rhs]
lowerExpr (A.Constraint e ty) k = lowerExpr e ((<> lowerType ty) . k)
lowerExpr (A.Wait exprs     ) k = I.Prim I.Wait args (k I.untyped)
  where args = map (`lowerExpr` id) exprs
lowerExpr (A.Seq l r) k = I.Let [(Nothing, lhs)] rhs (k I.untyped)
  where (lhs, rhs) = (lowerExpr l id, lowerExpr r id)
lowerExpr A.Break          k = I.Prim I.Break [] (k I.untyped)
lowerExpr (A.Return e    ) k = I.Prim I.Return [lowerExpr e id] (k I.untyped)
lowerExpr (A.OpRegion _ _) _ = error "Should already be desugared"
lowerExpr A.NoExpr         k = I.Lit I.LitEvent (k I.untyped)

-- | Lower an AST literal into an IR literal.
lowerLit :: A.Literal -> I.Literal
lowerLit (A.LitInt i)     = I.LitIntegral i
lowerLit A.LitEvent       = I.LitEvent
lowerLit (A.LitChar   _c) = error "Char literals are not yet implemented"
lowerLit (A.LitString _s) = error "String literals are not yet implemented"
lowerLit (A.LitRat    _r) = error "Rational literals are not yet implemented"
