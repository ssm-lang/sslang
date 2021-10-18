module IR.LowerAst
  ( lowerProgram
  ) where

import qualified Common.Compiler               as Compiler

import qualified Front.Ast                     as A

import qualified IR.IR                         as I
import qualified IR.Types.Annotated            as I
import qualified IR.Types.TypeSystem           as I

import           Common.Identifiers             ( fromString )

import           Data.Bifunctor                 ( Bifunctor(..) )
import           Data.Maybe                     ( fromJust )

todo, nope, what :: a
todo = error "TODO"
nope = error "Not going to implement this"
what = error "What does this even mean"

-- | Lower an AST 'Program' into IR.
lowerProgram :: A.Program -> Compiler.Pass (I.Program I.Type)
lowerProgram (A.Program ds) = return $ I.Program
  { I.programEntry = fromString "main"
  , I.programDefs  = map (first fromJust . lowerDef) ds
  , I.typeDefs     = [todo]
  }

-- | Lower a top-level 'Declaration' into triple of name, type, and definition.
lowerDef :: A.Definition -> (I.Binder, I.Expr I.Type)
lowerDef (A.DefPat (A.Bind (A.BindId bName) bTy) aBody) =
  (Just $ fromString bName, lowerExpr aBody (mconcat (map lowerType bTy) <>))
lowerDef (A.DefPat (A.Bind A.BindWildcard bTy) aBody) =
  (Nothing, lowerExpr aBody (mconcat (map lowerType bTy) <>))
lowerDef (A.DefPat _ _) = nope -- Should be desugared into match
lowerDef (A.DefFn aName aBinds aTy aBody) =
  (Just $ fromString aName, lowerBinds aBinds headAnn)
 where
  {- | Where 'A.TypFn' type annotation should be applied.

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

  lowerBinds :: [A.Bind] -> (I.Type -> I.Type) -> I.Expr I.Type
  lowerBinds (b@(A.Bind (A.BindId v) _) : bs) k =
    I.Lambda (Just $ fromString v) (lowerBinds bs id) (k $ lowerBindType b)
  lowerBinds (b@(A.Bind A.BindWildcard _) : bs) k =
    I.Lambda Nothing (lowerBinds bs id) (k $ lowerBindType b)
  lowerBinds [] k = lowerExpr aBody (k . tailAnn)
  lowerBinds _  _ = nope -- Should have been desugared into pattern matches

-- | Extracts and lowers possible AST type annotation from a binding.
lowerBindType :: A.Bind -> I.Type
lowerBindType (A.Bind (A.BindAs _ b) tys) =
  lowerBindType b <> mconcat (map lowerType tys)
lowerBindType (A.Bind (A.BindTup bs) tys) =
  I.tuple (map lowerBindType bs) <> mconcat (map lowerType tys)
lowerBindType (A.Bind (A.BindCon _ _bs) _tys) = nope -- should be desugared to match
lowerBindType (A.Bind _                 tys ) = mconcat $ map lowerType tys

-- | Lowers the AST's representation of types into that of the IR.
lowerType :: A.Typ -> I.Type
lowerType (  A.TCon "Int") = I.int 32
lowerType (  A.TCon "()" ) = I.unit
lowerType (  A.TCon i    ) = I.Type [I.TCon (fromString i) []]
lowerType a@(A.TApp _ _  ) = case A.collectTApp a of
  (A.TCon "&" , [arg] ) -> I.ref $ lowerType arg
  (A.TCon "[]", [_arg]) -> todo
  (A.TCon i   , args  ) -> I.Type [I.TCon (fromString i) $ map lowerType args]
  _                     -> nope
lowerType (A.TTuple tys    ) = I.tuple $ map lowerType tys
lowerType (A.TArrow lhs rhs) = lowerType lhs `I.arrow` lowerType rhs

-- | Lowers an AST expression into an IR expression. Performs desguaring inline.
lowerExpr :: A.Expr -> (I.Type -> I.Type) -> I.Expr I.Type
lowerExpr (A.Id  v    ) k = I.Var (fromString v) (k I.untyped)
lowerExpr (A.Lit l    ) k = I.Lit (lowerLit l) (k I.untyped)
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
lowerExpr (A.New e  ) k = I.Prim I.New [lowerExpr e id] (k I.untyped)
lowerExpr (A.Seq l r) k = I.Let [(Nothing, lhs)] rhs (k I.untyped)
  where (lhs, rhs) = (lowerExpr l id, lowerExpr r id)
lowerExpr A.Break          k = I.Prim I.Break [] (k I.untyped)
lowerExpr (A.Return e    ) k = I.Prim I.Return [lowerExpr e id] (k I.untyped)
lowerExpr (A.OpRegion _ _) _ = error "Should already be desugared"
lowerExpr A.NoExpr         _ = what -- Perhaps this should be a unit literal?
lowerExpr A.Wildcard       _ = what
lowerExpr (A.As _ _)       _ = what

-- | Lower an AST literal into an IR literal.
lowerLit :: A.Literal -> I.Literal
lowerLit (A.LitInt i)     = I.LitIntegral i
lowerLit A.LitEvent       = I.LitEvent
lowerLit (A.LitString _s) = todo
lowerLit (A.LitRat    _r) = todo
lowerLit (A.LitChar   _c) = todo
