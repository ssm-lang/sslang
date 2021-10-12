module IR.LowerAst
  ( lowerProgram
  ) where

import qualified Front.Ast                     as A

import qualified IR.IR                         as I
import qualified IR.Types.Annotated            as I
import qualified IR.Types.TypeSystem           as I

import           Common.Identifiers             ( fromString )
import           Data.Composition               ( (.:) )
import           Data.Maybe                     ( fromJust )

todo, nope, what :: a
todo = error "TODO"
nope = error "Not going to implement this"
what = error "What does this even mean"

-- | Lower an AST 'Program' into IR.
lowerProgram :: A.Program -> I.Program I.Type
lowerProgram (A.Program ds) = I.Program { I.programEntry = todo
                                        , I.programDefs  = map lowerDecl ds
                                        , I.typeDefs     = [todo]
                                        }
-- | Lower a top-level 'Declaration' into triple of name, type, and definition.
lowerDecl :: A.Declaration -> (I.VarId, I.Expr I.Type)
lowerDecl (A.Function aName aBinds aBody aTy) = (lName, lBody)
 where
  lName = fromString aName
  lTy   = case aTy of
    A.CurriedType t -> lowerType t
    A.ReturnType  t -> foldr buildArrow (lowerType t) aBinds
      -- Construct a curried arrow type while folding through a list of binds
     where
      buildArrow =
        I.Type
          .  (: [])
          .  I.TBuiltin
          .: I.Arrow
          .  lowerType
          .  fromJust
          .  bindType
  lBody = lowerBinds aBinds aBody lTy

  -- | Extracts possible AST type annotation from a binding.
  bindType :: A.Bind -> Maybe A.Ty
  bindType (A.Bind    _    (Just ty)) = return ty
  bindType (A.TupBind _    (Just ty)) = return ty
  bindType (A.TupBind tups Nothing  ) = A.TTuple <$> mapM bindType tups
  bindType _                          = Nothing

  {- | Lowers function args and body into nested lambdas.

  As this function unpacks the bindings, it also unpacks the outer function's
  type annotation to appropriate annotate each sub-lambda.
  -}
  lowerBinds :: [A.Bind] -> A.Expr -> I.Type -> I.Expr I.Type
  lowerBinds (A.Bind v _ : bs) body ty = I.Lambda
    (Just $ fromString v)
    (lowerBinds bs body (snd $ fromJust $ I.dearrow ty))
    ty
  lowerBinds (A.TupBind _b _ty : _bs) _body _  = todo -- Need to desugar this to match
  lowerBinds []                       body  ty = lowerExpr body (<> ty)

-- | Lowers the AST's representation of types into that of the IR.
lowerType :: A.Ty -> I.Type
lowerType (  A.TCon i  ) = I.Type [I.TCon (fromString i) []]
lowerType a@(A.TApp _ _) = case A.collectTApp a of
  (A.TCon i, args) -> I.Type [I.TCon (fromString i) $ map lowerType args]
  _                -> nope
lowerType (A.TTuple tys) = I.Type [I.TBuiltin $ I.Tuple $ map lowerType tys]
lowerType (A.TArrow lhs rhs) =
  I.Type [I.TBuiltin $ I.Arrow (lowerType lhs) (lowerType rhs)]

-- | Lowers an AST expression into an IR expression. Performs desguaring inline.
lowerExpr :: A.Expr -> (I.Type -> I.Type) -> I.Expr I.Type
lowerExpr (A.Id      v) k = I.Var (fromString v) (k I.untyped)
lowerExpr (A.Literal l) k = I.Lit (lowerLit l) (k I.untyped)
lowerExpr (A.Apply l r) k = I.App lhs rhs (k I.untyped)
  where (lhs, rhs) = (lowerExpr l id, lowerExpr r id)
lowerExpr (A.Let ds b) k = I.Let defs body (k I.untyped)
 where
  (defs, body) = (map lowerLet ds, lowerExpr b id)
  -- | Accumulator to lower an AST let-binding into an IR let-binding.
  lowerLet (A.Def (A.PId n)       d ) = (Just $ fromString n, lowerExpr d id)
  lowerLet (A.Def A.PWildcard     e ) = (Nothing, lowerExpr e id)
  lowerLet (A.Def (A.PAs  _n _p ) _e) = todo
    {- This is tricky because something like
      @
        let p'' = d''
            n@p = d
            p' = d'
        in b
      @

      is desugared into

      @
        let p'' = let p = n in d''
            n = d
            p' = let p = n in d'
        in let p = n in b
      @

      To account for the alias being used in co-recursive definitions as well as
      the let-body. This desugaring should be done somewhere, but probably not
      here.
    -}
  lowerLet (A.Def (A.PCon _n _ps) _ ) = what -- Why is n a TConId?
  lowerLet (A.Def (A.PLiteral _l) _e) = what

lowerExpr (A.While c b) k = I.Prim I.Loop [body] (k I.untyped)
  where body = lowerExpr (A.IfElse c A.Break A.NoExpr `A.Seq` b) id
lowerExpr (A.Loop b ) k = I.Prim I.Loop [lowerExpr b id] (k I.untyped)
lowerExpr (A.Par  es) k = I.Prim I.Fork exprs (k I.untyped)
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
lowerExpr A.NoExpr         _ = what -- Perhaps this should be a unit literal?
lowerExpr A.Wildcard       _ = what
lowerExpr (A.As _ _)       _ = what

-- | Lower an AST literal into an IR literal.
lowerLit :: A.Lit -> I.Literal
lowerLit (A.IntLit    i ) = I.LitIntegral i
lowerLit (A.StringLit _s) = todo
lowerLit (A.RatLit    _r) = todo
lowerLit (A.CharLit   _c) = todo
