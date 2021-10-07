module Front.Lowering
  ( lowerProgram
  ) where

import qualified Front.Ast                     as A
import qualified IR.IR                         as L
import qualified Types.Ast                     as L
import qualified Types.TypeSystem              as L

import           Common.Identifiers             ( fromString )
import           Data.Composition               ( (.:) )
import           Data.Maybe                     ( fromJust )

todo, nope, what :: a
todo = error "TODO"
nope = error "Not going to implement this"
what = error "What does this even mean"

-- | Lower an AST 'Program' into IR.
lowerProgram :: A.Program -> L.Program L.Type
lowerProgram (A.Program ds) = L.Program { L.programEntry = todo
                                        , L.programDefs  = map lowerDecl ds
                                        , L.typeDefs     = [todo]
                                        }
-- | Lower a top-level 'Declaration' into triple of name, type, and definition.
lowerDecl :: A.Declaration -> (L.VarId, L.Expr L.Type)
lowerDecl (A.Function aName aBinds aBody aTy) = (lName, lBody)
 where
  lName = fromString aName
  lTy   = case aTy of
    A.CurriedType t -> lowerType t
    A.ReturnType  t -> foldr buildArrow (lowerType t) aBinds
      -- Construct a curried arrow type while folding through a list of binds
     where
      buildArrow =
        L.Type
          .  (: [])
          .  L.TBuiltin
          .: L.Arrow
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
  lowerBinds :: [A.Bind] -> A.Expr -> L.Type -> L.Expr L.Type
  lowerBinds (A.Bind v _ : bs) body ty = L.Lambda
    (Just $ fromString v)
    (lowerBinds bs body (snd $ fromJust $ L.dearrow ty))
    ty
  lowerBinds (A.TupBind _b _ty : _bs) _body _  = todo -- Need to desugar this to match
  lowerBinds []                       body  ty = lowerExpr body (<> ty)

-- | Lowers the AST's representation of types into that of the IR.
lowerType :: A.Ty -> L.Type
lowerType (  A.TCon i  ) = L.Type [L.TCon (fromString i) []]
lowerType a@(A.TApp _ _) = case A.collectTApp a of
  (A.TCon i, args) -> L.Type [L.TCon (fromString i) $ map lowerType args]
  _                -> nope
lowerType (A.TTuple tys) = L.Type [L.TBuiltin $ L.Tuple $ map lowerType tys]
lowerType (A.TArrow lhs rhs) =
  L.Type [L.TBuiltin $ L.Arrow (lowerType lhs) (lowerType rhs)]

-- | Lowers an AST expression into an IR expression. Performs desguaring inline.
lowerExpr :: A.Expr -> (L.Type -> L.Type) -> L.Expr L.Type
lowerExpr (A.Id      v) k = L.Var (fromString v) (k L.untyped)
lowerExpr (A.Literal l) k = L.Lit (lowerLit l) (k L.untyped)
lowerExpr (A.Apply l r) k = L.App lhs rhs (k L.untyped)
  where (lhs, rhs) = (lowerExpr l id, lowerExpr r id)
lowerExpr (A.Let ds b) k = L.Let defs body (k L.untyped)
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

lowerExpr (A.While c b) k = L.Prim L.Loop [body] (k L.untyped)
  where body = lowerExpr (A.IfElse c A.Break A.NoExpr `A.Seq` b) id
lowerExpr (A.Loop b ) k = L.Prim L.Loop [lowerExpr b id] (k L.untyped)
lowerExpr (A.Par  es) k = L.Prim L.Fork exprs (k L.untyped)
  where exprs = map (`lowerExpr` id) es
lowerExpr (A.IfElse c t e) k = L.Match cond Nothing [tArm, eArm] (k L.untyped)
 where
  cond = lowerExpr c id
  tArm = L.AltLit (L.LitBool True) (lowerExpr t id)
  eArm = L.AltDefault (lowerExpr e id)
lowerExpr (A.Later delay lhs rhs) k = L.Prim L.After args (k L.untyped)
  where args = map (`lowerExpr` id) [delay, todo lhs, rhs]
lowerExpr (A.Assign lhs rhs) k = L.Prim L.Assign args (k L.untyped)
  where args = map (`lowerExpr` id) [todo lhs, rhs]
lowerExpr (A.Constraint e ty) k = lowerExpr e ((<> lowerType ty) . k)
lowerExpr (A.Wait exprs     ) k = L.Prim L.Wait args (k L.untyped)
  where args = map (`lowerExpr` id) exprs
lowerExpr (A.Seq l r) k = L.Let [(Nothing, lhs)] rhs (k L.untyped)
  where (lhs, rhs) = (lowerExpr l id, lowerExpr r id)
lowerExpr A.Break          k = L.Prim L.Break [] (k L.untyped)
lowerExpr (A.Return e    ) k = L.Prim L.Return [lowerExpr e id] (k L.untyped)
lowerExpr (A.OpRegion _ _) _ = error "Should already be desugared"
lowerExpr A.NoExpr         _ = what -- Perhaps this should be a unit literal?
lowerExpr A.Wildcard       _ = what
lowerExpr (A.As _ _)       _ = what

-- | Lower an AST literal into an IR literal.
lowerLit :: A.Lit -> L.Literal
lowerLit (A.IntLit    i ) = L.LitIntegral i
lowerLit (A.StringLit _s) = todo
lowerLit (A.RatLit    _r) = todo
lowerLit (A.CharLit   _c) = todo
