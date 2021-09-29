module Common.IR
  ( Program(..)
  , Binder
  , Literal(..)
  , PrimOp(..)
  , Expr(..)
  , Alt(..)
  , typeExpr
  , VarId(..)
  , DConId(..)
  ) where
import           Common.Identifiers             ( Binder
                                                , DConId(..)
                                                , VarId(..)
                                                )
import           Common.Types                   ( TypeSystem(..) )

-- | Top-level compilation unit.
data Program t = Program
  { programEntry :: VarId
  , programDefs  :: [(VarId, Expr t)]
  }

{- | Literal values supported by the language.

Note that these don't carry any connotation of type: '1' just means '1', 
-}
data Literal
  = LitIntegral Int
  | LitBool Bool

{- | Primitive operations.

These should be the kinds of functions one may expect to be available as
operators in C, or as instructions in an assembly language.

For simplicity and consistency, they should be:

- Strict in all operands (i.e., no '&&')
- Pure (i.e., side-effectful iff operands are side-effectful, i.e., no '=')

-}
data PrimOp
  = PrimAdd
  | PrimSub
  | PrimMul
  | PrimDiv
  | PrimMod
  | PrimNeg
  | PrimBitAnd
  | PrimBitOr
  | PrimBitNot
  | PrimEq
  | PrimNot
  | PrimGt
  | PrimGe
  | PrimLt
  | PrimLe

-- | The type of expressions.
--
-- t represents the type of this expression. At various stages, this may
-- represent a richer or simpler type system.
--
-- Note that sequencing should be performed using chained let-expressoins:
--
-- let _ = stmt1 in
-- let _ = stmt2 in
-- ...
data Expr t
  = Var VarId t
  | Data DConId t
  | Lit Literal t
  | App { appFn :: Expr t, appArg :: Expr t, appType :: t }
  | Let { letDefs :: [(Binder, Expr t)], letBody :: Expr t }
  | Lambda { lambdaBinder :: Binder, lambdaArgType :: t , lambdaBody :: Expr t }
  | Match { scrutinee :: Expr t
          , scrutBinder :: Binder
          , matchArms :: [Alt t]
          , matchType :: t
          }
  | New (Expr t)
  | Deref { derefArg :: Expr t, derefType :: t }
  | Assign { assignLhs :: Expr t, assignRhs :: Expr t }
  | Later { laterTime:: Expr t , laterLhs :: Expr t, laterRhs :: Expr t }
  | Fork [Expr t]
  | Wait [Expr t]
  | Loop (Expr t)
  | Break
  | Return -- TODO: should this take an expression?
  | PrimApp { primOp :: PrimOp, primArgs :: [Expr t], primType :: t }

data Alt t
  = AltData DConId [Binder] (Expr t)
  | AltLit Literal (Expr t)
  | AltDefault (Expr t)

typeExpr :: TypeSystem t => Expr t -> t
typeExpr (Var  _ t)               = t
typeExpr (Data _ t)               = t
typeExpr (Lit  _ t)               = t
typeExpr Let { letBody = b }      = typeExpr b
typeExpr Lambda { lambdaArgType = a, lambdaBody = b } = a `arrow` typeExpr b
typeExpr App { appType = t }      = t
typeExpr (Match _ _ _ t)          = t
typeExpr Break                    = void
typeExpr Return                   = void
typeExpr Loop{}                   = unit
typeExpr Fork{}                   = unit
typeExpr Wait{}                   = unit
typeExpr Assign{}                 = unit
typeExpr (New e)                  = ref $ typeExpr e
typeExpr Deref { derefType = t }  = t
typeExpr Later{}                  = unit
typeExpr PrimApp { primType = t } = t
