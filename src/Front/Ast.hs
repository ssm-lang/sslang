module Front.Ast where

import Prettyprinter
import Prelude hiding ( (<>), id )

-- | A type variable name (e.g., a, b)
type TVarId = String

-- | A type constructor name (e.g., Int, Bool)
type TConId = String

-- | A type class name (e.g., Eq, Ord)
type TClassId = String

-- | A variable name (e.g., x, y, f)
type VarId = String

-- | An operator name (e.g., +, foo)
type OperatorId = String

data Program = Program [Declaration]
  deriving (Eq, Show)

data FnTyAnnotation = ReturnType Ty
                    | CurriedType Ty
  deriving (Eq, Show)

data Declaration = Function VarId [Bind] Expr FnTyAnnotation
  deriving (Eq, Show)

data Bind = Bind VarId (Maybe Ty)
          | TupBind [Bind] (Maybe Ty)
  deriving (Eq, Show)

data Ty = TCon TConId
        | TApp Ty Ty
        | TTuple [Ty]
        | TArrow Ty Ty
  deriving (Eq, Show)

data Lit = IntLit Integer
         | StringLit String
         | RatLit Rational
         | CharLit Char
         | EventLit
  deriving (Eq, Show)

data Expr = Id VarId
          | Literal Lit
          | Apply Expr Expr
          | OpRegion Expr OpRegion
          | NoExpr
          | Let [Def] Expr
          | While Expr Expr
          | Loop Expr
          | Par [Expr]
          | IfElse Expr Expr Expr
          | After Expr Expr Expr
          | Assign Expr Expr
          | Constraint Expr Ty
          | As VarId Expr
          | Wait [Expr]
          | New Expr
          | Seq Expr Expr
          | Wildcard
          | Break
          | Return Expr
  deriving (Eq, Show)

data OpRegion = EOR
              | NextOp OperatorId Expr OpRegion
  deriving (Eq, Show)

data Def = Def Pat Expr
  deriving (Eq, Show)

data Pat = PId VarId
         | PLiteral Lit
         | PWildcard
         | PAs VarId Pat
         | PCon TConId [Pat]
  deriving (Eq, Show)

-- | TODO: document
collectTApp :: Ty -> (Ty, [Ty])
collectTApp (TApp lhs rhs) = (lf, la ++ [rhs])
  where (lf, la) = collectTApp lhs
collectTApp t = (t, [])

instance Pretty Declaration where
  pretty (Function id formals body r) =
    let ret = (case r of ReturnType t -> pretty "->" <+> pretty t
                         CurriedType t -> pretty ":" <+> pretty t) in
    nest 2 (vsep [ pretty id <> tupled (map pretty formals) <+> ret <+> pretty '='
                 , pretty body ])

instance Pretty Bind where
  pretty (Bind id mty) = let prettyId = pretty id in
                         case mty of Just ty -> prettyId <+> pretty ':' <+> pretty ty
                                     Nothing -> prettyId
  pretty (TupBind binds mty) = let prettyTup = hsep (punctuate comma $ map pretty binds) in
                               case mty of Just ty -> parens prettyTup <+> pretty ':' <+> pretty ty
                                           Nothing -> prettyTup

instance Pretty Ty where
  pretty (TCon id) = pretty id
  pretty (TApp (TCon "[]") t2) = pretty "[" <> parens (pretty t2) <> pretty "]"
  pretty (TApp (TCon "&") t2) = pretty "&" <> parens (pretty t2)
  pretty (TApp t (TCon id)) = pretty t <+> pretty id
  pretty (TApp t1 t2) = pretty t1 <+> parens (pretty t2)
  pretty (TTuple tys) = pretty "(" <> hsep (punctuate comma $ map pretty tys) <> pretty ")"
  pretty (TArrow t1 t2) = pretty t1 <+> pretty "->" <+> pretty t2

instance Pretty Lit where
  pretty (IntLit i) = pretty i
  pretty (StringLit s) = pretty '"' <> pretty s <> pretty '"'
  pretty (RatLit r) = pretty $ show r
  pretty (CharLit c) = pretty '\'' <> pretty c <> pretty '\''
  pretty EventLit = pretty "()"

instance Pretty Expr where
  pretty (Id id) = pretty id
  pretty (Literal l) = pretty l
  pretty (Apply (Id id) e) = pretty id <+> pretty e
  pretty (Apply e1 e2) = parens (pretty e1) <+> pretty e2
  pretty (OpRegion e1 r) = parens (pretty e1 <> p r)
    where p EOR = emptyDoc
          p (NextOp s e r') = space <> pretty s <+> pretty e <> p r'
  pretty (As v e) = pretty v <> pretty '@' <> pretty e
  pretty NoExpr = emptyDoc
  pretty (Let defs body) =
    vsep [pretty "let" <+> align (vsep $ map pretty defs), pretty body]
  pretty (While e1 e2) = nest 2 $ vsep [ pretty "while" <+> pretty e1, pretty e2 ]
  pretty (Loop e) = nest 2 $ vsep [ pretty "loop", pretty e ]
  pretty (Par es) = nest 2 $ vsep $ pretty "par" : map pretty es
  pretty (IfElse e1 e2 NoExpr) = nest 2 $ vsep [ pretty "if" <+> pretty e1
                                              , pretty e2 ]
  pretty (IfElse e1 e2 e3) = vsep [ nest 2 $ vsep [ pretty "if" <+> pretty e1
                                                  , pretty e2 ]
                                  , nest 2 $ vsep [ pretty "else"
                                                  , pretty e3 ] ]
  pretty (After e1 v e2) = pretty "after" <+> pretty e1 <+> pretty "," <+>
                           pretty v <+> pretty "<-" <+> pretty e2
  pretty (Assign v e) = pretty v <+> pretty "<-" <+> pretty e
  pretty (Wait vars) =
      pretty "wait" <+> hsep (punctuate comma $ map pretty vars)
  pretty (New e) = pretty "new" <+> pretty e
  pretty (Constraint e t) = pretty e <+> pretty ':' <+> pretty t
  pretty (Seq e1 e2) = vsep [pretty e1, pretty e2]
  pretty Wildcard = pretty '_'
  pretty Break = pretty "break"
  pretty (Return e) = pretty "return" <> pretty e

instance Pretty Def where
  pretty (Def p e) = pretty p <+> pretty '=' <+> pretty e

instance Pretty Pat where
  pretty (PId s) = pretty s
  pretty (PLiteral l) = pretty l
  pretty PWildcard = pretty '_'
  pretty (PAs v p) = pretty v <> pretty '@' <> pretty p
  pretty (PCon c ps) = pretty c <+> hsep (map pretty ps)
