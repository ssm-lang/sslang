module Front.Ast where

import           Prelude                 hiding ( (<>)
                                                , id
                                                )
import           Prettyprinter

-- | A type variable name (e.g., a, b)
type TVarId = String

-- | A type constructor name (e.g., Int, Bool)
type TConId = String

-- | A data constructor name (e.g., Just, False)
type DConId = String

-- | A type class name (e.g., Eq, Ord)
type TClassId = String

-- | A variable name (e.g., x, y, f)
type VarId = String

-- | An operator name (e.g., +, foo)
type OperatorId = String

-- | A complete program: a list of declarations
newtype Program = Program [Definition]
  deriving Eq

-- | A value definition
data Definition
  = DefFn VarId [Pat] TypFn Expr
  | DefPat Pat Expr
  deriving (Eq, Show)

-- | A pattern appearing on the LHS of a definition or match arm
data Pat
  = PatWildcard         -- ^ Match anything, i.e., @_@
  | PatId VarId         -- ^ Bind value to variable, e.g., @v@
  | PatLit Literal      -- ^ Literal match, e.g., @1@
  | PatAs VarId Pat     -- ^ Pattern alias, e.g., @a \@ <pat>@
  | PatTup [Pat]        -- ^ Match on a tuple, e.g., @(<pat>, <pat>)@
  | PatCon DConId [Pat] -- ^ Match on data constructor, e.g., @Some <pat>@
  | PatAnn Typ Pat      -- ^ Match with type annotation, e.g., @<pat>: Type@
  deriving (Eq, Show)

-- | Function type annotation
data TypFn
  = TypReturn TypAnn
  | TypProper TypAnn
  | TypNone
  deriving (Eq, Show)

-- | TODO: type classes
type TypAnn = Typ

-- | A type definition
data Typ
  = TCon TConId
  | TApp Typ Typ
  | TTuple [Typ]
  | TArrow Typ Typ
  -- TODO type variables
  deriving (Eq, Show)

-- | An expression
data Expr
  = Id VarId
  | Lit Literal
  | Apply Expr Expr
  | OpRegion Expr OpRegion
  | NoExpr
  | Let [Definition] Expr
  | While Expr Expr
  | Loop Expr
  | Par [Expr]
  | IfElse Expr Expr Expr
  | After Expr Expr Expr
  | Assign Expr Expr
  | Constraint Expr TypAnn
  | Wait [Expr]
  | Seq Expr Expr
  | Break
  | Return Expr
  deriving (Eq, Show)

{- | An operator region: a flat list of alternating expressions and operators
that is initially parsed flat but will be restructured into a tree by
the operator precedence parser.
-}
data OpRegion
  = NextOp OperatorId Expr OpRegion
  | EOR
  deriving (Eq, Show)

-- | A literal
data Literal
  = LitInt Integer
  | LitString String
  | LitRat Rational
  | LitChar Char
  | LitEvent
  deriving (Eq, Show)

-- | Collect a curried application into the function and its list of arguments.
collectTApp :: Typ -> (Typ, [Typ])
collectTApp (TApp lhs rhs) = (lf, la ++ [rhs])
  where (lf, la) = collectTApp lhs
collectTApp t = (t, [])

collectApp :: Expr -> (Expr, [Expr])
collectApp (Apply lhs rhs) = (lf, la ++ [rhs])
  where (lf, la) = collectApp lhs
collectApp t = (t, [])

instance Show Program where
  show (Program defs) = concatMap (\df -> show (pretty df) ++ "\n\n") defs

instance Pretty Definition where
  pretty (DefFn id formals r body) = nest
    2
    (vsep
      [ pretty id <> tupled (map pretty formals) <+> pretty r <+> pretty '='
      , pretty body
      ]
    )
  pretty (DefPat b body) = pretty b <+> pretty "=" <+> pretty "{" <+> pretty body <+> pretty "}"

instance Pretty Pat where
  pretty PatWildcard   = pretty '_'
  pretty (PatId  s   ) = pretty s
  pretty (PatLit l   ) = pretty l
  pretty (PatAs b p  ) = pretty b <> pretty '@' <> pretty p -- TODO(hans): Parens around p?
  pretty (PatTup bs  ) = parens $ hsep (punctuate comma $ map pretty bs)
  pretty (PatCon dc pat) = pretty dc <+> parens (pretty pat)
  pretty (PatAnn ty pat) = pretty pat <+> pretty ":" <+> pretty ty 


instance Pretty TypFn where
  pretty (TypReturn t) = pretty "->" <+> pretty t
  pretty (TypProper t) = pretty ":" <+> pretty t
  pretty TypNone       = pretty ": ()" -- TODO(hans): What is this?

instance Pretty Typ where
  pretty (TCon id                   ) = pretty id
  pretty (TApp (TCon "[]") t2) = pretty "[" <> parens (pretty t2) <> pretty "]"
  pretty (TApp (TCon "&" ) t2       ) = pretty "&" <> parens (pretty t2)
  pretty (TApp t           (TCon id)) = pretty t <+> pretty id
  pretty (TApp t1          t2       ) = pretty t1 <+> parens (pretty t2)
  pretty (TTuple tys) = parens $ hsep (punctuate comma $ map pretty tys)
  pretty (TArrow t1 t2              ) = pretty t1 <+> pretty "->" <+> pretty t2


instance Pretty Expr where
  pretty (Id  id             ) = pretty id
  pretty (Lit l              ) = pretty l
  pretty (Apply    (Id id) e ) = pretty id <+> pretty e
  pretty (Apply    e1      e2) = parens (pretty e1) <+> pretty e2
  pretty (OpRegion e1      r ) = parens (pretty e1 <> p r)
   where
    p EOR             = emptyDoc
    p (NextOp s e r') = space <> pretty s <+> pretty e <> p r'
  pretty NoExpr = emptyDoc
  pretty (Let defs body) =
    vsep [pretty "let" <+> align (vsep $ map pretty defs), pretty body]
  pretty (While e1 e2) =
    nest 2 $ vsep [pretty "while" <+> pretty e1, pretty e2]
  pretty (Loop e ) = nest 2 $ vsep [pretty "loop", pretty e]
  pretty (Par  es) = nest 2 $ vsep $ pretty "par" : map pretty es
  pretty (IfElse e1 e2 NoExpr) =
    nest 2 $ vsep [pretty "if" <+> pretty e1, pretty e2]
  pretty (IfElse e1 e2 e3) = vsep
    [ nest 2 $ vsep [pretty "if" <+> pretty e1, pretty e2]
    , nest 2 $ vsep [pretty "else", pretty e3]
    ]
  pretty (After e1 v e2) =
    pretty "after"
      <+> pretty e1
      <+> pretty ","
      <+> pretty v
      <+> pretty "<-"
      <+> pretty e2
  pretty (Assign     v e) = pretty v <+> pretty "<-" <+> pretty e
  pretty (Constraint e t) = pretty e <+> pretty ':' <+> pretty t
  pretty (As         v e) = pretty v <> pretty '@' <> pretty e
  pretty (Wait vars) =
    pretty "wait" <+> hsep (punctuate comma $ map pretty vars)
  pretty (Seq e1 e2) = vsep [pretty e1, pretty e2]
  pretty Wildcard    = pretty '_'
  pretty Break       = pretty "break"
  pretty (Return e)  = pretty "return" <> pretty e

instance Pretty Literal where
  pretty (LitInt    i) = pretty i
  pretty (LitString s) = pretty '"' <> pretty s <> pretty '"'
  pretty (LitRat    r) = pretty $ show r
  pretty (LitChar   c) = pretty '\'' <> pretty c <> pretty '\''
  pretty LitEvent      = pretty "()"
