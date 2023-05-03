{-# LANGUAGE DeriveDataTypeable #-}
-- | Sslang abstract syntax tree.
module Front.Ast where

import           Common.Identifiers             ( Identifiable(..)
                                                , Identifier
                                                )
import           Common.Pretty

import           Data.Generics
import           Data.List                      ( intersperse )
import           Data.Maybe                     ( mapMaybe )

-- | A complete program: a list of top-level definitions.
newtype Program = Program [TopDef]
  deriving (Eq, Show, Typeable, Data)

-- | A top-level definition.
data TopDef
  = TopDef Definition     -- ^ Bind a (data) value to a variable
  | TopType TypeDef       -- ^ Define an algebraic data type
  | TopCDefs String       -- ^ Inlined block of C definitions
  | TopExtern ExternDecl  -- ^ Declare external symbol for FFI
  | TopImport Import   -- ^ Top Level import statement
  deriving (Eq, Show, Typeable, Data)

data Import =
     ImportSym [Identifier]
   | ImportAs [Identifier] Identifier
   | ImportWith [Identifier] [ImportElement]
  deriving (Eq, Show, Typeable, Data)


data ImportElement
  = ElementAs Identifier Identifier
  | ElementSymbol Identifier
 deriving (Eq, Show, Typeable, Data)

-- | Associate a type with a symbol
data ExternDecl = ExternDecl Identifier Typ
  deriving (Eq, Show, Typeable, Data)

-- | An algebraic data type definition.
data TypeDef = TypeDef
  { typeName     :: Identifier      -- ^ The name of the type, e.g., @Option@
  , typeParams   :: [Identifier]    -- ^ List of type parameters, e.g., @a@
  , typeVariants :: [TypeVariant]   -- ^ List of variants, e.g., @Some@, @None@
  }
  deriving (Eq, Show, Typeable, Data)

-- | A type variant, i.e., a data constructor.
data TypeVariant = VariantUnnamed Identifier [Typ]
  deriving (Eq, Show, Typeable, Data)

-- | A value definition.
data Definition
  = DefFn Identifier [Pat] TypFn Expr
  | DefPat Pat Expr
  deriving (Eq, Show, Typeable, Data)

-- | A pattern appearing on the LHS of a definition or match arm
data Pat
  = PatWildcard           -- ^ Match anything, i.e., @_@
  | PatId Identifier      -- ^ Variable or data constructor, e.g., @v@ or @Some@
  | PatLit Literal        -- ^ Literal match, e.g., @1@
  | PatAs Identifier Pat  -- ^ Pattern alias, e.g., @a \@ <pat>@
  | PatTup [Pat]          -- ^ Match on a tuple, e.g., @(<pat>, <pat>)@
  | PatApp [Pat]          -- ^ Match on multiple patterns, e.g., @Some a@
  | PatAnn Typ Pat        -- ^ Match with type annotation, e.g., @<pat>: Type@
  deriving (Eq, Show, Typeable, Data)

-- | Function type annotation
data TypFn
  = TypReturn TypAnn
  | TypProper TypAnn
  | TypNone
  deriving (Eq, Show, Typeable, Data)

-- | TODO: type classes
type TypAnn = Typ

-- | A type definition
data Typ
  = TCon Identifier
  | TApp Typ Typ
  | TTuple [Typ]
  | TArrow Typ Typ
  -- TODO type variables
  deriving (Eq, Show, Typeable, Data)

-- | An expression
data Expr
  = Id Identifier
  | Lit Literal
  | Apply Expr Expr
  | Lambda [Pat] Expr
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
  | Match Expr [(Pat, Expr)]
  | CQuote String
  | CCall Identifier [Expr]
  | Tuple [Expr]
  | ListExpr [Expr]
  | ImportId [Identifier]
  deriving (Eq, Show, Typeable, Data)

{- | An operator region: a flat list of alternating expressions and operators
that is initially parsed flat but will be restructured into a tree by
the operator precedence parser.
-}
data OpRegion
  = NextOp Identifier Expr OpRegion
  | EOR
  deriving (Eq, Show, Typeable, Data)

-- | A literal
data Literal
  = LitInt Integer
  | LitString String
  | LitRat Rational
  | LitChar Char
  | LitEvent
  deriving (Eq, Show, Typeable, Data)

-- | Fixity declaration for binary operators.
data Fixity = Infixl Int Identifier
            | Infixr Int Identifier

{- | Apply a function to zero or more arguments.

Suppose we have as source code
@
type Color = RGB Int Int Int
...
    let x = RGB 203 200 100
@
and rgb = Id "RGB"
    r = Lit (A.LitInt 203)
    g = Lit (A.LitInt 200)
    b = Lit (A.LitInt 100),
then foldApp rgb [r, g, b] returns
@
Apply (Apply (Apply (Id RGB) (Lit (LitInt 100))) (Lit (LitInt 200))) (Lit (LitInt 203))
@
-}
foldApp :: Expr -> [Expr] -> Expr
foldApp = foldr $ \a f -> Apply f a

-- | Collect a type application into the type constructor and its arguments.
collectTApp :: Typ -> (Typ, [Typ])
collectTApp (TApp lhs rhs) = (lf, la ++ [rhs])
  where (lf, la) = collectTApp lhs
collectTApp t = (t, [])

-- | Collect a curried application into the function and its list of arguments.
collectApp :: Expr -> (Expr, [Expr])
collectApp (Apply lhs rhs) = (lf, la ++ [rhs]) where (lf, la) = collectApp lhs
collectApp t               = (t, [])

-- | Collect a pattern application into the destructor and arguments.
collectPApp :: Pat -> (Pat, [Pat])
collectPApp (PatApp (p : ps)) = (p, ps)
collectPApp p                 = (p, []) -- Note that @PatApp []@ is probably malformed!

-- | Unwrap a (potential) top-level data definition.
getTopDataDef :: TopDef -> Maybe Definition
getTopDataDef (TopDef d) = Just d
getTopDataDef _          = Nothing

-- | Unwrap a (potential) top-level type definition.
getTopTypeDef :: TopDef -> Maybe TypeDef
getTopTypeDef (TopType t) = Just t
getTopTypeDef _           = Nothing

-- | Unwrap a (potential) top-level C inline block.
getTopCDefs :: TopDef -> Maybe String
getTopCDefs (TopCDefs b) = Just b
getTopCDefs _            = Nothing

-- | Unwrap a (potential) top-level external definition.
getTopExtern :: TopDef -> Maybe ExternDecl
getTopExtern (TopExtern d) = Just d
getTopExtern _             = Nothing

-- | Unzip a list of top-level declarations into their counterparts.
getTops :: [TopDef] -> ([TypeDef], [String], [ExternDecl], [Definition])
getTops tds =
  ( mapMaybe getTopTypeDef tds
  , mapMaybe getTopCDefs   tds
  , mapMaybe getTopExtern  tds
  , mapMaybe getTopDataDef tds
  )

instance Pretty Program where
  pretty (Program defs) = vsep (intersperse emptyDoc $ map pretty defs)

instance Pretty TopDef where
  pretty (TopDef    d ) = pretty d
  pretty (TopType   t ) = pretty t
  pretty (TopCDefs  ds) = pretty "$$" <> pretty ds <> pretty "$$"
  pretty (TopExtern x ) = pretty x
  pretty (TopImport i) = pretty i

instance Pretty Import where
	pretty (ImportSym is) = hsep (map pretty is)
	pretty (ImportAs is i) = hsep (map pretty is) <+> pretty "as" <+> pretty i
	pretty (ImportWith is iel) = hsep (map pretty is) <+> pretty "with" <+> pretty iel


instance Pretty ImportElement where
    pretty (ElementAs s a) = pretty s <+> pretty "as" <+> pretty a
    pretty (ElementSymbol s) = pretty s

instance Pretty ExternDecl where
  pretty (ExternDecl i t) = pretty "extern" <+> pretty i <+> colon <+> pretty t

instance Pretty TypeDef where
  pretty TypeDef { typeName = tn, typeParams = tvs, typeVariants = tds } =
    pretty "type" <+> hsep (pretty tn : map pretty tvs) <+> equals <+> braces
      (hsep $ punctuate bar $ map pretty tds)

instance Pretty TypeVariant where
  pretty (VariantUnnamed dc vs) = pretty dc <+> hsep (map pretty vs)

instance Pretty Definition where
  pretty (DefFn fid formals r body) =
    pretty fid
      <+> hsep (map (parens . pretty) formals)
      <+> pretty r
      <+> equals
      <+> braces (pretty body)

  pretty (DefPat b body) = pretty b <+> equals <+> braces (pretty body)

instance Pretty Pat where
  pretty PatWildcard   = pretty '_'
  pretty (PatId  s   ) = pretty s
  pretty (PatApp ps  ) = parens $ hsep $ map pretty ps
  pretty (PatLit l   ) = pretty l
  pretty (PatAs b p  ) = parens $ pretty b <> pretty '@' <> pretty p
  pretty (PatTup bs  ) = parens $ hsep (punctuate comma $ map pretty bs)
  pretty (PatAnn ty p) = parens $ pretty p <> colon <+> pretty ty

instance Pretty TypFn where
  pretty (TypReturn t) = rarrow <+> pretty t
  pretty (TypProper t) = colon <+> pretty t
  pretty TypNone       = emptyDoc

instance Pretty Typ where
  pretty (TTuple tys) = parens $ hsep (punctuate comma $ map pretty tys)
  pretty (TCon   cid) = pretty cid
  pretty (TApp (TCon tc) t2) | ident tc == "[]" = brackets $ pretty t2
                             | ident tc == "&"  = amp <> pretty t2
  pretty (TApp   t1 t2) = parens $ pretty t1 <+> pretty t2
  pretty (TArrow t1 t2) = pretty t1 <+> rarrow <+> pretty t2


instance Pretty Expr where
  pretty (Tuple elts) = parens $ hsep (punctuate comma $ map pretty elts)
  pretty (Let defs body) =
    pretty "let"
      <+> braces (hsep $ punctuate dbar $ map pretty defs)
      <>  semi
      <+> pretty body
  pretty (Seq e1 e2) = hsep [pretty e1 <> semi, pretty e2]
  pretty (After e1 v e2) =
    parens
      $   pretty "after"
      <+> pretty e1
      <+> comma
      <+> pretty v
      <+> larrow
      <+> braces (pretty e2)
  pretty (Assign     v  e) = parens $ pretty v <+> larrow <+> braces (pretty e)
  pretty (Constraint e  t) = parens $ pretty e <+> colon <+> pretty t
  pretty (OpRegion   e1 r) = parens $ pretty e1 <> p r
   where
    p EOR             = emptyDoc
    p (NextOp s e r') = space <> pretty s <+> pretty e <> p r'
  pretty (IfElse e1 e2 NoExpr) =
    parens $ pretty "if" <+> pretty e1 <+> braces (pretty e2)
  pretty (IfElse e1 e2 e3) =
    parens
      $   pretty "if"
      <+> pretty e1
      <+> braces (pretty e2)
      <+> pretty "else"
      <+> braces (pretty e3)
  pretty (While e1 e2) =
    parens $ pretty "while" <+> pretty e1 <+> braces (pretty e2)
  pretty (Loop e ) = parens $ pretty "loop" <+> braces (pretty e)
  pretty (Par  es) = parens $ pretty "par" <+> braces (hsep $ map pretty es)
  pretty (Wait vars) =
    parens $ pretty "wait" <+> hsep (punctuate comma $ map pretty vars)
  pretty (Lambda ps b) =
    parens $ pretty "fun" <+> hsep (map (parens . pretty) ps) <+> braces
      (pretty b)
  pretty (Apply e1 e2) = parens $ pretty e1 <+> pretty e2
  pretty (Id  i      ) = pretty i
  pretty (Lit l      ) = pretty l
  pretty Break         = pretty "break"
  -- TODO: we should replace every '$$' in s with '$$$$'
  pretty (CQuote s)    = pretty "$$" <> pretty s <> pretty "$$"
  pretty (CCall s as) =
    pretty "$" <> pretty s <+> parens (hsep $ punctuate comma $ map pretty as)
  pretty (Match s as) = parens $ pretty "match" <+> pretty s <+> braces
    (hsep $ punctuate bar $ map prettyPatExprTup as)
   where
    prettyPatExprTup (p, e) = pretty p <+> pretty "=" <+> braces (pretty e)
  -- pretty (ListExpr es) = brackets $ hsep $ punctuate comma $ map pretty es
  pretty NoExpr        = error "Unexpected NoExpr"

instance Pretty Literal where
  pretty (LitInt    i) = pretty i
  pretty (LitString s) = dquotes $ pretty s
  pretty (LitRat    r) = pretty $ show r
  pretty (LitChar   c) = squotes $ pretty c
  pretty LitEvent      = pretty "()"

instance Dumpy Program  where
  dumpy = pretty
