{-|
Module: C
Description: AST and pretty-printing for the C language

This relies on the Text.PrettyPrint package

This should be as input-language-agnostic as possible

https://cs.wmich.edu/~gupta/teaching/cs4850/sumII06/The%20syntax%20of%20C%20in%20Backus-Naur%20form.htm

https://www.lysator.liu.se/c/ANSI-C-grammar-y.html

https://www.stackage.org/haddock/lts-16.1/prettyprinter-1.6.1/Data-Text-Prettyprint-Doc.html

https://stackoverflow.com/questions/35398355/pretty-printing-syntax-tree-with-operator-precedence-and-associativity-in-haskel

-}

module C where

import Data.Text.Prettyprint.Doc
import Prelude hiding ((<>), id)

newtype TranslationUnit = TranslationUnit [ExternalDeclaration]

data ExternalDeclaration =
    FunctionDefinition String CType Statement
  | External String CType Initializer
  | TopVerbatim String

data Declaration = Declaration String CType Initializer

-- A more mathematical approach to C's types, backwards from the usual
-- specifier/declarator syntax.  The idea is that if you unwrap the name
-- from the outside in (e.g., dereference, call) you end up with the
-- thing at the bottom: one or more specifiers.  Specifiers also include
-- typedef'ed type names

-- int * = Pointer $ Spec "int" NT
--  [ Bind "argc" (Spec "int" NT)                        "int argc"
--  , Bind "argv" (Array $ Pointer $ Spec "char" NT)])   "char *(argv[])"

data CType = NT                           -- The end of the type
           | Spec     String CType        -- E.g., typedef, extern, int, double
           | Struct   String [Bind] CType -- struct foo { ... }  name optional
           | Union    String [Bind] CType -- union bar { ... }   name optional
           | Pointer  CType               -- *a
           | Array    CType               -- a[]  FIXME: could add a sized array
           | Function CType [Bind]        -- a(int a, int *b)

data Bind = Bind String CType -- A name with a type

data Initializer = NoInit
                 | ExprInitializer Expression
-- FIXME: also handle int a[] = { 3, 4, 6 } case

data Statement = Compound [Declaration] [Statement] -- { ... }
               | Expr Expression                    -- a = 3;
               | Label String                       -- foo:
               | Case Expression                    -- case 3:
               | Default                            -- default:
               | If Expression Statement
               | IfElse Expression Statement Statement
               | Switch Expression Statement
               | While Expression Statement
               | Do Statement Expression
               | For (Maybe Expression) (Maybe Expression) (Maybe Expression)
                     Statement
               | Goto String
               | Break
               | Continue
               | Return (Maybe Expression)
               | StmtVerbatim String 

-- Unary operators

data UnOp = Incr    -- ++
          | Decr    -- --
          | Address -- &
          | Deref   -- *
          | Neg     -- -
          | LogNot  -- !
          | BitNot  -- ~

data BinOp = Mult   -- *
           | Div    -- /
           | Mod    -- %
           | Add    -- +
           | Sub    -- -
           | Equal  -- ==
           | Neq    -- !=
           | Lt     -- <
           | Gt     -- >
           | Le     -- <=
           | Ge     -- >=
           | Lshift -- <<
           | Rshift -- >>          
           | BitAnd -- &
           | BitOr  -- |
           | BitXor -- ^
           | LogAnd -- &&
           | LogOr  -- ||
           | Assign -- =
           | AssignOp BinOp -- +=, etc.

data Expression = ID String                    -- a
                | IntLit Int                   -- 42
                | LongLit Integer              -- 42L, e.g., for times
                | Call Expression [Expression] -- foo(3, 2)\
                | Index Expression Expression  -- b[3]
                | Field Expression String      -- a.x
                | Arrow Expression String      -- a->x
                | Post  Expression UnOp        -- a++, a-- (postfix)
                | Comma Expression Expression  -- a=3, b=4
                | Pre   UnOp Expression        -- ++a (prefix)
                | Binary Expression BinOp Expression -- a + b
                | Cond Expression Expression Expression -- a ? b : c
                | Cast CType Expression        -- (int *) foo
                | Sizeof CType                 -- sizeof(int *)

instance Show TranslationUnit where
  show t = show $ pretty t
                
emptyString :: String -> Doc ann
emptyString "" = emptyDoc
emptyString s = space <> pretty s

instance Pretty TranslationUnit where
  pretty (TranslationUnit decls) =
    concatWith (\x y -> x <> line <> y) (map pretty decls)

structUnion :: String -> String -> [Bind] -> Doc ann
structUnion su id [] = pretty su <> emptyString id
structUnion su id fields =
    vsep [nest 2 (vsep ((pretty su <> emptyString id <+> lbrace) :
                      map (\b -> pretty b <> semi) fields))
       , rbrace]

ppSpecifiers :: CType -> Doc ann
ppSpecifiers ty = hsep $ specifiers ty
  where
    specifiers NT                  = []
    specifiers (Spec s t)          = pretty s : specifiers t
    specifiers (Struct s fields t) =
      structUnion "struct" s fields : specifiers t
    specifiers (Union s fields t)  =
      structUnion "union" s fields : specifiers t
    specifiers (Pointer t)         = specifiers t
    specifiers (Array t)           = specifiers t
    specifiers (Function t _)      = specifiers t

-- | Parenthesize if flag is true
prettyParen :: Bool -> Doc ann -> Doc ann
prettyParen False d = d
prettyParen True d = parens d

emptyDeclarator :: CType -> Bool
emptyDeclarator NT = True
emptyDeclarator (Spec _ t) = emptyDeclarator t
emptyDeclarator (Struct _ _ t) = emptyDeclarator t
emptyDeclarator (Union _ _ t) = emptyDeclarator t
emptyDeclarator _ = False

{- Precedences of declarator operators
   2 : a	   Bare name
   1 : a[],  a()   Array, Function
   0 : *a	   Pointer
-}

ppDeclarator :: CType -> (Int, Doc ann) -> (Int, Doc ann)
ppDeclarator NT                  (_, d) = (2, d)
ppDeclarator (Spec _ t)          (p, d) = ppDeclarator t (p, d)
ppDeclarator (Struct _ _ t)      (p, d) = ppDeclarator t (p, d)
ppDeclarator (Union _ _ t)       (p, d) = ppDeclarator t (p, d)
ppDeclarator (Pointer t)         (_, d) = ppDeclarator t
         (0, pretty '*' <> d)
ppDeclarator (Array t)           (p, d) = ppDeclarator t
         (1, prettyParen (p < 1) d <> brackets emptyDoc)
ppDeclarator (Function t params) (p, d) = ppDeclarator t
         (1, prettyParen (p < 1) d <> align (tupled $ map pretty params))

ppBind :: String -> CType -> Doc ann
ppBind "" t | emptyDeclarator t = ppSpecifiers t
            | otherwise = ppSpecifiers t <+>
                          snd (ppDeclarator t (2, emptyDoc))
ppBind id t = ppSpecifiers t <+> snd (ppDeclarator t (2, pretty id))

instance Pretty Bind where
  pretty (Bind s t) = ppBind s t

instance Pretty ExternalDeclaration where
  pretty (FunctionDefinition id t statement) = vsep [ ppBind id t
                                                    , pretty statement] <> line
  pretty (External id t NoInit) = ppBind id t <> semi <> line
  pretty (External id t (ExprInitializer e)) =
    ppBind id t <+> pretty '=' <+> prettyPrec 1 e <> semi <> line
  pretty (TopVerbatim s) = pretty s

instance Pretty Declaration where
  pretty (Declaration id t NoInit) = ppBind id t <> semi
  pretty (Declaration id t (ExprInitializer e)) =
    ppBind id t <+> pretty '=' <+> prettyPrec 1 e <> semi
    
instance Pretty Statement where
  pretty (Compound decls stmts) = vsep [nest 2 (vsep $
                                         lbrace :
                                         map pretty decls ++
                                         map pretty stmts)
                                       , rbrace]
  pretty (Expr e) = prettyPrec 0 e <> semi
  pretty (Label l) = nest (-2) (line <> pretty l <> colon <+> semi)
  pretty (Case e) = nest (-2) (line <> pretty "case" <+> prettyPrec 0 e <> colon <+> semi)
  pretty Default = pretty "default:"
  pretty (If e s) = pretty "if" <+> parens (prettyPrec 0 e) <+> pretty s
  pretty (IfElse e s1 s2) =
    pretty "if" <+> parens (prettyPrec 0 e) <+> pretty s1 <+>
    pretty "else" <+> pretty s2
  pretty (Switch e s) = vsep [ pretty "switch" <+> parens (prettyPrec 0 e),
                               pretty s ]
  pretty (While e s) = pretty "while" <+> parens (prettyPrec 0 e) <+> pretty s
  pretty (Do s e) = pretty "do" <+>
                    pretty s <+>
                    pretty "while" <+> parens (prettyPrec 0 e) <> semi
  pretty (For e1 e2 e3 s) =
    pretty "for" <+> parens (hsep $ punctuate semi $ map mexpr [e1,e2,e3]) <+>
    pretty s
    where mexpr Nothing = emptyDoc
          mexpr (Just e) = prettyPrec 0 e
  pretty (Goto s) = pretty "goto" <+> pretty s <> semi
  pretty Break = pretty "break" <> semi
  pretty Continue = pretty "continue" <> semi
  pretty (Return Nothing) = pretty "return" <> semi
  pretty (Return (Just e)) = pretty "return" <+> prettyPrec 0 e <> semi
  pretty (StmtVerbatim s) = pretty s

class PrettyPrec a where
  prettyPrec :: Int -> a -> Doc ann

{-  C operator precedence levels:
  15 : id
  14 : Unary () [] -> . ++ --           (right to left)
  13 : Unary + - ! ~ ++ -- case sizeof  (left to right)
  12 : * / %                            (left to right)
  11 : + -                              (left to right)
  10 : << >>                            (left to right)
  9  : < <= > >=                        (left to right)
  8  : == !=                            (left to right)
  7  : &                                (left to right)
  6  : ^                                (left to right)
  5  : |                                (left to right)
  4  : &&                               (left to right)
  3  : ||                               (left to right)
  2  : ?:                               (right to left)
  1  : = +=, etc.                       (right to left)
  0  : ,                                (left to right)
-}

instance PrettyPrec Expression where
  prettyPrec _ (ID s)     = pretty s
  prettyPrec _ (IntLit i) = pretty i
  prettyPrec _ (LongLit l) = pretty l <> pretty 'L'
  prettyPrec p (Call e args) = prettyParen (p > 14) $
    prettyPrec 14 e <> align (tupled $ map (prettyPrec 0) args)
  prettyPrec p (Index e i) = prettyParen (p > 14) $
    prettyPrec 14 e <> brackets (prettyPrec 0 i)
  prettyPrec p (Field e s) = prettyParen (p > 14) $
    prettyPrec 14 e <> pretty '.' <> pretty s
  prettyPrec p (Arrow e s) = prettyParen (p > 14) $
    prettyPrec 14 e <> pretty "->" <> pretty s
  prettyPrec p (Post e o) = prettyParen (p > 14) $
    prettyPrec 14 e <> pretty o
  prettyPrec p (Comma e1 e2) = prettyParen (p > 0) $
    prettyPrec 0 e1 <> pretty ", " <> prettyPrec 1 e2
  prettyPrec p (Pre o e) = prettyParen (p > 13) $
    pretty o <> prettyPrec 13 e
  prettyPrec p (Binary e1 Assign e2) = prettyParen (p > 1) $
    prettyPrec 2 e1 <+> pretty "=" <+> prettyPrec 1 e2
  prettyPrec p (Binary e1 (AssignOp o) e2) = prettyParen (p > 1) $
    prettyPrec 2 e1 <+> pretty o <> pretty '=' <+> prettyPrec 1 e2    
  prettyPrec p (Binary e1 op e2) = prettyParen (p > p') $
    prettyPrec p' e1 <+> pretty op <+> prettyPrec (succ p') e2
    where p' = case op of Mult   -> 12           
                          Div    -> 12            
                          Mod    -> 12            
                          Add    -> 11             
                          Sub    -> 11            
                          Lshift -> 10               
                          Rshift -> 10              
                          Lt     -> 9             
                          Gt     -> 9              
                          Le     -> 9             
                          Ge     -> 9             
                          Equal  -> 8              
                          Neq    -> 8             
                          BitAnd -> 7              
                          BitXor -> 6              
                          BitOr  -> 5
                          LogAnd -> 4              
                          LogOr  -> 3              
                          Assign -> error "should never happen"     -- 1
                          AssignOp _ -> error "should never happen" -- 1
  prettyPrec p (Cond t i e) = prettyParen (p > 1) $
    prettyPrec (succ p) t <+> pretty '?' <+>
    prettyPrec p i <+> pretty ':' <+> prettyPrec p e   
  prettyPrec p (Cast t e) = prettyParen (p > 13) $
    parens (ppBind "" t) <+> prettyPrec 13 e
  prettyPrec p (Sizeof t) = prettyParen (p > 13) $
    pretty "sizeof" <> parens (ppBind "" t)

instance Pretty BinOp where
  pretty Mult   = pretty '*'
  pretty Div    = pretty '/'
  pretty Mod    = pretty '%'
  pretty Add    = pretty '+'
  pretty Sub    = pretty '-'
  pretty Equal  = pretty "=="
  pretty Neq    = pretty "!="
  pretty Lt     = pretty '<'
  pretty Gt     = pretty '>'
  pretty Le     = pretty "<="
  pretty Ge     = pretty ">="
  pretty Lshift = pretty "<<"
  pretty Rshift = pretty ">>"
  pretty BitAnd = pretty '&'
  pretty BitOr  = pretty '|'
  pretty BitXor = pretty '^'
  pretty LogAnd = pretty "&&"
  pretty LogOr  = pretty "||"
  pretty Assign = pretty '='
  pretty (AssignOp o) = pretty o <> pretty '='

instance Pretty UnOp where
  pretty Incr    = pretty "++"
  pretty Decr    = pretty "--"
  pretty Address = pretty '&'
  pretty Deref   = pretty '*'
  pretty Neg     = pretty '-'
  pretty LogNot  = pretty '!'
  pretty BitNot  = pretty '~'
