{ {- | Parser for sslang token stream.
To check for shift/reduce and reduce/reduce conflicts, generate parser manually:

  happy -i Parser.y -o /dev/null

Then check Parser.info.
-}
module Front.Parser where

import Front.Scanner
import Front.Ast
import Front.Token
import Prettyprinter (pretty)
}

%name parse
%error { parseError }
%lexer { lexerForHappy } { Token (_, TEOF) }
%monad { Alex }
%tokentype { Token }

%token
  'if'    { Token (_, TIf) }
  'else'  { Token (_, TElse) }
  'while' { Token (_, TWhile) }
  'do'    { Token (_, TDo) }
  'par'   { Token (_, TPar) }
  'loop'  { Token (_, TLoop) }
  'let'   { Token (_, TLet) }
  'after' { Token (_, TAfter) }
  'wait'  { Token (_, TWait) }
  '='     { Token (_, TEq) }
  '<-'    { Token (_, TLarrow) }
  '->'    { Token (_, TRarrow) }
  '||'    { Token (_, TDBar) }
  ':'     { Token (_, TColon) }
  ';'     { Token (_, TSemicolon) }
  ','     { Token (_, TComma) }
  '_'     { Token (_, TUnderscore) }
  '@'     { Token (_, TAt) }
  '&'     { Token (_, TAmpersand) }
  '('     { Token (_, TLparen) }
  ')'     { Token (_, TRparen) }
  '{'     { Token (_, TLbrace) }
  '}'     { Token (_, TRbrace) }
  '['     { Token (_, TLbracket) }
  ']'     { Token (_, TRbracket) }
  int     { Token (_, TInteger $$) }
  string  { Token (_, TString $$) }
  op      { Token (_, TOp $$) }
  id      { Token (_, TId $$) }

%left ';' -- Helps with if-then-else
%nonassoc NOELSE 'else'

%%
-- | Root node of sslang program parser.
program                               --> Program
  : defTop                              { Program $1 }
  -- TODO: support empty program

{- | Top-level definitions.

Note that although these also begin with 'let', they are grammatically (and
indeed semantically) distinct from actual let-bindings (@defLets@).

They are separated with '||' rather than ';' because the latter somehow causes
shift-reduce conflicts.
-}
defTop                                --> [Definition]
  : defLet '||' defTop                  { $1 : $3 }
  | defLet                              { [$1] }

-- | Mutually recursive block of let-definitions.
defLets                               --> [Definition]
  : defLet '||' defLets                 { $1 : $3 }
  | defLet                              { [$1] }

{- | Single let-definition.

We use 'categorizeDef' to figure out whether we are defining a function or a
variable.
-}
defLet                                --> Definition
  : bindArgs typFn '=' '{' expr '}'     { categorizeDef $1 $2 $5 }

-- | A list of argument binders consists of a series of juxtaposed patterns.
bindArgs                              --> [Bind]
  : bindArg bindArgs                    { $1 : $2 }
  | {- nothing -}                       { [] }

-- | Argument binder, which cannot be type-annotated without wrapping parens.
bindArg                               --> Bind
  : pat                                 { $1 }

-- | Tuple binders are comma-separated and can be type-annotated.
bindTups                              --> [Bind]
  : bindTup ',' bindTups                { $1 : $3 }
  | bindTup                             { [$1] }

-- | Tuple binder, which consist of a pattern and an optional type annotation.
bindTup                               --> Bind
  : pat typBind                         { annBind $1 $2 }

-- | Root node of pattern.
pat                                   --> Bind
  : patPre                              { $1 }

-- | Prefix pattern operators.
patPre                                --> Bind
  : id '@' patPre                       { Bind (BindAs $1 $3) [] }
  | patAtom                             { $1 }

{- | Atomic patterns.

Note that a 1-ary tuple decays into regular binding (i.e., pattern with optional
type annotation).
-}
patAtom                               --> Bind
  : '_'                                 { Bind BindWildcard []}
  | '(' bindTups ')'                    { normalize id (flip Bind []. BindTup) $2 }
  | '(' ')'                             { Bind (BindLit LitEvent) [] }
  | id                                  { Bind (BindId $1) [] }
  -- TODO: actually support literal patterns

-- | Root node for type annotation.
typ                                   --> Typ
  : typIn                               { $1 }

-- | Infix type operators. Note that '->' is right-associative.
typIn                                 --> Typ
  : typApp '->' typIn                   { TArrow $1 $3 }
  | typApp                              { $1 }

-- | Type application by juxtaposition. Left-associative.
typApp                                --> Typ
  : typApp typPre                       { TApp $1 $2 }
  | typPre                              { $1 }

-- | Prefix type operators.
typPre                                --> Typ
  : '&' typPre                          { TApp (TCon "&") $2 }
  | typAtom                             { $1 }

{- | Atomic type expressions.

Note that a 1-ary tuple type decays into regular type (which happens when you
parenthesize a type expression).
-}
typAtom                               --> Typ
  : id                                  { TCon $1 }
  | '(' typTups ')'                     { normalize id TTuple $2 }
  | '[' typ ']'                         { TApp (TCon "[]") $2 }
  | '(' ')'                             { TCon "()" }

-- | Type annotation for (potentially) functions.
typFn                                 --> TypFn
  : '->' typ                            { TypReturn $2 }
  | ':' typ                             { TypProper $2 }
  | {- nothing -}                       { TypNone }

-- | Type annotation for patterns.
typBind                               --> [Typ]
  : ':' typ                             { [$2] }
  | {- nothing -}                       { [] }

-- | Tuple types are comma-separated.
typTups                               --> [Typ]
  : typ ',' typTups                     { $1 : $3 }
  | typ                                 { [$1] }

-- | Root node for expressions.
expr                                  --> Expr
  : exprSeq                             { $1 }

{- | Sequenced expressions.

Note that the semantics of 'Seq' are fully associative, so it shouldn't really
matter how we assemble that tree as long as the leaves are correctly ordered.
Here it happens to be right-associative.
-}
exprSeq                               --> Expr
  : exprStm ';' exprSeq                 { Seq $1 $3 }
  | 'let' '{' defLets '}' ';' exprSeq   { Let $3 $6 }
  | exprStm                             { $1 }

{- | Expressions that resemble some kind of statement.

This rule exists because it ensures that type annotations are at a higher
precedence than these statements (for which it makes little sense to type
annotate). Yet this rule is not self-referential because we want to discourage
arbitrary combination of these syntactic constructs, e.g., no @a <- b <- c@
business.
-}
exprStm                               --> Expr
  : exprAnn '<-' '{' exprAnn '}'        { Assign $1 $4 }
  | 'after' exprAnn ',' exprAnn '<-' '{' exprAnn '}'
                                        { After $2 $4 $7 }
  | exprAnn                             { $1 }

-- | Expressions with type annotations.
exprAnn                               --> Expr
  : exprAnn ':' typ                     { Constraint $1 $3 }
  | exprOp                              { $1 }

-- | Expressions with operators, whose precedence is not known a priori.
exprOp                                --> Expr
  : exprBlk exprOpRegion                { OpRegion $1 $2 }
  | exprBlk                             { $1 }

-- | Expressions with operators are parsed as flat lists.
exprOpRegion                          --> OpRegion
  : op exprBlk exprOpRegion             { NextOp $1 $2 $3 }
  | op exprBlk                          { NextOp $1 $2 EOR }

{- | Expressions that consist of blocks of other expressions.

While these have statement-like semantics, they are at such a low precedence
because they syntactically resemble juxtaposed alphanumeric tokens. Yet they are
at higher precedence than regular application because something like 'f if ...'
looks confusing.

Note that do-blocks appear here as an alternative to parenthesizing expressions,
which the scanner can exploit to insert implicit braces and semicolons.
-}
exprBlk                               --> Expr
  : 'do' '{' expr '}'                   { $3 }
  | 'loop' '{' expr '}'                 { Loop $3 }
  | 'wait' '{' exprPar '}'              { Wait $3 }
  | 'par' '{' exprPar '}'               { Par $3 }
  | 'if' exprBlk '{' expr '}' exprElse  { IfElse $2 $4 $6 }
  | 'while' exprBlk '{' expr '}'        { While $2 $4 }
  | exprApp                             { $1 }

-- | Optional trailing 'else' branch to 'if' statement.
exprElse                              --> Expr
  : ';' 'else' '{' expr '}'             { $4 }
  | 'else' '{' expr '}'                 { $3 }
  | {- nothing -} %prec NOELSE          { NoExpr }

-- | Expressions with application by juxtaposition.
exprApp                               --> Expr
  : exprApp exprAtom                    { Apply $1 $2 }
  | exprAtom                            { $1 }

-- | Atomic expressions.
exprAtom
  : int                                 { Lit (LitInt $1) }
  | string                              { Lit (LitString $1) }
  | id                                  { Id $1 }
  | '(' expr ')'                        { $2 }
  | '(' ')'                             { Lit LitEvent }

-- | Pipe-separated expressions, for parallel composition.
exprPar                               --> [Expr]
  : expr '||' exprPar                   { $1 : $3 }
  | expr                                { [$1] }
{
-- | What to do upon encountering parse error.
--
-- TODO does this really just throw an uncaught exception on parse error, or
-- does Happy catch that for us?
parseError :: Token -> a
parseError (Token (sp, _)) =
    error $ show (pretty sp) ++ ":Syntax error"

-- | Top-level helper function to parse program from string.
parseProgram :: String -> Either String Program
parseProgram = flip runAlex parse

{- | List combinator for singleton vs other lists.

Useful for distinguishing between parentheses vs tuple lists.
-}
normalize :: (a -> b) -> ([a] -> b) -> [a] -> b
normalize f _ [x] = f x
normalize _ f xs = f xs

{- | Categorize the contents of a let-definition.

- Anything of the form @a : T = ...@ is treated as a pattern binding.
- Anything of the form @pat = ...@ is treated as a pattern binding.
- Anything of the form @id [pats..] [:T|->T] = ...@ is treated as a function
  binding.
- Nothing else is considered well-formed.
-}
categorizeDef :: [Bind] -> TypFn -> Expr -> Definition
categorizeDef [Bind pat []] (TypProper typ) = DefPat $ Bind pat [typ]
categorizeDef [b] (TypNone) = DefPat b
categorizeDef (Bind (BindId f) []:args) fAnn = DefFn f args fAnn
categorizeDef _ _ = error "Syntax error in definition"
}
