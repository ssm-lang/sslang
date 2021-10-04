{

{-
happy -i Parser.y -o /dev/null

Then check Parser.info

-}

module Parser where
  
import Scanner
import Ast

}

%name parse
%error { parseError }
%lexer { lexerForHappy } { Token _ TEOF }
%monad { Alex }
%tokentype { Token }

%token
  'if'    { Token _ TIf }
  'then'  { Token _ TThen }
  'else'  { Token _ TElse }
  'while' { Token _ TWhile }
  'do'    { Token _ TDo }
  'par'   { Token _ TPar }
  'loop'  { Token _ TLoop }  
  'let'   { Token _ TLet }
  'and'   { Token _ TAnd }  
  'later' { Token _ TLater }
  'wait'  { Token _ TWait }
  '='     { Token _ TEq }
  '<-'    { Token _ TLarrow }
  '||'    { Token _ TDBar }
  ':'     { Token _ TColon }
  ';'     { Token _ TSemicolon }
  ','     { Token _ TComma }
  '_'     { Token _ TUnderscore }
  '@'     { Token _ TAt }
  '('     { Token _ TLparen }
  ')'     { Token _ TRparen }
  '{'     { Token _ TLbrace }
  '}'     { Token _ TRbrace }
  int     { Token _ (TInteger $$) }  
  string  { Token _ (TString $$) }
  op      { Token _ (TOp $$) }
  id      { Token _ (TId $$) }

%left ';' -- Helps with if-then-else
%nonassoc NOELSE 'else'

%%

program : topdecls { Program (reverse $1) }

topdecls : topdecl               { [$1] }
         | topdecls ';' topdecl  { $3 : $1 }

topdecl : id '(' optFormals ')' '=' '{' expr '}' { Function $1 $3 $7 }

optFormals : {- nothing -} { [] }
           | formals       { reverse $1 }

formals : formal             { [$1] }
        | formals ',' formal { $3 : $1 }

formal : ids ':' typs   { Bind (reverse $1) $3 }

ids : id          { [$1] }
    | ids ',' id  { $3 : $1 }

typs : typ      { $1 }
     | typs typ { TApp $1 $2 }
    
typ : id           { TCon $1 }
    | '(' typs ')' { $2 }

expr : expr ';' expr0 { Seq $1 $3 }
     | expr0          { $1 }

expr0 : 'if' expr 'then' expr0 elseOpt  { IfElse $2 $4 $5 }
      | 'while' expr 'do' expr0         { While $2 $4 }
      | 'let' '{' decls '}'             { Let (reverse $3) }
      | 'loop' expr0                    { Loop $2 }
      | 'wait' ids                      { Wait $2 }
      | 'par' '{' parExprs '}'          { Par (reverse $3) }
      | expr2 'later' expr2 '<-' expr0  { Later $1 (exprToPat $3) $5 }
      | expr2 '<-' expr0                { Assign (exprToPat $1) $3 }
      | id '@' expr0                    { As $1 $3 }
      | expr1                           { $1 }
      
elseOpt : {- nothing -} %prec NOELSE { NoExpr }
        | ';' 'else' expr1           { $3 }

expr1 : expr1 ':' typs                  { Constraint $1 $3 }
      | expr2                           { $1 }
  
expr2 : apply opregion  { OpRegion $1 $2 }
      | apply           { $1 }

opregion : op apply opregion { NextOp $1 $2 $3 }
         | op apply          { NextOp $1 $2 EOR }

apply : apply aexpr { Apply $1 $2 }
      | aexpr       { $1 }

aexpr : int             { Literal (IntLit $1) }
      | string          { Literal (StringLit $1) }
      | id              { Id $1 }
      | '_'             { Wildcard }
      | '(' expr ')'    { $2 }
      | '{' expr '}'    { $2 }

decls : decls 'and' decl  { $3 : $1 }
      | decl              { [$1] }

parExprs : parExprs '||' expr { $3 : $1 }
         | expr               { [$1] }

decl : expr2 '=' aexpr      { Def (exprToPat $1) $3 }

       
{

data Lit = SLit String
         | ILit Integer
  deriving Show
  
parseError :: Token -> a
parseError t = error $ case t of
  Token (AlexPn _ l c) _ -> show l ++ ":" ++ show c ++ ":Syntax error"

exprToPat :: Expr -> Pat
exprToPat (Id s) = PId s
exprToPat (Literal l) = PLiteral l
exprToPat Wildcard = PWildcard
exprToPat (As v e) = PAs v (exprToPat e)
exprToPat (Apply (Id pc) pats) = PCon pc $ patList pats
 where
   patList (Apply p1 ps) = exprToPat p1 : patList ps
   patList e = [exprToPat e]
exprToPat e = error $ "syntax error in pattern " ++ show e

}
