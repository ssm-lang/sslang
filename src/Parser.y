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
  '->'    { Token _ TRarrow }
  '||'    { Token _ TDBar }
  ':'     { Token _ TColon }
  ';'     { Token _ TSemicolon }
  ','     { Token _ TComma }
  '_'     { Token _ TUnderscore }
  '@'     { Token _ TAt }
  '&'     { Token _ TAmpersand }
  '('     { Token _ TLparen }
  ')'     { Token _ TRparen }
  '{'     { Token _ TLbrace }
  '}'     { Token _ TRbrace }
  '['     { Token _ TLbracket }
  ']'     { Token _ TRbracket }
  int     { Token _ (TInteger $$) }  
  string  { Token _ (TString $$) }
  op      { Token _ (TOp $$) }
  id      { Token _ (TId $$) }
  duration { Token _ (TDuration $$) }

%left ';' -- Helps with if-then-else
%right '->'
%nonassoc NOELSE 'else'

%%

program : topdecls { Program (reverse $1) }

topdecls : topdecl               { [$1] }
         | topdecls ';' topdecl  { $3 : $1 }

topdecl : id optFormals optReturnType '=' '{' expr '}' { Function $1 $2 $6 $3 }
        | id optFormals ':' typ '=' '{' expr '}'       { Function $1 $2 $7 (CurriedType $4) }

optReturnType : '->' typ      { ReturnType $2 }
              | {- nothing -} { ReturnType (TCon "()") }

optFormals : formals       { $1 }
           | {- nothing -} { [] }

formals : formalTop         { [$1] }
        | formalTop formals { $1 : $2 }

formalTop : id                    { Bind $1 Nothing }
          | '_'                   { Bind "_" Nothing }
          | '(' ')'               { Bind "()" Nothing }
          | '(' formalOrTuple ')' { $2 }

formalOrTuple : formal                  { $1 }
              | formal ',' tupleFormals { TupBind ($1 : $3) Nothing }

formal : '(' formal ')'                          { $2 }
       | id optType                              { Bind $1 $2 }
       | '_' optType                             { Bind "_" $2 }
       | '(' ')' optType                         { Bind "()" $3 }
       | '(' formal ',' tupleFormals ')' optType { TupBind ($2 : $4) $6 }

tupleFormals : formal                  { [$1] }
             | formal ',' tupleFormals { $1 : $3 }

optType : {- nothing -} { Nothing }
        | ':' typ       { Just $2 }

typ : typ1 '->' typ { TArrow $1 $3 }
    | typ1          { $1 }

typ1 : typ1 typ2 { TApp $1 $2 }
     | typ2      { $1 }

typ2 : id                        { TCon $1 }
     | '[' typ ']'               { TApp (TCon "List") $2 }
     | '(' typ ')'               { $2 }
     | '(' typ ',' tupleTyps ')' { TTuple ($2 : $4) }
     | '(' ')'                   { TCon "()" }
     | '&' typ2                  { TApp (TCon "Ref") $2 }

tupleTyps : typ               { [$1] }
          | typ ',' tupleTyps { $1 : $3 }

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

ids : id          { [$1] }
    | ids ',' id  { $3 : $1 }
      
elseOpt : {- nothing -} %prec NOELSE { NoExpr }
        | ';' 'else' expr1           { $3 }

expr1 : expr1 ':' typ                  { Constraint $1 $3 }
      | expr2                          { $1 }
  
expr2 : apply opregion  { OpRegion $1 $2 }
      | apply           { $1 }

opregion : op apply opregion { NextOp $1 $2 $3 }
         | op apply          { NextOp $1 $2 EOR }

apply : apply aexpr { Apply $1 $2 }
      | aexpr       { $1 }

aexpr : int             { Literal (IntLit $1) }
      | string          { Literal (StringLit $1) }
      | duration        { Literal (DurLit $1) }
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
