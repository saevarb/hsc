{
module Parser where

import Data.Char

import Types
import Lexer
}

%name parseProgram
%tokentype { Token }
%error { parseError }
%token
id       { TokenId $$ }
num      { TokenInt $$ }
return   { TokenReturn }
allocate { TokenAllocate }
write    { TokenWrite }
if       { TokenIf }
then     { TokenThen }
else     { TokenElse }
true     { TokenTrue }
false    { TokenElse }
null     { TokenNull }
"of length" { TokenOfLength }
'='      { TokenEq }
'+'      { TokenPlus }
'-'      { TokenMinus }
'*'      { TokenMul }
'/'      { TokenDiv }
'('      { TokenLP }
')'      { TokenRP }
';'      { TokenSC }
','      { TokenComma }
'!'      { TokenNot }
'|'      { TokenPipe }




%left '+' '-'
%left '*' '/'
%right '!'
%%

Body : DeclList StmtList { Body $1 $2 }

DeclList : { [] }

StmtList :: { [Stmt] }
    : Stmt      { [$1] }
    | StmtList Stmt  { $2 : $1 }

Stmt : Stmt1 ';' { $1 }

Stmt1 :: { Stmt }
    : allocate Var                 { AllocStmt $2 (ConstExp 1) }
    | allocate Var "of length" Exp { AllocStmt $2 $4 }
    | return Exp                   { RetStmt $2 }
    | write Exp                    { WriteStmt $2 }

ExpList :: { [Exp] }
    :             { [] }
    | Exp         { [$1] }
    | ExpList ',' Exp { $3 : $1 }

Exp :: { Exp }
    : Exp '+' Exp        { BinExp $1 Add $3 }
    | Exp '-' Exp        { BinExp $1 Sub $3 }
    | Exp '*' Exp        { BinExp $1 Mul $3 }
    | Exp '/' Exp        { BinExp $1 Div $3 }
    | Var                { VarExp $1 }
    | id '(' ExpList ')' { AppExp $1 $3 }
    | '(' Exp ')'        { $2 }
    | '!' Exp            { UnExp $2 LNot }
    | '|' Exp '|'        { AbsExp $2 }
    | null               { NullExp }
    | true               { BoolExp True }
    | false              { BoolExp False }
    | num                { ConstExp $1 }

Var : id  { VarId $1 }
{

}
