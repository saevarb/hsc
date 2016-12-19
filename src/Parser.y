{
module Parser where

import Data.Char

import Lexer
import Types

}

%name parseProgram
%error { parseError }
%lexer { lexer } {TokenEOF}
%monad {Alex}
%tokentype { Token }
%token
var         { TokenVar }
id          { TokenId $$ }
num         { TokenInt $$ }
return      { TokenReturn }
allocate    { TokenAllocate }
write       { TokenWrite }
if          { TokenIf }
then        { TokenThen }
else        { TokenElse }
while       { TokenWhile }
do          { TokenDo }
true        { TokenTrue }
false       { TokenFalse }
null        { TokenNull }
type        { TokenType }
"of length" { TokenOfLength }
'='         { TokenAssign }
'+'         { TokenPlus }
'-'         { TokenMinus }
'*'         { TokenMul }
'/'         { TokenDiv }
'('         { TokenLP }
')'         { TokenRP }
';'         { TokenSC }
':'         { TokenColon }
','         { TokenComma }
'!'         { TokenNot }
'|'         { TokenPipe }
"=="        { TokenEq }
"||"        { TokenOr }
"&&"        { TokenAnd }




%left '+' '-'
%left '*' '/'
%right '!'
%%

Body : DeclList StmtList { Body (concatDecls $1) $2 }

DeclList :: { [Decl] }
: DeclList Decl ';' { $2 : $1 }
| { [] }

Decl :: { Decl }
: var VarDeclList { VarDecls $2 } 
| TypeDecl { $1 }

VarDeclList :: { [Decl] }
: VarType { [$1] }
| VarDeclList ',' VarType { $3 : $1 }
|  { [] }

VarType :: { Decl }
VarType : VarId ':' TypeId { VarDecl $1 $3 }

TypeDecl :: { Decl }
: type TypeId '=' TypeId  { TypeDecl $2 $4 }

StmtList :: { [Stmt] }
: Stmt          { [$1] }
| StmtList Stmt { $2 : $1 }

Stmt :: { Stmt }
: allocate Var                 ';' { AllocStmt $2 (ConstExp 1) }
| allocate Var "of length" Exp ';' { AllocStmt $2 $4 }
| return Exp ';'                  { RetStmt $2 }
| write Exp  ';'                  { WriteStmt $2 }
| Var '=' Exp ';' { AssignStmt $1 $3 }
| if Exp then Stmt else Stmt { IfStmt $2 $4 (Just $6) }
| if Exp then Stmt { IfStmt $2 $4 Nothing }
| while Exp do Stmt { WhileStmt $2 $4 }


ExpList :: { [Exp] }
    :             { [] }
    | Exp         { [$1] }
    | ExpList ',' Exp { $3 : $1 }

Exp :: { Exp }
    : Exp '+' Exp        { BinExp $1 Add $3 }
    | Exp '-' Exp        { BinExp $1 Sub $3 }
    | Exp '*' Exp        { BinExp $1 Mul $3 }
    | Exp '/' Exp        { BinExp $1 Div $3 }
    | Exp "||" Exp       { BinExp $1 LOr $3 }
    | Exp "&&" Exp       { BinExp $1 LAnd $3 }
    | Exp "==" Exp       { BinExp $1 LEq $3 }
    | Var                { VarExp $1 }
    | id '(' ExpList ')' { AppExp $1 $3 }
    | '(' Exp ')'        { $2 }
    | '!' Exp            { UnExp $2 LNot }
    | '|' Exp '|'        { AbsExp $2 }
    | null               { NullExp }
    | true               { BoolExp True }
    | false              { BoolExp False }
    | num                { ConstExp $1 }

VarId : id { VarId $1 }
TypeId : id { TypeId $1 }

Var : VarId { Var $1 }

{


concatDecls = concatMap expand
  where
    expand (VarDecls ds) = ds
    expand e = [e]

parseError e = do
    (pos, pc, _, inp)<- alexGetInput
    alexError $ unlines
      [ ""
      , formatLocationInfo pos ++ " - error: " ++ show e
      , "\t... " ++ (take 50 inp) ++ " ..."
      ]
  where
    formatLocationInfo (AlexPn _ l c) = concat [show l, ":", show c]

}
