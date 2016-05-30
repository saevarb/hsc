{
module Lexer (lexer, Token(..)) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+	;
  "#".*		;
  "("  { \s -> TokenLP }
  ")"  { \s -> TokenRP }
  "{"  { \s -> TokenLB }
  "}"  { \s -> TokenRB }
  "["  { \s -> TokenLBR }
  "]"  { \s -> TokenRBR }
  "%"  { \s -> TokenMod }
  ","  { \s -> TokenComma }
  ";"  { \s -> TokenSC }
  "="  { \s -> TokenAssign }
  "."  { \s -> TokenDot }
  "!"  { \s -> TokenNot }
  "|"  { \s -> TokenPipe }
  ":"  { \s -> TokenColon }
  "+"  { \s -> TokenPlus }
  "-"  { \s -> TokenMinus }
  "*"  { \s -> TokenMul }
  "/"  { \s -> TokenDiv }
  "==" { \s -> TokenEq }
  "!=" { \s -> TokenNotEq }
  "<"  { \s -> TokenLT }
  ">"  { \s -> TokenGT }
  "<=" { \s -> TokenLTE }
  ">=" { \s -> TokenGTE }
  "&&" { \s -> TokenAnd }
  "||" { \s -> TokenOr }

  $digit+     { \s -> TokenInt (read s) }

  "bool"      { \s -> TokenBoolType  }
  "int"       { \s -> TokenIntType }
  "type"      { \s -> TokenType }
  "array of"  { \s -> TokenArray }
  "record of" { \s -> TokenRecord }
  "return"    { \s -> TokenReturn }
  "write"     { \s -> TokenWrite }
  "var"       { \s -> TokenVar }
  "if"        { \s -> TokenIf }
  "then"      { \s -> TokenThen }
  "else"      { \s -> TokenElse }
  "while"     { \s -> TokenWhile }
  "do"        { \s -> TokenDo }
  "allocate"  { \s -> TokenAllocate }
  "of length" { \s -> TokenOfLength }
  "true"      { \s -> TokenTrue }
  "false"     { \s -> TokenFalse }
  "func"      { \s -> TokenFunc }
  "end"       { \s -> TokenEnd }

  $alpha [$alpha $digit \_ \']*		{ \s -> TokenId s }

{
-- Each action has type :: String -> Token

lexer :: String -> [Token]
lexer = alexScanTokens

-- The token type:
data Token
    -- An integer
    = TokenInt Integer
    -- Assignment: =
    | TokenAssign
    -- Comma: ,
    | TokenComma
    -- Dot: .
    | TokenDot
    -- Pipe: |
    | TokenPipe
    -- Addition operator: +
    | TokenPlus
    -- Subtraction operator: -
    | TokenMinus
    -- Multiplication operator: *
    | TokenMul
    -- Division operator: /
    | TokenDiv
    -- Modulo operator: %
    | TokenMod
    -- Comparison: ==
    | TokenEq
    -- Negated comparison: !=
    | TokenNotEq
    -- Less than: <
    | TokenLT
    -- Greater than: >
    | TokenGT
    -- Less than or equal <=
    | TokenLTE
    -- Greater than or equal: >=
    | TokenGTE
    -- Logical conjunction: &&
    | TokenAnd
    -- Logical disjunction: ||
    | TokenOr
    -- Logical negation: !
    | TokenNot
    -- Left bracket: [
    | TokenLBR
    -- Right bracket ]
    | TokenRBR
    -- Left bracket: {
    | TokenLB
    -- Right bracket }
    | TokenRB
    -- Left parenthesis: (
    | TokenLP
    -- Right parenthesis: )
    | TokenRP
    -- Semicolon
    | TokenSC
    -- Colon
    | TokenColon
    -- The keyword "var"
    | TokenVar
    -- The keyword "func"
    | TokenFunc
    -- The keyword "end"
    | TokenEnd
    -- The keyword "array of"
    | TokenArray
    -- The keyword "record of"
    | TokenRecord
    -- The keyword "int"
    | TokenIntType
    -- The keyword "bool"
    | TokenBoolType
    -- The keyword "write"
    | TokenWrite
    -- The keyword "return"
    | TokenReturn
    -- The keyword "do"
    | TokenDo
    -- The keyword "while"
    | TokenWhile
    -- The keyword "if"
    | TokenIf
    -- The keyword "then"
    | TokenThen
    -- The keyword "else"
    | TokenElse
    -- The keyword "allocate"
    | TokenAllocate
    -- The keyword "type"
    | TokenType
    -- The keywords "of length"
    | TokenOfLength
    -- The boolean constants
    | TokenTrue
    | TokenFalse
    -- The null constant
    | TokenNull
    -- An identifier
    | TokenId String
    deriving (Show, Eq)

}
