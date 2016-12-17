{
module Lexer where

import Control.Monad.Trans
}

%wrapper "monadUserState"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$white = [\t ]

tokens :-

--  \n   { incNewlines }
  $white+	;
  "#".*		;
  "("  { just TokenLP }
  ")"  { just TokenRP }
  "{"  { just TokenLB }
  "}"  { just TokenRB }
  "["  { just TokenLBR }
  "]"  { just TokenRBR }
  "%"  { just TokenMod }
  ","  { just TokenComma }
  ";"  { just TokenSC }
  "="  { just TokenAssign }
  "."  { just TokenDot }
  "!"  { just TokenNot }
  "|"  { just TokenPipe }
  ":"  { just TokenColon }
  "+"  { just TokenPlus }
  "-"  { just TokenMinus }
  "*"  { just TokenMul }
  "/"  { just TokenDiv }
  "==" { just TokenEq }
  "!=" { just TokenNotEq }
  "<"  { just TokenLT }
  ">"  { just TokenGT }
  "<=" { just TokenLTE }
  ">=" { just TokenGTE }
  "&&" { just TokenAnd }
  "||" { just TokenOr }

  $digit+     { token $ \ (_, _, _, s) i -> TokenInt (read s) }

  "bool"      { just TokenBoolType  }
  "int"       { just TokenIntType }
  "type"      { just TokenType }
  "array of"  { just TokenArray }
  "record of" { just TokenRecord }
  "return"    { just TokenReturn }
  "write"     { just TokenWrite }
  "var"       { just TokenVar }
  "if"        { just TokenIf }
  "then"      { just TokenThen }
  "else"      { just TokenElse }
  "while"     { just TokenWhile }
  "do"        { just TokenDo }
  "allocate"  { just TokenAllocate }
  "of length" { just TokenOfLength }
  "true"      { just TokenTrue }
  "false"     { just TokenFalse }
  "func"      { just TokenFunc }
  "end"       { just TokenEnd }

  $alpha [$alpha $digit \_ \']*		{ token $ \ (_, _, _, s) i -> TokenId s }

{
-- Each action has type :: String -> Token

data AlexUserState
    = AlexUserState
    { newlines :: Int
    }

alexInitUserState = AlexUserState 0

alexEOF = return TokenEOF

-- Why doesn't `token` do this? What is it even useful for?
just :: Token -> AlexInput -> Int -> Alex Token
just t _ _ = return t

lexer :: (Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)



/* getNewlines :: Alex Int */
/* getNewlines = newlines <$> alexGetUserState */

/* setNewlines :: Int -> Alex () */
/* setNewlines n = do */
/*     s <- alexGetUserState */
/*     alexSetUserState $ s { newlines = n} */

/* incNewlines :: Alex () */
/* incNewlines = do */
/*     n <- getNewlines */
/*     setNewlines (n + 1) */

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
    | TokenEOF
    deriving (Show, Eq)

}
