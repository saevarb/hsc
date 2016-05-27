module Types where

parseError :: [Token] -> a
parseError _ = error "Parse error"

data BinOp
    = Add
    | Sub
    | Div
    | Mul
    deriving Show

data UnOp
    = LNot
    deriving Show

data Var
    = VarId String
    | VarOffset Var Exp
    | VarDot Var String
    deriving (Show)

data Exp
    = BinExp Exp BinOp Exp
    | UnExp Exp UnOp
    | VarExp
    | AbsExp Exp
    | ConstExp Int
    | BoolExp Bool
    | Null
    deriving Show

data Token
    -- An integer
    = TokenInt Integer
    -- Equals sign: =
    | TokenEq
    -- Plus operator
    | TokenPlus
    -- Subtraction operator
    | TokenMinus
    -- Multiplication operator
    | TokenMul
    -- Division operator
    | TokenDiv
    -- Logical conjunction: &&
    | TokenAnd
    -- Logical disjunction: ||
    | TokenOr
    -- Logical negation: !
    | TokenNot
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
    -- The keyword "return"
    | TokenReturn
    -- The keyword "if"
    | TokenIf
    -- The keyword "then"
    | TokenThen
    -- The keyword "else"
    | TokenElse
    -- An identifier
    | TokenId String
    deriving Show
