module Types where

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp
    = Let String Exp Exp
    | Exp1 Exp1
    deriving Show

data Exp1
    = Plus Exp1 Term
    | Minus Exp1 Term
    | Term Term
    deriving Show

data Term
    = Times Term Factor
    | Div Term Factor
    | Factor Factor
    deriving Show

data Factor
    = Int Int
    | Var String
    | Brack Exp
    deriving Show

data Token
      = TokenLet
      | TokenIn
      | TokenInt Int
      | TokenVar String
      | TokenEq
      | TokenPlus
      | TokenMinus
      | TokenTimes
      | TokenDiv
      | TokenOB
      | TokenCB
 deriving Show
