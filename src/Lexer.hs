module Lexer where

import           Control.Monad
import Data.Either

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.String

import           Types


spaceConsumer :: Parser ()
spaceConsumer =
    L.space
    (void $ spaceChar)
    (L.skipLineComment "#")
    (L.skipBlockComment "(*" "*)")

-- lex :: String -> [Token]
test file = do
    file <- readFile file
    let res = parse (spaceConsumer >> many (choice $ map try allParsers)) "" file
    case res of
        Left e -> print e
        Right v -> mapM_ print v

allParsers :: [Parser Token]
allParsers =
    [ arrayP
    , recordP
    , numP
    , eqP
    , plusP
    , minusP
    , mulP
    , divP
    , lbP
    , rbP
    , lpP
    , rpP
    , semicolonP
    , colonP
    , varP
    , funcP
    , endP
    , intP
    , boolP
    , returnP
    , orP
    , andP
    , notP
    , ifP
    , thenP
    , elseP
    -- Must be last
    , idP
    ]




-- Utility functions
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

-- Parser definitions
idP :: Parser Token
idP = TokenId <$> lexeme (some letterChar)

numP :: Parser Token
numP = TokenInt <$> lexeme L.integer

semicolonP :: Parser Token
semicolonP = symbol ";" >> return TokenSC

colonP :: Parser Token
colonP = symbol ":" >> return TokenColon

eqP :: Parser Token
eqP = symbol "=" >> return TokenEq

lpP :: Parser Token
lpP = symbol "(" >> return TokenLP

rpP :: Parser Token
rpP = symbol ")" >> return TokenRP

lbP :: Parser Token
lbP = symbol "{" >> return TokenLP

rbP :: Parser Token
rbP = symbol "}" >> return TokenRP

funcP :: Parser Token
funcP = symbol "func" >> return TokenFunc

endP :: Parser Token
endP = symbol "end" >> return TokenEnd

arrayP :: Parser Token
arrayP  = symbol "array of" >> return TokenArray

recordP :: Parser Token
recordP  = symbol "record of" >> return TokenRecord

varP :: Parser Token
varP = symbol "var" >> return TokenVar

intP :: Parser Token
intP = symbol "int" >> return TokenIntType

boolP :: Parser Token
boolP = symbol "bool" >> return TokenBoolType

returnP :: Parser Token
returnP = symbol "return" >> return TokenReturn

ifP :: Parser Token
ifP = symbol "if" >> return TokenIf

thenP :: Parser Token
thenP = symbol "then" >> return TokenThen

elseP :: Parser Token
elseP = symbol "else" >> return TokenElse

plusP :: Parser Token
plusP = symbol "+" >> return TokenPlus

minusP :: Parser Token
minusP = symbol "-" >> return TokenMinus

divP :: Parser Token
divP = symbol "*" >> return TokenDiv

mulP :: Parser Token
mulP = symbol "/" >> return TokenMul

orP :: Parser Token
orP = symbol "||" >> return TokenOr

andP :: Parser Token
andP = symbol "&&" >> return TokenAnd

notP :: Parser Token
notP = symbol "!" >> return TokenNot



-- Bogus function for reminding me if I'm missing
-- a parser for a token
bogus :: Token -> Parser Token
bogus TokenArray    = arrayP
bogus TokenRecord   = recordP
bogus (TokenInt _)  = numP
bogus (TokenId _)   = idP
bogus TokenEq       = eqP
bogus TokenPlus     = plusP
bogus TokenMinus    = minusP
bogus TokenMul      = mulP
bogus TokenDiv      = divP
bogus TokenLB       = lbP
bogus TokenRB       = rbP
bogus TokenLP       = lpP
bogus TokenRP       = rpP
bogus TokenSC       = semicolonP
bogus TokenColon    = colonP
bogus TokenVar      = varP
bogus TokenFunc     = funcP
bogus TokenEnd      = endP
bogus TokenIntType  = intP
bogus TokenBoolType = boolP
bogus TokenReturn   = returnP
bogus TokenOr       = orP
bogus TokenAnd      = andP
bogus TokenNot      = notP
bogus TokenIf       = ifP
bogus TokenThen     = thenP
bogus TokenElse     = elseP
