module Lexer where

import           Control.Monad
import Data.Either

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.String

-- import           Types

data Token
    -- An integer
    = TokenInt Integer
    -- Equals sign: =
    | TokenEq
    -- Comma: ,
    | TokenComma
    -- Pipe: |
    | TokenPipe
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
    -- The keyword "write"
    | TokenWrite
    -- The keyword "return"
    | TokenReturn
    -- The keyword "if"
    | TokenIf
    -- The keyword "then"
    | TokenThen
    -- The keyword "else"
    | TokenElse
    -- The keyword "allocate"
    | TokenAllocate
    -- The keywords "of length"
    | TokenOfLength
    -- The boolean constants
    | TokenTrue
    | TokenFalse
    -- The null constant
    | TokenNull
    -- An identifier
    | TokenId String
    deriving Show

parseError :: [Token] -> a
parseError xs = error $ "Parse error: " ++ show xs

spaceConsumer :: Parser ()
spaceConsumer =
    L.space
    (void $ spaceChar)
    (L.skipLineComment "#")
    (L.skipBlockComment "(*" "*)")

test :: FilePath -> IO ()
test file = do
    contents <- readFile file
    let res = parse (spaceConsumer >> many (choice $ map try allParsers)) "" contents
    case res of
        Left e -> print e
        Right v -> mapM_ print v

lexer :: String -> [Token]
lexer str = do
    let res = parse (spaceConsumer >> many (choice $ map try allParsers)) "" str
    case res of
        Left e -> error $ show e
        Right v -> v

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
    , trueP
    , falseP
    , nullP
    , writeP
    , ofLengthP
    , orP
    , andP
    , notP
    , ifP
    , thenP
    , elseP
    , allocateP
    , commaP
    , pipeP
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

trueP :: Parser Token
trueP = symbol "true" >> return TokenTrue

falseP :: Parser Token
falseP = symbol "false" >> return TokenFalse

nullP :: Parser Token
nullP = symbol "null" >> return TokenNull

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

writeP :: Parser Token
writeP = symbol "write" >> return TokenWrite

allocateP :: Parser Token
allocateP = symbol "allocate" >> return TokenAllocate

ofLengthP :: Parser Token
ofLengthP = symbol "of length" >> return TokenOfLength

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
divP = symbol "*" >> return TokenMul

mulP :: Parser Token
mulP = symbol "/" >> return TokenDiv

orP :: Parser Token
orP = symbol "||" >> return TokenOr

andP :: Parser Token
andP = symbol "&&" >> return TokenAnd

notP :: Parser Token
notP = symbol "!" >> return TokenNot

commaP :: Parser Token
commaP = symbol "," >> return TokenComma

pipeP :: Parser Token
pipeP = symbol "|" >> return TokenPipe


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
