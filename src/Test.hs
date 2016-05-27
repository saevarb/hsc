module Test where

import           Control.Monad
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.String

spaceConsumer :: Parser ()
spaceConsumer =
    L.space
    (void $ some spaceChar)
    (L.skipLineComment "#")
    (L.skipBlockComment "(*" "*)")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

funcP :: Parser String
funcP = symbol "func"

idP :: Parser String
idP = lexeme (some letterChar)


-- This fails, makes sense. it parses "f" in foo inside funcP
-- and then hits 'o' and
test1 = parseTest (many $ funcP <|> idP) "func foo"
test2 = parseTest (many $ try funcP <|> idP) "func foo"
test3 = parseTest (many $ choice [funcP, idP]) "func foo"
test4 = parseTest (many $ choice $ map try [funcP, idP]) "func foo"
