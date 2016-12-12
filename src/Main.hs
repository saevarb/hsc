module Main where
import Parser
import Lexer

main :: IO ()
main = do
    contents <- getContents
    let lexed = lexer contents
    putStrLn "Lexed:"
    print lexed
    -- putStrLn "Parsed:"
    -- print . parseProgram $ lexed
