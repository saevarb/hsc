module Main where
import Parser
import Lexer

main :: IO ()
main = do
    contents <- getContents
    let parsed = runAlex contents parseProgram
    -- putStrLn "Lexed:"
    -- print lexed
    putStrLn "Parsed:"
    print parsed
    -- print . parseProgram $ lexed
    
