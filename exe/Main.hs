module Main where
import Parser
import Lexer

import Types
import Util

main :: IO ()
main = do
    contents <- getContents
    let parsed = runAlex contents parseProgram
    -- putStrLn "Lexed:"
    -- print lexed
    case parsed of
        Right t -> do
            visualizeAndShow t
            putStrLn "============"
            printTree t
        Left err ->
            putStr err
  where
    printTree (Body decls stmts) = do
        putStrLn "\n"
        mapM_ pprint decls
        putStrLn "---"
        mapM_ pprint stmts
    pprint :: Show a => a -> IO ()
    pprint = putStrLn . ("\t" ++) . show
