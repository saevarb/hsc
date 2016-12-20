module Test where

import Test.Tasty
import Test.Tasty.HUnit


import Parser
import TestUtil

main :: IO ()
main = defaultMain $ do
    testGroup "Parsing tests" (map canParse testFiles)

canParse f =
    testCase f $ do
        contents <- readFile ("test/data/" ++ f)
        let res = parseCode contents
        case res of
            Left e -> assertFailure e
            Right _ -> return ()
    
