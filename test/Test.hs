module Test where

import Test.Tasty
import Test.Tasty.HUnit


import Parser
import TestUtil

main :: IO ()
main = defaultMain $ do
    testGroup "Parsing tests"
        [ testGroup "Parsing valid files" (map canParse validTestFiles)
        , testGroup "Parsing invalid files" (map cantParse invalidTestFiles)
        ]

canParse f =
    testCase f $ do
        res <- parseFile ("test/data/" ++ f)
        assertRight id res
        return ()

cantParse f =
    testCase f $ do
        res <- parseFile ("test/data/" ++ f)
        assertLeft (const "should fail to parse") res
        return ()

assertRight _ (Right e) = return ()
assertRight f (Left e) = assertFailure $ f e

assertLeft _ (Left e) = return ()
assertLeft f (Right e) = assertFailure $ f e

parseFile f = do
        contents <- readFile f
        return $ parseCode contents
