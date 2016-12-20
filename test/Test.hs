module Test where

import Test.Tasty
import Test.Tasty.HUnit

import Util

main :: IO ()
main = defaultMain $ do
    testGroup "Parsing tests" (map (\x -> testCase x $ assertHasVowels x) testFiles)

hasVowels = any (`elem` "e")

assertHasVowels = assertBool "hasVowels" . hasVowels
