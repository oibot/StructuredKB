module Main where

import Test.Tasty
import Test.Tasty.HUnit as HU

import Logic

testDisjunctionOutput :: TestTree
testDisjunctionOutput = HU.testCase "Output of a or b" $
    "a + b" HU.@=? show (Disjunction (Atom "a") (Atom "b"))

testConjunctionOutput :: TestTree
testConjunctionOutput = HU.testCase "Output of a and b" $
    "a*b" HU.@=? show (Conjunction (Atom "a") (Atom "b"))

testImplicationOutput :: TestTree
testImplicationOutput = HU.testCase "Output of a implies b" $
    "a --> b" HU.@=? show (Implication (Atom "a") (Atom "b"))

testEqualityOutput :: TestTree
testEqualityOutput = HU.testCase "Output of a equals b" $
    "a <--> b" HU.@=? show (Equality (Atom "a") (Atom "b"))

testNegationOutput :: TestTree
testNegationOutput = HU.testCase "Output of a negated" $
    "-a" HU.@=? show (Negation (Atom "a"))

allTests :: TestTree
allTests = testGroup "All Tests" [
      testGroup "Logic" [ testDisjunctionOutput
                        , testConjunctionOutput
                        , testImplicationOutput
                        , testEqualityOutput
                        , testNegationOutput
                        ]
    ]

main :: IO ()
main = defaultMain allTests
