module Main where

import Test.Tasty
import Test.Tasty.HUnit as HU

import Logic
import Data.List

atom1 = Atom "a"
atom2 = Atom "b"
atom3 = Atom "c"
atom4 = Atom "d"

form1 = Implication (Negation atom1)
                    (Conjunction (Disjunction atom2 atom3)
                                 atom4)

-- Unit test

testDisjunctionOutput :: TestTree
testDisjunctionOutput = HU.testCase "Output of a or b" $
    "(a + b)" HU.@=? show (Disjunction (Atom "a") (Atom "b"))

testConjunctionOutput :: TestTree
testConjunctionOutput = HU.testCase "Output of a and b" $
    "(a * b)" HU.@=? show (Conjunction (Atom "a") (Atom "b"))

testImplicationOutput :: TestTree
testImplicationOutput = HU.testCase "Output of a implies b" $
    "(a --> b)" HU.@=? show (Implication (Atom "a") (Atom "b"))

testEqualityOutput :: TestTree
testEqualityOutput = HU.testCase "Output of a equals b" $
    "(a <--> b)" HU.@=? show (Equality (Atom "a") (Atom "b"))

testNegationOutput :: TestTree
testNegationOutput = HU.testCase "Output of a negated" $
    "(-a)" HU.@=? show (Negation (Atom "a"))

testAtoms :: TestTree
testAtoms = HU.testCase "List of atoms" $
    ["a", "b", "c", "d"] HU.@=? sort (atoms form1)


-- Main

allTests :: TestTree
allTests = testGroup "All Tests" [
      testGroup "Logic" [ testDisjunctionOutput
                        , testConjunctionOutput
                        , testImplicationOutput
                        , testEqualityOutput
                        , testNegationOutput
                        , testAtoms
                        ]
    ]

main :: IO ()
main = defaultMain allTests
