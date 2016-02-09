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

form2 = Implication atom1
                    (Conjunction (Negation atom2)
                                 atom3)

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

testValuationWithAtom :: TestTree
testValuationWithAtom = HU.testCase "Valuations for an atom" $
    [[("a", True)], [("a", False)]] HU.@=? valuations atom1

testValuationsLength :: TestTree
testValuationsLength = HU.testCase "Number of valuations" $
    (2*2*2*2) HU.@=? length (valuations form1)

testValuationNotSatisfiesAtom :: TestTree
testValuationNotSatisfiesAtom = HU.testCase "False does not satisfy a" $
    False HU.@=? satisfies atom1 [("a",False)]

testValuationSatisfiesAtom :: TestTree
testValuationSatisfiesAtom = HU.testCase "True does satisfy a" $
    True HU.@=? satisfies atom1 [("a", True)]

testValuationSatisfiesComplexFormula :: TestTree
testValuationSatisfiesComplexFormula =
    HU.testCase "[True, False, True] satisfies a --> -b * c" $
        True HU.@=? satisfies form2 [("a", True), ("b", False), ("c", True)]

testValuationNotSatisfiesComplexFormula :: TestTree
testValuationNotSatisfiesComplexFormula =
    HU.testCase "[True, True, True] does not satisfies a --> -b * c" $
        False HU.@=? satisfies form2 [("a", True), ("b", True), ("c", True)]


-- Main

allTests :: TestTree
allTests = testGroup "All Tests" [
      testGroup "Logic" [ testDisjunctionOutput
                        , testConjunctionOutput
                        , testImplicationOutput
                        , testEqualityOutput
                        , testNegationOutput
                        , testAtoms
                        , testValuationWithAtom
                        , testValuationsLength
                        , testValuationNotSatisfiesAtom
                        , testValuationSatisfiesAtom
                        , testValuationSatisfiesComplexFormula
                        , testValuationNotSatisfiesComplexFormula
                        ]
    ]

main :: IO ()
main = defaultMain allTests
