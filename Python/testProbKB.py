import unittest
from probKB import *
from sympy import *
import numpy as np
import math

class testProbabilisticKnowledgebases(unittest.TestCase):

    def setUp(self):
        self.signature = symbols('x,y,z')
        self.x = self.signature[0]
        self.y = self.signature[1]
        self.z = self.signature[2]
        self.w1 = {self.x: True, self.y: False, self.z: False}
        self.w2 = {self.x: True, self.y: True, self.z: False}
        self.w3 = {self.x: False, self.y: False, self.z: False}
        self.rule1 = Rule(self.x, self.y, .8)
        self.rule2 = Rule(true, self.y, .8)

    def testWorldsWithSymbols(self):
        self.assertTrue(len(worlds(self.signature)) == 8)

    def testRuleConstruction(self):
        self.assertEqual(self.rule1.premise, self.x)
        self.assertEqual(self.rule1.conclusion, self.y)
        self.assertEqual(self.rule1.probability, .8)

    def testWorldSatisfiesAtom(self):
        self.assertTrue(satisfies(self.w1, self.x))

    def testWorldDoesNotSatisfyAtom(self):
        self.assertFalse(satisfies(self.w1, self.y))

    def testWorldSatisfiesTautology(self):
        self.assertTrue(satisfies(self.w1, true))

    def testWorldSatisfiesComplexFormula(self):
        self.assertTrue(satisfies(self.w1, self.x | self.y))

    def testWorldDoesNotSatisfiesComplexFormula(self):
        self.assertFalse(satisfies(self.w1, self.x & self.y))

    def testWorldSatisfiesRule(self):
        self.assertTrue(satisfies_rule(self.w2, self.rule1))

    def testWorldDoesNotSatisfiesRule(self):
        self.assertFalse(satisfies_rule(self.w1, self.rule1))

    def testWorldSatisfiesRuleWithEmptyPremise(self):
        self.assertTrue(satisfies_rule(self.w2, self.rule2))

    def testWorldDoesNotSatifiesRuleWithEmptyPremise(self):
        self.assertFalse(satisfies_rule(self.w1, self.rule2))

    def testRuleIsVerifiedInWorld(self):
        self.assertTrue(verified(self.rule1, self.w2))

    def testRuleIsNotVerifiedInWorld(self):
        self.assertFalse(verified(self.rule1, self.w1))

    def testRuleIsFalsifiedInWorld(self):
        self.assertTrue(falsified(self.rule1, self.w1))

    def testRuleIsNotFalsifiedInWorld(self):
        self.assertFalse(falsified(self.rule1, self.w2))

    def testRuleIsNeitherVerifiedNorFalsified(self):
        self.assertFalse(verified(self.rule1, self.w3))
        self.assertFalse(falsified(self.rule1, self.w3))

    def testEffectOnVerifiedWorld(self):
        eff = 1 - self.rule1.probability
        self.assertEqual(effect(self.rule1, self.w2), eff)

    def testEffectOnFalsifiedWorld(self):
        eff = -self.rule1.probability
        self.assertEqual(effect(self.rule1, self.w1), eff)

    def testEffectOnNeutralWorld(self):
        self.assertEqual(effect(self.rule1, self.w3), .0)

    def testSignatureFromKnowledgebase(self):
        kb = [self.rule1, self.rule2]
        s = set([self.x, self.y])
        self.assertEqual(signature(kb), s)
        self.assertEqual(len(s), 2)

    def testWorldsFromSignatureFromKnowledgebase(self):
        kb = [self.rule1, self.rule2]
        self.assertEqual(len(worlds(signature(kb))), 4)

    def testConstraintsMatrix(self):
        a, b = symbols('a,b')
        r1 = Rule(true, a, .8)
        r2 = Rule(true, b, .6)
        r3 = Rule(a, b, .9)
        kb = [r1, r2, r3]
        A = constraints_matrix(kb)
        ws = worlds(signature(kb))
        self.assertEqual(A.shape, (len(kb), len(ws)))

    def testVerifyingMatrix(self):
        a, b = symbols('a,b')
        r = Rule(true, a, 0.9)
        ws = worlds([a, b])
        v = np.array([1,1,0,0])
        self.assertTrue(np.array_equal(verifying_matrix(ws, r), v))

    def testFalsifyingMatrix(self):
        a, b = symbols('a,b')
        r = Rule(true, a, 0.9)
        ws = worlds([a, b])
        v = np.array([0,0,1,1])
        self.assertTrue(np.array_equal(falsifying_matrix(ws, r), v))

    def testQueryWith2Norm(self):
        # An example from Log4KR
        a, b = symbols('a,b')
        r1 = Rule(true, a, .8)
        r2 = Rule(true, b, .6)
        r3 = Rule(a, b, .9)
        kb = [r1, r2, r3]
        la, ua = query(r1, kb)
        lb, ub = query(r2, kb)
        lab, uab = query(r3, kb)
        self.assertTrue(math.isclose(la, 0.7615, abs_tol=1e-4))
        self.assertTrue(math.isclose(ua, 0.7615, abs_tol=1e-4))
        self.assertTrue(math.isclose(lb, 0.6427, abs_tol=1e-4))
        self.assertTrue(math.isclose(ub, 0.6427, abs_tol=1e-4))
        self.assertTrue(math.isclose(lab, 0.8439, abs_tol=1e-4))
        self.assertTrue(math.isclose(uab, 0.8439, abs_tol=1e-4))

    def testQueryWith1Norm(self):
        # An example from Log4KR
        a, b = symbols('a,b')
        r1 = Rule(true, a, .8)
        r2 = Rule(true, b, .6)
        r3 = Rule(a, b, .9)
        kb = [r1, r2, r3]
        la, ua = query(r1, kb, norm="1")
        lb, ub = query(r2, kb, norm="1")
        lab, uab = query(r3, kb, norm="1")
        self.assertTrue(math.isclose(la, 0.8, abs_tol=1e-2))
        self.assertTrue(math.isclose(ua, 0.8, abs_tol=1e-2))
        self.assertTrue(math.isclose(lb, 0.6, abs_tol=1e-2))
        self.assertTrue(math.isclose(ub, 0.72, abs_tol=1e-2))
        self.assertTrue(math.isclose(lab, 0.75, abs_tol=1e-2))
        self.assertTrue(math.isclose(uab, 0.9, abs_tol=1e-2))

    def testQueryWithInfNorm(self):
        # An example from Log4KR
        a, b = symbols('a,b')
        r1 = Rule(true, a, .8)
        r2 = Rule(true, b, .6)
        r3 = Rule(a, b, .9)
        kb = [r1, r2, r3]
        la, ua = query(r1, kb, norm="inf")
        lb, ub = query(r2, kb, norm="inf")
        lab, uab = query(r3, kb, norm="inf")
        self.assertTrue(math.isclose(la, 0.7586, abs_tol=1e-4))
        self.assertTrue(math.isclose(ua, 0.7586, abs_tol=1e-4))
        self.assertTrue(math.isclose(lb, 0.6413, abs_tol=1e-4))
        self.assertTrue(math.isclose(ub, 0.6413, abs_tol=1e-4))
        self.assertTrue(math.isclose(lab, 0.8454, abs_tol=1e-4))
        self.assertTrue(math.isclose(uab, 0.8454, abs_tol=1e-4))


if __name__ == "__main__":
    unittest.main()
