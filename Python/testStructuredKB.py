import unittest
from sympy import *
from structuredKB import *
import math

class TestStructuredKBsWithExample(unittest.TestCase):

    def setUp(self):
        # Test with small signature
        self.signature1 = symbols('x,y,z')
        self.x = self.signature1[0]
        self.y = self.signature1[1]
        self.z = self.signature1[2]
        self.w1 = {self.x: True, self.y: False, self.z: False}
        self.w2 = {self.x: True, self.y: True, self.z: False}
        self.w3 = {self.x: False, self.y: False, self.z: False}
        self.rule1 = Rule(self.x, self.y, .8)
        self.rule2 = Rule(true, self.y, .8)

        # Test example from Log4KR
        sym = ("ga_af1, ga_af2, ga_bf1, ga_bf2,"
               "b_a, b_b,"
               "e_a, e_b,"
               "ex_a, ex_b,"
               "c_f1, c_f2")
        self.signature2 = symbols(sym)
        self.exec_alice = self.signature2[8]
        self.exec_bob = self.signature2[9]
        self.employee_alice = self.signature2[6]
        self.employee_bob = self.signature2[7]
        self.blacklisted_alice = self.signature2[4]
        self.blacklisted_bob = self.signature2[5]
        self.access_alice_f1 = self.signature2[0]
        self.access_alice_f2 = self.signature2[1]
        self.access_bob_f1 = self.signature2[2]
        self.access_bob_f2 = self.signature2[3]
        self.confidential_f1 = self.signature2[10]
        self.confidential_f2 = self.signature2[11]

        r1 = Rule(self.exec_alice, self.employee_alice, 1.0)
        r2 = Rule(self.exec_bob, self.employee_bob, 1.0)
        r3 = Rule(self.blacklisted_alice, self.access_alice_f1, 0.0)
        r4 = Rule(self.blacklisted_alice, self.access_alice_f2, 0.0)
        r5 = Rule(self.blacklisted_bob, self.access_bob_f1, 0.0)
        r6 = Rule(self.blacklisted_bob, self.access_bob_f2, 0.0)
        ic = [r1, r2, r3, r4, r5, r6]


    def testSignatureCount(self):
        self.assertEqual(len(self.signature1), 3)
        self.assertEqual(len(self.signature2), 12)

    def testAtoms(self):
        self.assertEqual(str(self.exec_alice), "ex_a")
        self.assertEqual(str(self.exec_bob), "ex_b")
        self.assertEqual(str(self.employee_alice), "e_a")
        self.assertEqual(str(self.employee_bob), "e_b")
        self.assertEqual(str(self.blacklisted_alice), "b_a")
        self.assertEqual(str(self.blacklisted_bob), "b_b")
        self.assertEqual(str(self.access_alice_f1), "ga_af1")
        self.assertEqual(str(self.access_alice_f2), "ga_af2")
        self.assertEqual(str(self.access_bob_f1), "ga_bf1")
        self.assertEqual(str(self.access_bob_f2), "ga_bf2")
        self.assertEqual(str(self.confidential_f1), "c_f1")
        self.assertEqual(str(self.confidential_f2), "c_f2")

    def testWorldsWithSymbols(self):
        self.assertTrue(len(worlds(self.signature1) ) == 8)
        self.assertTrue(len(worlds(self.signature2)) == 4096)

    def testRuleConstruction(self):
        x, y = symbols('x, y')
        rule = Rule(x, y, .8)
        self.assertEqual(rule.premise, x)
        self.assertEqual(rule.conclusion, y)
        self.assertEqual(rule.probability, .8)

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

    def testSignatureFromKnowledgebaseOnlyAtoms(self):
        kb = [self.rule1, self.rule2]
        s = set([self.x, self.y])
        self.assertEqual(signature(kb), s)

    def testSignatureFromKnowledgebaseComplexFormulas(self):
        r = Rule(true, self.x & self.y, .1)
        s = set([self.x, self.y])
        self.assertEqual(signature([r]), s)

    def testWorldsFromSignatureFromKnowledgebase(self):
        kb = [self.rule1, self.rule2]
        self.assertEqual(len(worlds(signature(kb))), 4)

    def testConstraintsMatrix(self):
        a, b = symbols('a,b')
        r1 = Rule(true, a, .8)
        r2 = Rule(true, b, .6)
        r3 = Rule(a, b, .9)
        kb = [r1, r2, r3]
        ws = worlds(signature(kb))
        _, As = constraints_matrices(ws, [kb])
        self.assertEqual(As[0].shape, (len(kb), len(ws)))
        IC, _ = constraints_matrices(ws, [kb], ic=kb)
        self.assertEqual(IC.shape, (len(kb), len(ws)))

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

    def testConstraintMatrixHelper(self):
        M = np.array([[2,2],
                      [2,2],
                      [4,4]])
        wf = lambda x: 2*x
        As = [np.ones((2,2), dtype=np.int), np.ones((1,2), dtype=np.int)]
        A = constraintMat(As, wf)
        self.assertTrue(np.array_equal(A, M))

    def testViolation1(self):
        a, b = symbols('a,b')
        r1 = Rule(true, a, .8)
        r2 = Rule(true, b, .6)
        r3 = Rule(a, b, .9)
        kb = [r1, r2, r3]
        ws = worlds(signature(kb))
        _, As = constraints_matrices(ws, [kb])
        A = constraintMat(As)
        incm, incv = violation(ws, A)
        self.assertTrue(math.isclose(incm, 0.07158, abs_tol=1e-5))
        self.assertEqual(incv.shape, (len(kb), 1))


    def testViolation2(self):
        a, b = symbols('a,b')
        r1 = Rule(true, a, .75)
        r2 = Rule(true, a, .5)
        ws = worlds(set([a,b]))
        kb = [r1, r2]
        _, As = constraints_matrices(ws, [kb])
        A = constraintMat(As)
        incm, incv = violation(ws, A)
        self.assertTrue(math.isclose(incm, .17677, abs_tol=1e-5))
        self.assertEqual(incv.shape, (len(kb), 1))

    def testViolation3(self):
        canFly, bird, penguin = symbols('canFly, bird, penguin')
        r1 = Rule(bird, canFly, 1.0)
        r2 = Rule(penguin, canFly, 0.0)
        r3 = Rule(penguin, bird, 1.0)
        kb = [r1, r2, r3]
        ws = worlds(signature(kb))
        _, As = constraints_matrices(ws, [kb])
        A = constraintMat(As)
        incm, incv = violation(ws, A)
        self.assertTrue(math.isclose(incm, 0.0, abs_tol=1e-5))
        self.assertEqual(incv.shape, (len(kb), 1))

    def testViolation4(self):
        canFly, bird, penguin = symbols('canFly, bird, penguin')
        r1 = Rule(bird, canFly, 1.0)
        r2 = Rule(penguin, canFly, 0.0)
        r3 = Rule(penguin, bird, 1.0)
        r4 = Rule(true, penguin, 1.0)
        kb = [r1, r2, r3, r4]
        ws = worlds(signature(kb))
        _, As = constraints_matrices(ws, [kb])
        A = constraintMat(As)
        incm, incv = violation(ws, A)
        self.assertTrue(math.isclose(incm, 0.5, abs_tol=1e-5))
        self.assertEqual(incv.shape, (len(kb), 1))

    # def testViolation5(self):
        r1 = Rule(true, self.access_alice_f1, 0.0)
        r2 = Rule(true, self.access_alice_f2, 0.0)
        r3 = Rule(true, self.access_bob_f1, 0.0)
        r4 = Rule(true, self.access_bob_f2, 0.0)
        r5 = Rule(true, self.blacklisted_bob, 0.05)
        r6 = Rule(true, self.blacklisted_alice, 0.05)
        kb1 = [r1, r2, r3, r4, r5, r6]

        r7 = Rule(self.employee_alice, self.access_alice_f1, 0.5)
        r8 = Rule(self.employee_alice, self.access_alice_f2, 0.5)
        r9 = Rule(self.employee_bob, self.access_bob_f1, 0.5)
        r10 = Rule(self.employee_bob, self.access_bob_f2, 0.5)
        r11 = Rule(self.employee_alice, self.blacklisted_alice, 0.01)
        r12 = Rule(self.employee_bob, self.blacklisted_bob, 0.01)
        kb2 = [r7, r8, r9, r10, r11, r12]

        r13 = Rule(self.confidential_f1, self.access_alice_f1, 0.0)
        r14 = Rule(self.confidential_f2, self.access_alice_f2, 0.0)
        r15 = Rule(self.confidential_f1, self.access_bob_f1, 0.0)
        r16 = Rule(self.confidential_f2, self.access_bob_f2, 0.0)
        kb3 = [r13, r14, r15, r16]

        r17 = Rule(self.exec_alice, self.access_alice_f1, 0.7)
        r18 = Rule(self.exec_alice, self.access_alice_f2, 0.7)
        r19 = Rule(self.exec_bob, self.access_bob_f1, 0.7)
        r20 = Rule(self.exec_bob, self.access_bob_f2, 0.7)
        r21 = Rule(self.exec_bob, self.blacklisted_bob, 0.001)
        r22 = Rule(self.exec_alice, self.blacklisted_alice, 0.001)
        kb4 = [r17, r18, r19, r20, r21, r22]

        r23 = Rule(true, self.exec_alice, 1.0)
        r24 = Rule(true, self.employee_bob, 1.0)
        r25 = Rule(true, self.confidential_f1, 1.0)
        kb5 = [r23, r24, r25]

        r26 = Rule(self.exec_alice, self.employee_alice, 1.0)
        r27 = Rule(self.exec_bob, self.employee_bob, 1.0)
        r28 = Rule(self.blacklisted_alice, self.access_alice_f1, 0.0)
        r29 = Rule(self.blacklisted_alice, self.access_alice_f2, 0.0)
        r30 = Rule(self.blacklisted_bob, self.access_bob_f1, 0.0)
        r31 = Rule(self.blacklisted_bob, self.access_bob_f2, 0.0)
        ic = [r26, r27, r28, r29, r30, r31]

        wf = lambda x: 2*x
        # ws = worlds(self.signature2)
        # IC, As = constraints_matrices(ws, [kb1, kb2, kb3, kb4, kb5], ic)
        # incm, incv = violation(ws, As, IC, wf=wf)
        # print(incm)
        KB = [kb1, kb2, kb3, kb4, kb5]
        q = Rule(true, self.access_alice_f1, 0.0)
        l, u = query(q, KB, ic, wf=wf)
        print(l)
        print(u)






    # def testWith_IC_KB5_W2(self):

    #     q_bl_a = Rule(true, self.blacklisted_alice, 0.0)
    #     q_bl_b = Rule(true, self.blacklisted_bob, 0.0)
    #     q_ac_af1 = Rule(true, self.access_alice_f1, 0.0)
    #     q_ac_af2 = Rule(true, self.access_alice_f2, 0.0)
    #     q_ac_bf1 = Rule(true, self.access_bob_f1, 0.0)
    #     q_ac_bf2 = Rule(true, self.access_bob_f2, 0.0)

    #     l1, u1 = query(q_ac_af1, kbs)
    #     self.assertEqual(l1, 0)
    #     self.assertEqual(l2, 1)

if __name__ == "__main__":
    unittest.main()
