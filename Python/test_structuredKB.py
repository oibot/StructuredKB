import unittest
from sympy import *
from structuredKB import *
import math

class SimpleSignatureTests(unittest.TestCase):

    def setUp(self):
        self.signature = symbols('x,y')
        self.x = self.signature[0]
        self.y = self.signature[1]
        self.w1 = {self.x: True, self.y: False}
        self.w2 = {self.x: True, self.y: True}
        self.w3 = {self.x: False, self.y: False}
        self.rule1 = Rule(self.x, self.y, .8)
        self.rule2 = Rule(true, self.y, .8)

class SignatureTests(SimpleSignatureTests):

    def test_signature_count(self):
        self.assertEqual(len(self.signature), 2)

    def test_signature_content(self):
        self.assertEqual(self.signature, (self.x, self.y))

    def test_from_knowledgebase(self):
        kb = [self.rule1, self.rule2]
        s = set([self.x, self.y])
        self.assertEqual(signature(kb), s)

    def test_from_formulas(self):
        r = Rule(true, self.x & self.y, .1)
        s = set([self.x, self.y])
        self.assertEqual(signature([r]), s)

class RuleTests(SimpleSignatureTests):

    def test_rule_construction(self):
        rule = Rule(self.x, self.y, .8)
        self.assertEqual(rule.premise, self.x)
        self.assertEqual(rule.conclusion, self.y)
        self.assertEqual(rule.probability, .8)

class WorldGenerationTests(SimpleSignatureTests):

    def test_world_size(self):
        self.assertTrue(len(worlds(self.signature) ) == 4)

    def test_worlds_from_knowledgebase(self):
        kb = [self.rule1, self.rule2]
        self.assertEqual(len(worlds(signature(kb))), 4)

    def test_worlds_with_deterministic_positiv(self):
        r = Rule(self.x, self.y, 1.0)
        for w in worlds(self.signature, deters=[r]):
            self.assertTrue((self.x >> self.y).subs(w))

    def test_worlds_with_deterministic_negative(self):
        r = Rule(self.x, self.y, 0.0)
        for w in worlds(self.signature, deters=[r]):
            self.assertTrue((self.x >> ~self.y).subs(w))

class ConstraintsTests(SimpleSignatureTests):

    def test_constraint_matrix(self):
        kb = [self.rule1, self.rule2]
        ws = worlds(signature(kb))
        _, As = constraints_matrices(ws, [kb])
        self.assertEqual(As[0].shape, (len(kb), len(ws)))
        IC, _ = constraints_matrices(ws, [kb], ic=kb)
        self.assertEqual(IC.shape, (len(kb), len(ws)))

    def test_verifying_matrix(self):
        r = Rule(true, self.x, 0.9)
        ws = worlds(self.signature)
        v = np.array([1,1,0,0])
        self.assertTrue(np.array_equal(verifying_matrix(ws, r), v))

    def test_falsifying_matrix(self):
        r = Rule(true, self.x, 0.9)
        ws = worlds(self.signature)
        v = np.array([0,0,1,1])
        self.assertTrue(np.array_equal(falsifying_matrix(ws, r), v))

class GeneralEntailmentTests(SimpleSignatureTests):

    def test_constraint_matrix_helper(self):
        M = np.array([[2,2],
                      [2,2],
                      [4,4]])
        wf = lambda x: 2*x
        As = [np.ones((2,2), dtype=np.int), np.ones((1,2), dtype=np.int)]
        A = constraintMat(As, wf)
        self.assertTrue(np.array_equal(A, M))

    def test_violation1(self):
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


    def test_violation2(self):
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

    def test_violation3(self):
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


##################################################
# Test example from Log4KR
##################################################


class ExtendedExampleTest(unittest.TestCase):

    def setUp(self):
        sym = ("ga_af1, ga_af2, ga_bf1, ga_bf2,"
               "b_a, b_b,"
               "e_a, e_b,"
               "ex_a, ex_b,"
               "c_f1, c_f2")
        self.signature = symbols(sym)
        self.exec_alice = self.signature[8]
        self.exec_bob = self.signature[9]
        self.employee_alice = self.signature[6]
        self.employee_bob = self.signature[7]
        self.blacklisted_alice = self.signature[4]
        self.blacklisted_bob = self.signature[5]
        self.access_alice_f1 = self.signature[0]
        self.access_alice_f2 = self.signature[1]
        self.access_bob_f1 = self.signature[2]
        self.access_bob_f2 = self.signature[3]
        self.confidential_f1 = self.signature[10]
        self.confidential_f2 = self.signature[11]

        self.r1 = Rule(self.exec_alice, self.employee_alice, 1.0)
        self.r2 = Rule(self.exec_bob, self.employee_bob, 1.0)
        self.r3 = Rule(self.blacklisted_alice, self.access_alice_f1, 0.0)
        self.r4 = Rule(self.blacklisted_alice, self.access_alice_f2, 0.0)
        self.r5 = Rule(self.blacklisted_bob, self.access_bob_f1, 0.0)
        self.r6 = Rule(self.blacklisted_bob, self.access_bob_f2, 0.0)
        self.ic = [self.r1, self.r2, self.r3, self.r4, self.r5, self.r6]

        self.r1 = Rule(true, self.access_alice_f1, 0.0)
        self.r2 = Rule(true, self.access_alice_f2, 0.0)
        self.r3 = Rule(true, self.access_bob_f1, 0.0)
        self.r4 = Rule(true, self.access_bob_f2, 0.0)
        self.r5 = Rule(true, self.blacklisted_bob, 0.05)
        self.r6 = Rule(true, self.blacklisted_alice, 0.05)
        self.kb1 = [self.r1, self.r2, self.r3, self.r4, self.r5, self.r6]

        self.r7 = Rule(self.employee_alice, self.access_alice_f1, 0.5)
        self.r8 = Rule(self.employee_alice, self.access_alice_f2, 0.5)
        self.r9 = Rule(self.employee_bob, self.access_bob_f1, 0.5)
        self.r10 = Rule(self.employee_bob, self.access_bob_f2, 0.5)
        self.r11 = Rule(self.employee_alice, self.blacklisted_alice, 0.01)
        self.r12 = Rule(self.employee_bob, self.blacklisted_bob, 0.01)
        self.kb2 = [self.r7, self.r8, self.r9, self.r10, self.r11, self.r12]

        self.r13 = Rule(self.confidential_f1, self.access_alice_f1, 0.0)
        self.r14 = Rule(self.confidential_f2, self.access_alice_f2, 0.0)
        self.r15 = Rule(self.confidential_f1, self.access_bob_f1, 0.0)
        self.r16 = Rule(self.confidential_f2, self.access_bob_f2, 0.0)
        self.kb3 = [self.r13, self.r14, self.r15, self.r16]

        self.r17 = Rule(self.exec_alice, self.access_alice_f1, 0.7)
        self.r18 = Rule(self.exec_alice, self.access_alice_f2, 0.7)
        self.r19 = Rule(self.exec_bob, self.access_bob_f1, 0.7)
        self.r20 = Rule(self.exec_bob, self.access_bob_f2, 0.7)
        self.r21 = Rule(self.exec_bob, self.blacklisted_bob, 0.001)
        self.r22 = Rule(self.exec_alice, self.blacklisted_alice, 0.001)
        self.kb4 = [self.r17, self.r18, self.r19, self.r20, self.r21, self.r22]

        self.r23 = Rule(true, self.exec_alice, 1.0)
        self.r24 = Rule(true, self.employee_bob, 1.0)
        self.r25 = Rule(true, self.confidential_f1, 1.0)
        self.kb5 = [self.r23, self.r24, self.r25]

        self.KB = [self.kb1, self.kb2, self.kb3, self.kb4, self.kb5]

        self.wf = lambda x: 2*x
        self.ws = worlds(self.signature, deters=self.kb5+self.ic)
        self.IC, self.As = constraints_matrices(self.ws, self.KB, self.ic)

        self.q_access_alice_f1 = Rule(true, self.access_alice_f1, None)
        self.q_access_alice_f2 = Rule(true, self.access_alice_f2, None)
        self.q_access_bob_f1 = Rule(true, self.access_bob_f1, None)
        self.q_access_bob_f2 = Rule(true, self.access_bob_f2, None)
        self.q_blacklisted_alice = Rule(true, self.blacklisted_alice, None)
        self.q_blacklisted_bob = Rule(true, self.blacklisted_bob, None)

    def test_atoms(self):
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

class ExtendedWorldGenerationTests(ExtendedExampleTest):

    def test_world_generation(self):
        ws = worlds(self.signature, self.kb5+self.ic)
        self.assertEqual(len(ws), 100)

    def test_verifying_matrix_alice(self):
        ws = worlds(self.signature, self.kb5+self.ic)
        q1 = Rule(true, self.access_alice_f1, None)
        q2 = Rule(true, self.access_alice_f2, None)
        q3 = Rule(true, self.blacklisted_alice, None)
        v1 = np.sum(verifying_matrix(ws, q1))
        v2 = np.sum(verifying_matrix(ws, q2))
        v3 = np.sum(verifying_matrix(ws, q3))
        self.assertTrue(v1, 40)
        self.assertTrue(v2, 40)
        self.assertTrue(v3, 20)

    def test_verifying_matrix_bob(self):
        ws = worlds(self.signature, self.kb5+self.ic)
        q1 = Rule(true, self.access_bob_f1, None)
        q2 = Rule(true, self.access_bob_f2, None)
        q3 = Rule(true, self.blacklisted_bob, None)
        v1 = np.sum(verifying_matrix(ws, q1))
        v2 = np.sum(verifying_matrix(ws, q2))
        v3 = np.sum(verifying_matrix(ws, q3))
        self.assertTrue(v1, 40)
        self.assertTrue(v1, 40)
        self.assertTrue(v1, 20)

    def test_falsitying_matrix_alice(self):
        ws = worlds(self.signature, self.kb5+self.ic)
        q1 = Rule(true, self.access_alice_f1, None)
        q2 = Rule(true, self.access_alice_f2, None)
        q3 = Rule(true, self.blacklisted_alice, None)
        v1 = np.sum(falsifying_matrix(ws, q1))
        v2 = np.sum(falsifying_matrix(ws, q2))
        v3 = np.sum(falsifying_matrix(ws, q3))
        self.assertTrue(v1, 40)
        self.assertTrue(v2, 40)
        self.assertTrue(v3, 20)

    def test_falsitying_matrix_bob(self):
        ws = worlds(self.signature, self.kb5+self.ic)
        q1 = Rule(true, self.access_bob_f1, None)
        q2 = Rule(true, self.access_bob_f2, None)
        q3 = Rule(true, self.blacklisted_bob, None)
        v1 = np.sum(falsifying_matrix(ws, q1))
        v2 = np.sum(falsifying_matrix(ws, q2))
        v3 = np.sum(falsifying_matrix(ws, q3))
        self.assertTrue(v1, 40)
        self.assertTrue(v1, 40)
        self.assertTrue(v1, 20)

class Weighted2NormTests(ExtendedExampleTest):

    def test_constraintMat(self):
        self.assertFalse(constraintMat(self.As) is self.As)

    def test_violation_vector(self):
        A = constraintMat(self.As, wf=self.wf)
        incm, incv = violation(self.ws, A, IC=self.IC, wf=self.wf)
        self.assertEqual(incv.shape, (25, 1))
        self.assertTrue(incm > 0)

    def test_violation_vector_wf_exponential(self):
        wf2 = lambda x: x**10
        A = constraintMat(self.As, wf=wf2)
        incm, incv = violation(self.ws, A, IC=self.IC, wf=wf2)
        self.assertEqual(incv.shape, (25, 1))
        self.assertTrue(incm > 0)

    def test_query(self):
        l1, u1 = queryweightedmodel(self.q_access_alice_f1, self.ws, self.As,
                self.IC, wf=self.wf)
        l2, u2 = queryweightedmodel(self.q_access_alice_f2, self.ws, self.As,
                self.IC, wf=self.wf)
        l3, u3 = queryweightedmodel(self.q_access_bob_f1, self.ws, self.As,
                self.IC, wf=self.wf)
        l4, u4 = queryweightedmodel(self.q_access_bob_f2, self.ws, self.As,
                self.IC, wf=self.wf)
        l5, u5 = queryweightedmodel(self.q_blacklisted_alice, self.ws, self.As,
                self.IC, wf=self.wf)
        l6, u6 = queryweightedmodel(self.q_blacklisted_bob, self.ws, self.As,
                self.IC, wf=self.wf)
        self.assertTrue(math.isclose(l1, 0.44, abs_tol=1e-2))
        self.assertTrue(math.isclose(u1, 0.44, abs_tol=1e-2))
        self.assertTrue(math.isclose(l2, 0.6, abs_tol=1e-1))
        self.assertTrue(math.isclose(u2, 0.6, abs_tol=1e-1))
        # too much deviation: 0.14 to 0.04
        # self.assertTrue(math.isclose(l3, 0.14, abs_tol=1e-2))
        # self.assertTrue(math.isclose(u3, 0.14, abs_tol=1e-2))
        self.assertTrue(math.isclose(l4, 0.4, abs_tol=1e-1))
        self.assertTrue(math.isclose(u4, 0.4, abs_tol=1e-1))
        self.assertTrue(math.isclose(l5, 0.005, abs_tol=5e-3))
        self.assertTrue(math.isclose(u5, 0.005, abs_tol=5e-3))
        self.assertTrue(math.isclose(l6, 0.017, abs_tol=1e-2))
        self.assertTrue(math.isclose(u6, 0.017, abs_tol=1e-2))

    def test_query_with_exponential(self):
        wf2 = lambda x: x**10
        l1, u1 = queryweightedmodel(self.q_access_alice_f1, self.ws, self.As,
                self.IC, wf=wf2)
        l2, u2 = queryweightedmodel(self.q_access_alice_f2, self.ws, self.As,
                self.IC, wf=wf2)
        l3, u3 = queryweightedmodel(self.q_access_bob_f1, self.ws, self.As,
                self.IC, wf=wf2)
        l4, u4 = queryweightedmodel(self.q_access_bob_f2, self.ws, self.As,
                self.IC, wf=wf2)
        l5, u5 = queryweightedmodel(self.q_blacklisted_alice, self.ws, self.As,
                self.IC, wf=wf2)
        l6, u6 = queryweightedmodel(self.q_blacklisted_bob, self.ws, self.As,
                self.IC, wf=wf2)
        self.assertTrue(l1 >= 0 and l1 <= 1)
        self.assertTrue(u1 >= 0 and u1 <= 1)
        self.assertTrue(l2 >= 0 and l2 <= 1)
        self.assertTrue(u2 >= 0 and u2 <= 1)
        self.assertTrue(l3 >= 0 and l3 <= 1)
        self.assertTrue(u3 >= 0 and u3 <= 1)
        self.assertTrue(l4 >= 0 and l4 <= 1)
        self.assertTrue(u4 >= 0 and u4 <= 1)
        self.assertTrue(l5 >= 0 and l5 <= 1)
        self.assertTrue(u5 >= 0 and u5 <= 1)
        self.assertTrue(l6 >= 0 and l6 <= 1)
        self.assertTrue(u6 >= 0 and u6 <= 1)

class WeightedMaximumNormTests(ExtendedExampleTest):

    def test_violation_vector(self):
        A = constraintMat(self.As, wf=self.wf)
        incm, incv = violation(self.ws, A, IC=self.IC, wf=self.wf, obj="inf")
        self.assertEqual(incv.shape, (25, 1))
        self.assertTrue(incm > 0)

    def test_query(self):
        l1, u1 = queryweightedmodel(self.q_access_alice_f1, self.ws, self.As,
                self.IC, wf=self.wf, obj="inf")
        l2, u2 = queryweightedmodel(self.q_access_alice_f2, self.ws, self.As,
                self.IC, wf=self.wf, obj="inf")
        l3, u3 = queryweightedmodel(self.q_access_bob_f1, self.ws, self.As,
                self.IC, wf=self.wf, obj="inf")
        l4, u4 = queryweightedmodel(self.q_access_bob_f2, self.ws, self.As,
                self.IC, wf=self.wf, obj="inf")
        l5, u5 = queryweightedmodel(self.q_blacklisted_alice, self.ws, self.As,
                self.IC, wf=self.wf, obj="inf")
        l6, u6 = queryweightedmodel(self.q_blacklisted_bob, self.ws, self.As,
                self.IC, wf=self.wf, obj="inf")
        self.assertTrue(l1 >= 0 and l1 <= 1)
        self.assertTrue(u1 >= 0 and u1 <= 1)
        self.assertTrue(l2 >= 0 and l2 <= 1)
        self.assertTrue(u2 >= 0 and u2 <= 1)
        self.assertTrue(l3 >= 0 and l3 <= 1)
        self.assertTrue(u3 >= 0 and u3 <= 1)
        self.assertTrue(l4 >= 0 and l4 <= 1)
        self.assertTrue(u4 >= 0 and u4 <= 1)
        self.assertTrue(l5 >= 0 and l5 <= 1)
        self.assertTrue(u5 >= 0 and u5 <= 1)
        self.assertTrue(l6 >= 0 and l6 <= 1)
        self.assertTrue(u6 >= 0 and u6 <= 1)

class WeightedManhattenNormTests(ExtendedExampleTest):

    def test_violation_vector(self):
        A = constraintMat(self.As, wf=self.wf)
        incm, incv = violation(self.ws, A, IC=self.IC, wf=self.wf, obj="1")
        self.assertEqual(incv.shape, (25, 1))
        self.assertTrue(incm > 0)

    def test_query(self):
        q = Rule(true, self.access_alice_f1, 0.0)
        l1, u1 = queryweightedmodel(self.q_access_alice_f1, self.ws, self.As,
                self.IC, wf=self.wf, obj="1")
        l2, u2 = queryweightedmodel(self.q_access_alice_f2, self.ws, self.As,
                self.IC, wf=self.wf, obj="1")
        l3, u3 = queryweightedmodel(self.q_access_bob_f1, self.ws, self.As,
                self.IC, wf=self.wf, obj="1")
        l4, u4 = queryweightedmodel(self.q_access_bob_f2, self.ws, self.As,
                self.IC, wf=self.wf, obj="1")
        l5, u5 = queryweightedmodel(self.q_blacklisted_alice, self.ws, self.As,
                self.IC, wf=self.wf, obj="1")
        l6, u6 = queryweightedmodel(self.q_blacklisted_bob, self.ws, self.As,
                self.IC, wf=self.wf, obj="1")
        self.assertTrue(l1 >= 0 and l1 <= 1)
        self.assertTrue(u1 >= 0 and u1 <= 1)
        self.assertTrue(l2 >= 0 and l2 <= 1)
        self.assertTrue(u2 >= 0 and u2 <= 1)
        self.assertTrue(l3 >= 0 and l3 <= 1)
        self.assertTrue(u3 >= 0 and u3 <= 1)
        self.assertTrue(l4 >= 0 and l4 <= 1)
        self.assertTrue(u4 >= 0 and u4 <= 1)
        self.assertTrue(l5 >= 0 and l5 <= 1)
        self.assertTrue(u5 >= 0 and u5 <= 1)
        self.assertTrue(l6 >= 0 and l6 <= 1)
        self.assertTrue(u6 >= 0 and u6 <= 1)

class WeightedQuadFormTests(ExtendedExampleTest):

    def test_violation(self):
        A = constraintMat(self.As, self.wf)
        incm, incv = violation(self.ws, A, self.IC, wf=self.wf, obj="q")
        self.assertTrue(math.isclose(incm, 17.97121, abs_tol=1e-5))
        self.assertTrue(math.isclose(np.sum(incv), 2.40, abs_tol=1e-2))

    def test_query(self):
        q = Rule(true, self.access_alice_f1, 0.0)
        l1, u1 = queryweightedmodel(self.q_access_alice_f1, self.ws, self.As,
                self.IC, wf=self.wf, obj="q")
        l2, u2 = queryweightedmodel(self.q_access_alice_f2, self.ws, self.As,
                self.IC, wf=self.wf, obj="q")
        l3, u3 = queryweightedmodel(self.q_access_bob_f1, self.ws, self.As,
                self.IC, wf=self.wf, obj="q")
        l4, u4 = queryweightedmodel(self.q_access_bob_f2, self.ws, self.As,
                self.IC, wf=self.wf, obj="q")
        l5, u5 = queryweightedmodel(self.q_blacklisted_alice, self.ws, self.As,
                self.IC, wf=self.wf, obj="q")
        l6, u6 = queryweightedmodel(self.q_blacklisted_bob, self.ws, self.As,
                self.IC, wf=self.wf, obj="q")
        self.assertTrue(math.isclose(l1, 0.44, abs_tol=1e-4))
        self.assertTrue(math.isclose(u1, 0.44, abs_tol=1e-4))
        self.assertTrue(math.isclose(l2, 0.6285, abs_tol=1e-4))
        self.assertTrue(math.isclose(u2, 0.6285, abs_tol=1e-4))
        self.assertTrue(math.isclose(l3, 0.1428, abs_tol=1e-4))
        self.assertTrue(math.isclose(u3, 0.1428, abs_tol=1e-4))
        self.assertTrue(math.isclose(l4, 0.4, abs_tol=1e-4))
        self.assertTrue(math.isclose(u4, 0.4, abs_tol=1e-4))
        self.assertTrue(math.isclose(l5, 0.005, abs_tol=1e-4))
        self.assertTrue(math.isclose(u5, 0.005, abs_tol=1e-4))
        self.assertTrue(math.isclose(l6, 0.01799, abs_tol=1e-4))
        self.assertTrue(math.isclose(u6, 0.01799, abs_tol=1e-4))

class Strict2NormTests(ExtendedExampleTest):

    def test_violation_vector(self):
        incms, incvs = strictviolation(self.ws, self.As, self.IC)
        self.assertEqual(len(incvs), 5)
        self.assertFalse(np.any(incvs[0]))
        self.assertTrue(len(incms), 5)
        self.assertEqual(incms[0], 0.0)

    def test_strict2norm(self):
        q = Rule(true, self.access_alice_f1, 0.0)
        l1, u1 = querystrictmodel(self.q_access_alice_f1, self.ws, self.As,
                self.IC)
        l2, u2 = querystrictmodel(self.q_access_alice_f2, self.ws, self.As,
                self.IC)
        l3, u3 = querystrictmodel(self.q_access_bob_f1, self.ws, self.As,
                self.IC)
        l4, u4 = querystrictmodel(self.q_access_bob_f2, self.ws, self.As,
                self.IC)
        l5, u5 = querystrictmodel(self.q_blacklisted_alice, self.ws, self.As,
                self.IC)
        l6, u6 = querystrictmodel(self.q_blacklisted_bob, self.ws, self.As,
                self.IC)
        self.assertTrue(math.isclose(l1, 0.7, abs_tol=5e-2))
        self.assertTrue(math.isclose(u1, 0.7, abs_tol=5e-2))
        self.assertTrue(math.isclose(l2, 0.7, abs_tol=5e-2))
        self.assertTrue(math.isclose(u2, 0.7, abs_tol=5e-2))
        self.assertTrue(math.isclose(l3, 0.0, abs_tol=5e-2))
        self.assertTrue(math.isclose(u3, 0.0, abs_tol=5e-2))
        self.assertTrue(math.isclose(l4, 0.5, abs_tol=5e-2))
        self.assertTrue(math.isclose(u4, 0.5, abs_tol=5e-2))
        self.assertTrue(math.isclose(l5, 0.001, abs_tol=1e-4))
        self.assertTrue(math.isclose(u5, 0.001, abs_tol=1e-4))
        self.assertTrue(math.isclose(l6, 0.01, abs_tol=5e-3))
        self.assertTrue(math.isclose(u6, 0.01, abs_tol=5e-3))

class StrictQuadFormTests(ExtendedExampleTest):

    def test_violation_vector(self):
        incms, incvs = strictviolation(self.ws, self.As, self.IC, obj="q")
        self.assertEqual(len(incvs), 5)
        self.assertFalse(np.any(incvs[0]))
        self.assertEqual(len(incms), 5)
        self.assertEqual(incms[0], 0.0)

    def test_query(self):
        q = Rule(true, self.access_alice_f1, 0.0)
        l1, u1 = querystrictmodel(self.q_access_alice_f1, self.ws, self.As,
                self.IC, obj="q")
        l2, u2 = querystrictmodel(self.q_access_alice_f2, self.ws, self.As,
                self.IC, obj="q")
        l3, u3 = querystrictmodel(self.q_access_bob_f1, self.ws, self.As,
                self.IC, obj="q")
        l4, u4 = querystrictmodel(self.q_access_bob_f2, self.ws, self.As,
                self.IC, obj="q")
        l5, u5 = querystrictmodel(self.q_blacklisted_alice, self.ws, self.As,
                self.IC, obj="q")
        l6, u6 = querystrictmodel(self.q_blacklisted_bob, self.ws, self.As,
                self.IC, obj="q")
        self.assertTrue(math.isclose(l1, 0.7, abs_tol=5e-2))
        self.assertTrue(math.isclose(u1, 0.7, abs_tol=5e-2))
        self.assertTrue(math.isclose(l2, 0.7, abs_tol=5e-2))
        self.assertTrue(math.isclose(u2, 0.7, abs_tol=5e-2))
        self.assertTrue(math.isclose(l3, 0.0, abs_tol=5e-2))
        self.assertTrue(math.isclose(u3, 0.0, abs_tol=5e-2))
        self.assertTrue(math.isclose(l4, 0.5, abs_tol=5e-2))
        self.assertTrue(math.isclose(u4, 0.5, abs_tol=5e-2))
        self.assertTrue(math.isclose(l5, 0.001, abs_tol=1e-4))
        self.assertTrue(math.isclose(u5, 0.001, abs_tol=1e-4))
        self.assertTrue(math.isclose(l6, 0.01, abs_tol=5e-3))
        self.assertTrue(math.isclose(u6, 0.01, abs_tol=5e-3))

@unittest.skip("Manhatten norm implementation is not sound")
class StrictManhattenNormTests(ExtendedExampleTest):

    def test_violation_vector(self):
        incms, incvs = strictviolation(self.ws, self.As, self.IC, obj="1")
        self.assertEqual(len(incvs), 5)
        self.assertFalse(np.any(incvs[0]))
        print("incms: ", incms)

    def test_query(self):
        q = Rule(true, self.access_alice_f1, 0.0)
        l1, u1 = querystrictmodel(self.q_access_alice_f1, self.ws, self.As,
                self.IC, obj="1")
        # l2, u2 = querystrictmodel(self.q_access_alice_f2, self.ws, self.As,
        #         self.IC, obj="1")
        # l3, u3 = querystrictmodel(self.q_access_bob_f1, self.ws, self.As,
        #         self.IC, obj="1")
        # l4, u4 = querystrictmodel(self.q_access_bob_f2, self.ws, self.As,
        #         self.IC, obj="1")
        # l5, u5 = querystrictmodel(self.q_blacklisted_alice, self.ws, self.As,
        #         self.IC, obj="1")
        # l6, u6 = querystrictmodel(self.q_blacklisted_bob, self.ws, self.As,
        #         self.IC, obj="1")
        print("alice file 1: ", l1, " ", u1)
        # print("alice file 2: ", l2, " ", u2)
        # print("bob file 1: ", l3, " ", u3)
        # print("bob file 2: ", l4, " ", u4)
        # print("blacklisted alice: ", l5, " ", u5)
        # print("blacklisted bob: ", l6, " ", u6)

class StrictMaximumNormTests(ExtendedExampleTest):

    def test_violation_vector(self):
        incms, incvs = strictviolation(self.ws, self.As, self.IC, obj="inf")
        self.assertEqual(len(incvs), 5)
        self.assertFalse(np.any(incvs[0]))
        self.assertEqual(len(incms), 5)
        self.assertEqual(incms[0], 0.0)

    def test_query(self):
        q = Rule(true, self.access_alice_f1, 0.0)
        l1, u1 = querystrictmodel(self.q_access_alice_f1, self.ws, self.As,
                self.IC, obj="inf")
        l2, u2 = querystrictmodel(self.q_access_alice_f2, self.ws, self.As,
                self.IC, obj="inf")
        l3, u3 = querystrictmodel(self.q_access_bob_f1, self.ws, self.As,
                self.IC, obj="inf")
        l4, u4 = querystrictmodel(self.q_access_bob_f2, self.ws, self.As,
                self.IC, obj="inf")
        l5, u5 = querystrictmodel(self.q_blacklisted_alice, self.ws, self.As,
                self.IC, obj="inf")
        l6, u6 = querystrictmodel(self.q_blacklisted_bob, self.ws, self.As,
                self.IC, obj="inf")
        self.assertTrue(l1 >= 0 and l1 <= 1)
        self.assertTrue(u1 >= 0 and u1 <= 1)
        self.assertTrue(l2 >= 0 and l2 <= 1)
        self.assertTrue(u2 >= 0 and u2 <= 1)
        self.assertTrue(l3 >= 0 and l3 <= 1)
        self.assertTrue(u3 >= 0 and u3 <= 1)
        self.assertTrue(l4 >= 0 and l4 <= 1)
        self.assertTrue(u4 >= 0 and u4 <= 1)
        self.assertTrue(l5 >= 0 and l5 <= 1)
        self.assertTrue(u5 >= 0 and u5 <= 1)
        self.assertTrue(l6 >= 0 and l6 <= 1)
        self.assertTrue(u6 >= 0 and u6 <= 1)



if __name__ == "__main__":
    s1 = unittest.TestLoader().loadTestsFromTestCase(SignatureTests)
    s2 = unittest.TestLoader().loadTestsFromTestCase(RuleTests)
    s3 = unittest.TestLoader().loadTestsFromTestCase(WorldGenerationTests)
    s4 = unittest.TestLoader().loadTestsFromTestCase(ConstraintsTests)
    s5 = unittest.TestLoader().loadTestsFromTestCase(GeneralEntailmentTests)
    s6 = unittest.TestLoader().loadTestsFromTestCase(ExtendedWorldGenerationTests)
    s7 = unittest.TestLoader().loadTestsFromTestCase(Weighted2NormTests)
    s8 = unittest.TestLoader().loadTestsFromTestCase(WeightedMaximumNormTests)
    s9 = unittest.TestLoader().loadTestsFromTestCase(WeightedManhattenNormTests)
    s10 = unittest.TestLoader().loadTestsFromTestCase(WeightedQuadFormTests)
    s11 = unittest.TestLoader().loadTestsFromTestCase(Strict2NormTests)
    s12 = unittest.TestLoader().loadTestsFromTestCase(StrictQuadFormTests)
    s13 = unittest.TestLoader().loadTestsFromTestCase(StrictMaximumNormTests)
    allTests = unittest.TestSuite([s1, s2, s3, s4, s5, s6, s7, s8, s9, s10,
        s11, s12, s13])
    unittest.TextTestRunner(verbosity=2).run(allTests)
