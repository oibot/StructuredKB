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

class ExtractedSignatureTest(unittest.TestCase):

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

class ExtractedWorldGenerationTests(ExtractedSignatureTest):

    def test_world_generation(self):
        ws = worlds(self.signature, self.kb5+self.ic)
        self.assertEqual(len(ws), 100)


class Weighted2NormTests(ExtractedSignatureTest):

    def test_violation5(self):
        wf = lambda x: 2*x
        ws = worlds(self.signature, deters=self.kb5+self.ic)
        IC, As = constraints_matrices(ws, self.KB, self.ic)

        A = constraintMat(As, wf=wf)
        incm, incv = violation(ws, A, IC=IC, wf=wf)

        q = Rule(true, self.access_alice_f1, 0.0)
        l1, u1 = query2norm(q, ws, As, IC, wf=wf)

        q = Rule(true, self.access_alice_f2, 0.0)
        l2, u2 = query2norm(q, ws, As, IC, wf=wf)

        q = Rule(true, self.access_bob_f1, 0.0)
        l3, u3 = query2norm(q, ws, As, IC, wf=wf)

        q = Rule(true, self.access_bob_f2, 0.0)
        l4, u4 = query2norm(q, ws, As, IC, wf=wf)

        q = Rule(true, self.blacklisted_alice, 0.0)
        l5, u5 = query2norm(q, ws, As, IC, wf=wf)

        q = Rule(true, self.blacklisted_bob, 0.0)
        l6, u6 = query2norm(q, ws, As, IC, wf=wf)

        print("grantAccess(alice, f1):")
        print ("lower: ", l1, " upper: ", u1)
        print("grantAccess(alice, f2):")
        print ("lower: ", l2, " upper: ", u2)
        print("grantAccess(bob, f1):")
        print ("lower: ", l3, " upper: ", u3)
        print("grantAccess(bob, f2):")
        print ("lower: ", l4, " upper: ", u4)
        print("blacklisted(alice):")
        print ("lower: ", l5, " upper: ", u5)
        print("blacklisted(bob):")
        print ("lower: ", l6, " upper: ", u6)

class Strict2NormTests(ExtractedSignatureTest):

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

    def test_strict2norm(self):
        ws = worlds(self.signature, deters=self.kb5+self.ic)
        IC, As = constraints_matrices(ws, self.KB, self.ic)

        vs = strictviolation(ws, As, IC)
        print("length vs: ", len(vs))
        print("shape for vs[0]: ", vs[0].shape)

        q = Rule(true, self.access_alice_f1, 0.0)
        l1, u1 = querystrict2norm(q, ws, As, IC)

        q = Rule(true, self.access_alice_f2, 0.0)
        l2, u2 = querystrict2norm(q, ws, As, IC)

        q = Rule(true, self.access_bob_f1, 0.0)
        l3, u3 = querystrict2norm(q, ws, As, IC)

        q = Rule(true, self.access_bob_f2, 0.0)
        l4, u4 = querystrict2norm(q, ws, As, IC)

        q = Rule(true, self.blacklisted_alice, 0.0)
        l5, u5 = querystrict2norm(q, ws, As, IC)

        q = Rule(true, self.blacklisted_bob, 0.0)
        l6, u6 = querystrict2norm(q, ws, As, IC)

        print("grantAccess(alice, f1):")
        print ("lower: ", l1, " upper: ", u1)
        print("grantAccess(alice, f2):")
        print ("lower: ", l2, " upper: ", u2)
        print("grantAccess(bob, f1):")
        print ("lower: ", l3, " upper: ", u3)
        print("grantAccess(bob, f2):")
        print ("lower: ", l4, " upper: ", u4)
        print("blacklisted(alice):")
        print ("lower: ", l5, " upper: ", u5)
        print("blacklisted(bob):")
        print ("lower: ", l6, " upper: ", u6)





if __name__ == "__main__":
    s1 = unittest.TestLoader().loadTestsFromTestCase(SignatureTests)
    s2 = unittest.TestLoader().loadTestsFromTestCase(RuleTests)
    s3 = unittest.TestLoader().loadTestsFromTestCase(WorldGenerationTests)
    s4 = unittest.TestLoader().loadTestsFromTestCase(ConstraintsTests)
    s5 = unittest.TestLoader().loadTestsFromTestCase(GeneralEntailmentTests)
    s6 = unittest.TestLoader().loadTestsFromTestCase(ExtractedSignatureTest)
    s7 = unittest.TestLoader().loadTestsFromTestCase(ExtractedWorldGenerationTests)
    allTests = unittest.TestSuite([s1, s2, s3, s4, s5, s6, s7])
    unittest.TextTestRunner(verbosity=2).run(allTests)
