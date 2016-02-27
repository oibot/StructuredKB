from itertools import product
import collections
import numpy as np
import cvxpy as cvx
import sympy as sym
from functools import reduce

def worlds(signature, deters=[]):
    vs = product((True,False), repeat=len(signature))
    ws = []
    if len(deters):
        ds = [d.premise >> d.conclusion if d.probability == 1.0
                else d.premise >> ~d.conclusion
                for d in deters]
        c = reduce(lambda x,y: x & y, ds)
        for v in vs:
            w = dict(zip(signature, v))
            if c.subs(w): ws += [w]
    else:
        ws = [dict(zip(signature, v)) for v in vs]
    return ws

Rule = collections.namedtuple('Rule', ['premise', 'conclusion', 'probability'])

# The effect of a rule to a world. It is ...
#   * 0 if the world does not satisfies the premise
#   * (1-p) if the world satisfies premise and conclusion
#   * -p if the world satisfies premise and negated conclusion
def effect(rule, world):
    if (~rule.premise.subs(world)):
        return 0.0
    elif rule.conclusion.subs(world):
        return 1 - rule.probability
    else:
        return -rule.probability

def signature(rules):
    sig = set()
    for p, c, _ in rules:
        sig.update(p.atoms() | c.atoms())
    return sig

def constraints_matrices(worlds, kbs, ic=[]):
    As = [np.array([[effect(r, w) for w in worlds] for r in kb]) for kb in kbs]
    IC = np.array([[effect(r, w) for w in worlds] for r in ic])
    return (IC, As)

def verifying_matrix(worlds, rule):
    return np.array([1 if (rule.premise & rule.conclusion).subs(w) else 0
        for w in worlds])

def falsifying_matrix(worlds, rule):
    return np.array([1 if (rule.premise & ~rule.conclusion).subs(w) else 0
        for w in worlds])


##################################################
# QUERY
##################################################

def queryweightedmodel(rule, worlds, As, IC, wf=lambda x: x, obj="2"):
    # violation vector
    A = constraintMat(As, wf)
    incm, incv = violation(worlds, A, IC, wf, obj=obj)

    # entailment
    vm = verifying_matrix(worlds, rule)
    fm = falsifying_matrix(worlds, rule)
    P = cvx.Variable(len(worlds))
    t = cvx.Variable()
    if obj == "2" or obj == "q":
        cons = [P >= 0, t >= 0,
                IC*P == 0,
                A*P == t*incv,
                cvx.sum_entries(P) == t,
                (vm+fm)*P == 1]
    elif obj == "1":
        y = cvx.Variable(A.shape[0])
        cons = [P >= 0, t >= 0, y >= 0,
                -y <= A*P, A*P <= y,
                cvx.sum_entries(y) == t*incm,
                IC*P == 0,
                cvx.sum_entries(P) == t,
                (vm+fm)*P == 1]
    elif obj == "inf":
        cons = [P >= 0, t >= 0,
                -t*incm <= A*P, A*P <= t*incm,
                IC*P == 0,
                cvx.sum_entries(P) == t,
                (vm+fm)*P == 1]

    probL = cvx.Problem(cvx.Minimize(vm*P), cons)
    probU = cvx.Problem(cvx.Maximize(vm*P), cons)

    # return (probL.solve(solver=cvx.CVXOPT, verbose=True,
    #                     kktsolver=cvx.ROBUST_KKTSOLVER),
    #         probU.solve(solver=cvx.CVXOPT, verbose=True,
    #                     kktsolver=cvx.ROBUST_KKTSOLVER))

    probL.solve()
    if probL.status == cvx.OPTIMAL or probL.status == cvx.OPTIMAL_INACCURATE:
        l = probL.value
    else:
        l = 0
    probU.solve()
    if probU.status == cvx.OPTIMAL or probU.status == cvx.OPTIMAL_INACCURATE:
        u = probU.value
    else:
        u = 1
    return (l, u)

def violation(worlds, A, IC=[], wf=lambda x: x, obj="2"):
    P = cvx.Variable(len(worlds))

    cons = [P >= 0, cvx.sum_entries(P) == 1]
    if len(IC):
        cons += [IC*P == 0]

    if obj == "2":
        prob = cvx.Problem(cvx.Minimize(cvx.norm(A*P)), cons)
    elif obj == "q":
        Q = np.dot(np.transpose(A), A)
        prob = cvx.Problem(cvx.Minimize(cvx.quad_form(P, Q)), cons)
    elif obj == "1":
        prob = cvx.Problem(cvx.Minimize(cvx.norm(A*P, 1)), cons)
    elif obj == "inf":
        prob = cvx.Problem(cvx.Minimize(cvx.norm(A*P, "inf")), cons)

    prob.solve()
    incm = prob.value
    incv = A * P.value
    return (incm, incv)

def querystrict2norm(rule, worlds, As, IC):
    # violation vector
    vs = strictviolation(worlds, As, IC)

    #entailment
    vm = verifying_matrix(worlds, rule)
    fm = falsifying_matrix(worlds, rule)
    P = cvx.Variable(len(worlds))
    t = cvx.Variable()
    cons = [P >= 0, t >= 0,
            IC*P == 0,
            cvx.sum_entries(P) == t,
            (vm+fm)*P == 1]
    cons += [As[i]*P == t*vs[-(i+1)] for i in range(len(As))]
    probL = cvx.Problem(cvx.Minimize(vm*P), cons)
    probU = cvx.Problem(cvx.Maximize(vm*P), cons)
    return (probL.solve(verbose=True), probU.solve(verbose=True))


def strictviolation(worlds, As, IC):
    vs = []
    for p in reversed(range(len(As))):
        A = As[p]
        P = cvx.Variable(len(worlds))
        obj = cvx.Minimize(cvx.norm(A*P))
        cons = [P >= 0, cvx.sum_entries(P) == 1]
        if len(vs):
            cons += [B*P == vs[-(i+1)] for i, B in enumerate(As[(p+1):])]
        if len(IC):
            cons += [IC*P == 0]
            print("length of cons", len(cons))
        prob = cvx.Problem(obj, cons)
        incm = prob.solve(verbose=True)
        vs += [A * P.value]
    return vs


def constraintMat(As, wf=lambda x: x):
    cs = []
    for idx, A in enumerate(As):
        B = np.copy(A) * wf(idx+1)
        cs += [B]
    return np.vstack(cs)
