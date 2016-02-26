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

def queryweighted2norm(rule, worlds, As, IC, wf=lambda x: x):
    # violation vector
    A = constraintMat(As, wf)
    incm, incv = violation(worlds, A, IC, wf)

    # entailment
    vm = verifying_matrix(worlds, rule)
    fm = falsifying_matrix(worlds, rule)
    P = cvx.Variable(len(worlds))
    t = cvx.Variable()
    cons = [P >= 0, t >= 0,
            IC*P == 0,
            A*P == t*incv,
            cvx.sum_entries(P) == t,
            (vm+fm)*P == 1]
    probL = cvx.Problem(cvx.Minimize(vm*P), cons)
    probU = cvx.Problem(cvx.Maximize(vm*P), cons)
    return (probL.solve(verbose=True), probU.solve(verbose=True))

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

def violation(worlds, A, IC=[], wf=lambda x: x):
    P = cvx.Variable(len(worlds))
    obj = cvx.Minimize(cvx.norm(A*P))
    cons = [P >= 0, cvx.sum_entries(P) == 1]
    if len(IC):
        cons += [IC*P == 0]
    prob = cvx.Problem(obj, cons)
    incm = prob.solve(verbose=True)
    incv = A * P.value
    return (incm, incv)

def constraintMat(As, wf=lambda x: x):
    for idx, A in enumerate(As):
        A *= wf(idx+1)
    return np.vstack(As)
