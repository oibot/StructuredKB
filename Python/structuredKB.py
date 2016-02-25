from itertools import product
import collections
import numpy as np
import cvxpy as cvx
import sympy as sym

def worlds(signature):
    vs = product((True,False), repeat=len(signature))
    return [dict(zip(signature, v)) for v in vs]

Rule = collections.namedtuple('Rule', ['premise', 'conclusion', 'probability'])

# The effect of a rule to a world. It is ...
#   * (1-p) if the world satisfies the rule
#   * -p if the world satisfies the rule with a negated conclusion
#   * it is 0 for a world that satisfies neither premise nor conclusion
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

# Entry point for queries
def query(rule, kbs, ic=[], norm="2", wf=lambda x: x):
    rules = [r for kb in kbs for r in kb] + ic
    ws = worlds(signature(rules))
    IC, As = constraints_matrices(ws, kbs, ic)
    if norm == "2":
        return query2norm(rule, ws, As, IC)
    else:
        return False

def query2norm(rule, worlds, As, IC, wf=lambda x: x):
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

def violation(worlds, A, IC=[], wf=lambda x: x):
    P = cvx.Variable(len(worlds))
    obj = cvx.Minimize(cvx.norm(A*P))
    cons = [P >= 0, cvx.sum_entries(P) == 1]
    if len(IC):
        cons += [IC*P == 0]
        print(len(cons))
    prob = cvx.Problem(obj, cons)
    incm = prob.solve(verbose=True)
    incv = A * P.value
    return (incm, incv)

def constraintMat(As, wf=lambda x: x):
    for idx, A in enumerate(As):
        A *= wf(idx+1)
    return np.vstack(As)
