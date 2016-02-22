from itertools import product
import collections
import numpy as np
import cvxpy as cvx
import sympy as sym

def worlds(signature):
    vs = product((True,False), repeat=len(signature))
    return [dict(zip(signature, v)) for v in vs]

Rule = collections.namedtuple('Rule', ['premise', 'conclusion', 'probability'])

def satisfies(world, formula):
    return formula.subs(world)

def satisfies_rule(world, rule):
    return satisfies(world, rule.premise) and satisfies(world, rule.conclusion)

# The effect of a rule to a world. It is ...
#   * (1-p) if the world satisfies the rule
#   * -p if the world satisfies the rule with a negated conclusion
#   * it is 0 for a world that satisfies neither premise nor conclusion
def effect(rule, world):
    if verified(rule, world):
        return 1 - rule.probability
    elif falsified(rule, world):
        return -rule.probability
    else:
        return 0.0

# A rule is verified in a world when the world satisfies premise and conclusion
def verified(rule, world):
    return satisfies(world, rule[0]) and satisfies(world, rule[1])

# A rule is falsified in a world when the world satisfies the premise
# and the negation of the conculusion
def falsified(rule, world):
    return satisfies(world, rule[0]) and not satisfies(world, rule[1])

# Generates a signatur out of a knowledgebase. All the formulas in the
# rules of the knowledgebase become letters in the signature. A premise
# can hold a tautology symbol, which must not be added to the signature
def signature(kb):
    sig = set()
    for p, c, _ in kb:
        if not p is sym.true: sig.add(p)
        sig.add(c)
    return sig

# A constraint matrix consists of rows for every rule in a knowledgebasea and
# columns for the effects of a rule for every world.
def constraints_matrix(kb):
    sig = signature(kb)
    ws = worlds(sig)
    return np.array([[effect(c, w) for w in ws] for c in kb])

def verifying_matrix(worlds, rule):
    return np.array([1 if verified(rule, w) else 0 for w in worlds])

def falsifying_matrix(worlds, rule):
    return np.array([1 if falsified(rule, w) else 0 for w in worlds])


##################################################
# QUERY
##################################################


def query(rule, kb, norm="2"):
    A = constraints_matrix(kb)
    ws = worlds(signature(kb))
    if norm == "2":
        return query2norm(rule, ws, A)
    elif norm == "1":
        return query1norm(rule, ws, A)
    elif norm == "inf":
        return queryInfnorm(rule, ws, A)
    else:
        return False

def query2norm(rule, worlds, A):
    _, incv = violation(worlds, A)

    fm = falsifying_matrix(worlds, rule)
    vm = verifying_matrix(worlds, rule)

    x = cvx.Variable(len(worlds))
    t = cvx.Variable()
    cons = [x >= 0, t >= 0,
            A*x == t*incv,
            cvx.sum_entries(x) == t,
            (vm + fm)*x == 1]

    l = entailment_lower(cons, vm, x)
    u = entailment_upper(cons, vm, x)

    return (l, u)

def query1norm(rule, worlds, A):
    incm, _ = violation(worlds, A, norm="1")

    fm = falsifying_matrix(worlds, rule)
    vm = verifying_matrix(worlds, rule)

    x = cvx.Variable(len(worlds))
    t = cvx.Variable()
    y = cvx.Variable(len(A))
    cons = [x >= 0, y >= 0, t >= 0,
            -y <= A*x, A*x <= y,
            cvx.sum_entries(y) == t*incm,
            cvx.sum_entries(x) == t,
            (vm + fm)*x == 1]

    l = entailment_lower(cons, vm, x)
    u = entailment_upper(cons, vm, x)

    return (l, u)

def queryInfnorm(rule, worlds, A):
    incm, _ = violation(worlds, A, norm="inf")

    fm = falsifying_matrix(worlds, rule)
    vm = verifying_matrix(worlds, rule)

    x = cvx.Variable(len(worlds))
    t = cvx.Variable()
    cons = [x >= 0, t >= 0,
            -t*incm <= A*x, A*x <= t*incm,
            cvx.sum_entries(x) == t,
            (vm + fm)*x == 1]

    l = entailment_lower(cons, vm, x)
    u = entailment_upper(cons, vm, x)

    return (l, u)

def entailment_lower(cons, vm, x):
    obj = cvx.Minimize(vm*x)
    prob = cvx.Problem(obj, cons)
    prob.solve()
    return prob.value if prob.status == "optimal" else 0

def entailment_upper(cons, vm, x):
    obj = cvx.Maximize(vm*x)
    prob = cvx.Problem(obj, cons)
    prob.solve()
    return prob.value if prob.status == "optimal" else 1

def violation(worlds, A, norm="2"):
    x = cvx.Variable(len(worlds))
    obj = violation_objective(x, A, norm=norm)
    cons = [x >= 0, cvx.sum_entries(x) == 1]
    prob = cvx.Problem(obj, cons)
    return (prob.solve(), A*x.value)

def violation_objective(x, A, norm="2"):
    if norm == "2":
        return cvx.Minimize(cvx.norm(A*x))
    elif norm == "1":
        return cvx.Minimize(cvx.norm(A*x, 1))
    elif norm == "inf":
        return cvx.Minimize(cvx.norm(A*x, "inf"))
    else:
        return nil
