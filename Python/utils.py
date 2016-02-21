from itertools import product
import collections
import numpy as np
import cvxpy as cvx

# Generates a list of all possible worlds for a signatur. A world is a
# dictionary with the letters as keys and true or false as values.
def worlds(signatur):
    vs = product((True,False), repeat=len(signatur))
    return [dict(zip(signatur, v)) for v in vs]

# Simple datastructure to hold a conditional rule. A empty premise is
# tantamount with a tautological premise.
Rule = collections.namedtuple('Rule', ['premise', 'conclusion', 'probability'])

# A world satisfies a formula if the formula is true in this world
def satisfies(world, formula):
    return True if not formula else world[formula]

# A world satisfies a rule when the world satisfies both premise and conclusion
def satisfies_rule(world, conditional):
    return satisfies(world, conditional[0]) and satisfies(world, conditional[1])

# The effect of a rule to a world. It is ...
#   * (1-p) if the world satisfies the rule
#   * -p if the world satisfies the rule with a negated conclusion
#   * it is 0 for a world that satisfies neither premise nor conclusion
def effect(world, rule):
    if verifies(world, rule):
        return 1 - rule[2]
    elif falsifies(world, rule):
        return -rule[2]
    else:
        return 0.0

def verifies(world, rule):
    return satisfies(world, rule[0]) and satisfies(world, rule[1])

def falsifies(world, rule):
    return satisfies(world, rule[0]) and not satisfies(world, rule[1])

# Generates a signatur out of a knowledgebase. All the formulas in the
# conditionals of the knowledgebase become letters in the signatur.
def signatur(kb):
    sig = set()
    for p, c, _ in kb:
        if p: sig.add(p)
        sig.add(c)
    return sig

# A constraint matrix consists of rows for every rule in a knowledgebasea and
# columns for the effects of a rule for every world.
def constraints_matrix(kb):
    sig = signatur(kb)
    ws = worlds(sig)
    return np.array([[effect(w, c) for w in ws] for c in kb])

def verifying_matrix(worlds, rule):
    return np.array([1 if verifies(w,rule) else 0 for w in worlds])

def falsifying_matrix(worlds, rule):
    return np.array([1 if falsifies(w,rule) else 0 for w in worlds])

def query(rule, worlds, kb, norm="2"):
    A = constraints_matrix(kb)
    if norm == "2":
        return query2norm(rule, worlds, A)
    elif norm == "1":
        return query1norm(rule, worlds, A)
    elif norm == "inf":
        return queryInfnorm(rule, worlds, A)
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
