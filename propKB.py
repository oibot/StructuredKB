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

def signature(rules):
    sig = set()
    for p, c, _ in rules:
        sig.update(p.atoms() | c.atoms())
    return sig

def constraints_matrices(worlds, kbs, ic=[]):
    A = np.array([[[effect(r, w) for w in worlds] for r in kb] for kb in kbs])
    IC = np.array([[effect(r, w) for w in worlds] for r in ic])
    return (IC, A)

def verifying_matrix(worlds, rule):
    return np.array([1 if verified(rule, w) else 0 for w in worlds])

def falsifying_matrix(worlds, rule):
    return np.array([1 if falsified(rule, w) else 0 for w in worlds])




##################################################
# QUERY
##################################################

# Entry point for queries
def query(rule, kbs, ic=[], norm="2"):
    rules = [r for kb in kbs for r in kb] + ic
    ws = worlds(signature(rules))
    IC, As = constraints_matrices(ws, kbs, ic)
    if norm == "2":
        return query2norm(rule, ws, As, IC)
    elif norm == "1":
        return query1norm(rule, ws, As)
    elif norm == "inf":
        return queryInfnorm(rule, ws, As)
    else:
        return False

##################################################
#Query-Helper
##################################################

def query2norm(rule, worlds, As, IC=np.array([])):
    A = As[0]
    _, incv = violation(worlds, A, IC)

    fm = falsifying_matrix(worlds, rule)
    vm = verifying_matrix(worlds, rule)

    x = cvx.Variable(len(worlds))
    t = cvx.Variable()
    cons = [x >= 0, t >= 0,
            A*x == t*incv,
            cvx.sum_entries(x) == t,
            (vm + fm)*x == 1]
    if IC.size:
        cons += [IC*x == 0]

    l = entailment_lower(cons, vm, x)
    u = entailment_upper(cons, vm, x)

    return (l, u)

def query1norm(rule, worlds, As):
    A = As[0]
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

def queryInfnorm(rule, worlds, As):
    A = As[0]
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

def violation(worlds, A, IC=np.array([]), norm="2"):
    x = cvx.Variable(len(worlds))
    obj = violation_objective(x, A, norm=norm)
    cons = [x >= 0, cvx.sum_entries(x) == 1]
    if IC.size:
        cons += [IC*x == 0]
    prob = cvx.Problem(obj, cons)
    incm = prob.solve()
    return (incm, A*x.value)

def violation_objective(x, A, norm="2"):
    if norm == "2":
        return cvx.Minimize(cvx.norm(A*x))
    elif norm == "1":
        return cvx.Minimize(cvx.norm(A*x, 1))
    elif norm == "inf":
        return cvx.Minimize(cvx.norm(A*x, "inf"))
    else:
        return nil

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

