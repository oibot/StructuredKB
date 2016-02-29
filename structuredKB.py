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
##################################################
# QUERY
##################################################
##################################################

##################################################
# Weitghted Priority Models
##################################################

def queryweightedmodel(rule, worlds, As, IC, wf=lambda x: x, obj="2"):
    # violation vector
    A = constraintMat(As, wf)
    incm, incv = weightedviolation(worlds, A, IC, wf, obj=obj)

    # entailment
    vm = verifying_matrix(worlds, rule)
    fm = falsifying_matrix(worlds, rule)
    P = cvx.Variable(len(worlds))
    t = cvx.Variable()
    cons = [P >= 0, t >= 0,
            IC*P == 0,
            cvx.sum_entries(P) == t,
            (vm+fm)*P == 1]
    if obj == "2" or obj == "q":
        cons += [A*P == t*incv]
    elif obj == "1":
        y = cvx.Variable(A.shape[0])
        cons += [y >= 0,
                 -y <= A*P, A*P <= y,
                 cvx.sum_entries(y) == t*incm]
    elif obj == "inf":
        cons += [-t*incm <= A*P, A*P <= t*incm]

    probL = cvx.Problem(cvx.Minimize(vm*P), cons)
    probU = cvx.Problem(cvx.Maximize(vm*P), cons)

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

def weightedviolation(worlds, A, IC=[], wf=lambda x: x, obj="2"):
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


def constraintMat(As, wf=lambda x: x):
    cs = []
    for idx, A in enumerate(As):
        B = np.copy(A) * wf(idx+1)
        cs += [B]
    return np.vstack(cs)

##################################################
# Structured Priority Models
##################################################

def querystrictmodel(rule, worlds, As, IC, obj="2"):
    # violation vector
    incms, incvs = strictviolation(worlds, As, IC)

    #entailment
    vm = verifying_matrix(worlds, rule)
    fm = falsifying_matrix(worlds, rule)
    P = cvx.Variable(len(worlds))
    t = cvx.Variable()
    cons = [P >= 0, t >= 0,
            IC*P == 0,
            cvx.sum_entries(P) == t,
            (vm+fm)*P == 1]
    if obj == "2" or obj == "q":
        cons += [As[i]*P == t*incvs[-(i+1)] for i in range(len(As))]
    # elif obj == "1":
    #     ys = [cvx.Variable(A.shape[0]) for A in As]
    #     cons = [P >= 0, t >= 0,
    #             IC*P == 0,
    #             cvx.sum_entries(P) == t,
    #             (vm+fm)*P == 1]
    #     cons += [-ys[i] <= As[i]*P for i in range(len(As))]
    #     cons += [As[i]*P <= ys[i] for i in range(len(As))]
    #     cons += [cvx.sum_entries(ys[i]) == t*incms[-(i+1)] for i in range(len(As))]
    elif obj == "inf":
        cons += [As[i]*P <= t*incms[-(i+1)] for i in range(len(As))]
        cons += [As[i]*P >= -t*incms[-(i+1)] for i in range(len(As))]

    probL = cvx.Problem(cvx.Minimize(vm*P), cons)
    probU = cvx.Problem(cvx.Maximize(vm*P), cons)

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


def strictviolation(worlds, As, IC, obj="2"):
    incvs = []
    incms = []
    for p in reversed(range(len(As))):
        A = As[p]
        if not np.any(A):
            incvs.append(np.zeros((A.shape[0], 1)))
            incms.append(0.0)
            continue

        P = cvx.Variable(len(worlds))
        cons = [P >= 0, cvx.sum_entries(P) == 1]
        if len(incvs):
            cons += [B*P == incvs[-(i+1)] for i, B in enumerate(As[(p+1):])]
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
        incms += [prob.value]
        incvs += [A * P.value]
    return (incms, incvs)

