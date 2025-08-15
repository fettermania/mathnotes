import functools
import math


def fact(i):
    if i == 0:
        return 1
    else:
        return functools.reduce(lambda x, y: x*y, range(1, i+1))

def n_choose_k(r, k):
    return int(fact(r) / (fact(r-k) * fact(k)))


states = [12, 1, 1, 1]
sides = [6,8,10,12]
probs = list(map(lambda x: 1/x, sides))


# returns in topological order
def all_states(state_vec):
    def _create_state_list(max_state):
        return list(map(lambda x: [x], list(range(0,max_state+1))))
    def _list_product(a,b):
        return list(x+y for x in a for y in b)
    state_list = list(map(_create_state_list, state_vec))
    return functools.reduce(_list_product, state_list)

def reachable_states(state_vec):
    return all_states(state_vec)[:-1]  # remove self

def create_initial_state_evs(state_vec):
    keys = list(map(tuple, all_states(state_vec)))
    vals = [0 for x in range(0, len(keys))]
    evs = dict(zip(keys, vals))
    evs[keys[0]] = 1
    return evs


def prob_single_die_transition(count1, count2, p):
    n = count1
    k = count1 - count2
    return n_choose_k(n, k) * math.pow(p, k) * math.pow(1-p, n-k)


def prob_state_transition(state, reachable_state, probs):
    zipped = list(zip(state, reachable_state, probs))
    per_die_probs = list(map(lambda x: prob_single_die_transition(x[0], x[1], x[2]), zipped))
    return functools.reduce(lambda x, y: x*y, per_die_probs)

def calculate_evs(state_vec, probs):
    evs = create_initial_state_evs(state_vec)
    iterable_states = all_states(state_vec)[1:]
    for state in iterable_states:
        reachables = reachable_states(state)
        total_ev = 0
        for reachable_state in reachables:
            p = prob_state_transition(state, reachable_state, probs)
            previous_ev = evs[tuple(reachable_state)]
            total_ev = total_ev + p*previous_ev
   #        print(f"P({state} -> {reachable_state} = {p}, previous_ev = {previous_ev}")
        evs[tuple(state)] = total_ev
    return evs

print("\n === COMMERCIAL GAME ==== \n")
print(calculate_evs(states, probs))


asymptotic_x = 100
for s in range(1,10):
    print(f"\n === Single die {s}, asymptotic: ==== \n")
    print(calculate_evs([asymptotic_x], [1/s])[tuple([asymptotic_x])])

print("\n === Non-monotonic value: p=.495 GAME ==== \n")
print(calculate_evs([asymptotic_x], [.495]))


