from Population import Population
from Automata import Automaton
from random import randrange
from retic import Int

import os, itertools
fname = os.path.join(os.path.dirname(__file__), "automata-random-numbers.txt")
#TODO: Cannot type variable in retic
rand_num = itertools.cycle((int(line.strip()) for line in open(fname, "r")))


def make_random_automaton(n: Int)->Automaton:
    """
    builds an n states x k inputs automation
    with a random transition table
    :param n:
    :return: Automation
    """
    seed = (next(rand_num))
    table = [[(next(rand_num)) for _ in range(n)] for _ in range(n)]
    return Automaton(seed, 0.0, table, seed)


def build_random_population(n: Int)->Population:
    """
    for even n, build a population of size n
    :param n: Natural
    :return: Population
    """
    DEF_COO = 2
    v = [make_random_automaton(DEF_COO) for i in range(n)]
    return Population(v)




