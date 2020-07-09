#!/usr/bin/env retic
from retic import List, Void, Int, Float
from Utilities import choose_randomly
from Automata import Automaton
from copy import copy

import os, itertools
from retic import fields
fname = os.path.join(os.path.dirname(__file__), "population-random-numbers.txt")
#TODO: Cannot type variable in retic
rand_num = itertools.cycle((int(line.strip()) for line in open(fname, "r")))


@fields({'a':List(Automaton)})
class Population:
    """
    Populations of Automata
    """

    def __init__(self: Population, a: List(Automaton)) -> Void:
        self.a = a

    def payoffs(self: Population)->List(Float):
        result = []
        for element in self.a:
            result = result + [element.payoff]
        return result

    def match_up(self: Population, r: Int) -> Population:
        """
        matches up neighboring pairs of
        automata in this population for r rounds
        :return: Population
        """
        self.a = [element.reset() for element in self.a]

        for i in range(0, len(self.a) - 1, 2):
            p1 = self.a[i]
            p2 = self.a[i+1]
            a = p1.interact(p2, r)
            self.a[i] = a[0]
            self.a[i+1] = a[1]
        return self

    def regenerate(self:Population, rate: Int)->Population:
        """
        Replaces r elements of p with r 'children' of randomly chosen
        fittest elements of p, also shuffle constraint (r < (len p))
        :param rate: Number of elements to replace in a
        :param q: threshold
        :return: Population
        """
        payoffs = self.payoffs()
        substitutes = choose_randomly(payoffs, rate)
        for i in range(rate):
            index = substitutes[i]
            self.a[i] = self.a[index].clone()
        self.shuffle()
        return self

    def shuffle(self:Population)->Void:
        b = copy(self.a)
        for i in range(len(self.a)):
            j = next(rand_num)
            if j != i:
                b[i] = b[j]
            b[j] = self.a[i]
        self.a = b

