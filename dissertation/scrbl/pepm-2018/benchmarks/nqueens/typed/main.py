from Timer import Timer

"""Simple, brute-force N-Queens solver."""

"""
bg:
- replace `timer` with `Timer`
- remove:
  - optional arg to `permutations`
- replace iterables/yield with lists
- add ITERATIONS constant
"""

__author__ = "collinwinter@google.com (Collin Winter)"

## Python imports
#import optparse
#import re
#import string
#import time
#
## Local imports
#import util
#from compat import range


def permutations(iterable:List(int))->List(List(int)):
    """permutations(range(3), 2) --> (0,1) (0,2) (1,0) (1,2) (2,0) (2,1)"""
    pool = tuple(iterable)
    n = len(pool)
    r = n
    indices = list(range(n))
    cycles = list(range(n-r+1, n+1))[::-1]
    result = [ [pool[i] for i in indices[:r]] ]
    while n:
        for i in reversed(range(r)):
            cycles[i] -= 1
            if cycles[i] == 0:
                indices[i:] = indices[i+1:] + indices[i:i+1]
                cycles[i] = n - i
            else:
                j = cycles[i]
                indices[i], indices[-j] = indices[-j], indices[i]
                result.append([pool[i] for i in indices[:r]])
                break
        else:
            return result
    return result

## From http://code.activestate.com/recipes/576647/
def n_queens(queen_count:int)->List(List(int)):
    """N-Queens solver.

    Args:
        queen_count: the number of queens to solve for. This is also the
            board size.

    Yields:
        Solutions to the problem. Each yielded value is looks like
        (3, 8, 2, 1, 4, ..., 6) where each number is the column position for the
        queen, and the index into the tuple indicates the row.
    """
    cols = list(range(queen_count))
    res = []
    for vec in permutations(cols):
        if (queen_count == len(set(vec[i]+i for i in cols))
                        == len(set(vec[i]-i for i in cols))):
            res.append( list(vec) )
    return res


ITERATIONS = 10

if __name__ == "__main__":
    t = Timer()
    with t:
        for _ in range(ITERATIONS):
            list(n_queens(8))
