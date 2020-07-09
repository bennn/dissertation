# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
# Contributed by Sebastien Loisel
# Fixed by Isaac Gouy
# Sped up by Josh Goldfoot
# Dirtily sped up by Simon Descarpentries
# Concurrency by Jason Stitt

from Timer import Timer

"""
bg:
- remove `main` function

note, I don't see any concurrency in this code
"""


def eval_A(i:float, j:float) -> float:
    return 1.0 / ((i + j) * (i + j + 1) // 2 + i + 1)

def eval_times_u(func:Function([Tuple(float, List(float))], float), u:List(float)) -> List(float):
    return [func((i,u)) for i in range(len(list(u)))]


def eval_AtA_times_u(u:List(float))->List(float):
    return eval_times_u(part_At_times_u, eval_times_u(part_A_times_u, u))

def part_A_times_u(i_u:(float, List(float)))->float:
    i, u = i_u
    partial_sum = 0
    for j, u_j in enumerate(u):
        partial_sum += eval_A(i, j) * u_j
    return partial_sum

def part_At_times_u(i_u:(float, List(float)))->float:
    i, u = i_u
    partial_sum = 0
    for j, u_j in enumerate(u):
        partial_sum += eval_A(j, i) * u_j
    return partial_sum

DEFAULT_N = 130

if __name__ == "__main__":
  t = Timer()
  with t:
    u = [1] * DEFAULT_N

    for dummy in range(10):
        v = eval_AtA_times_u(u)
        u = eval_AtA_times_u(v)

    vBv = vv = 0

    for ue, ve in zip(u, v):
        vBv += ue * ve
        vv  += ve * ve
