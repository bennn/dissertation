from streams import Stream, make_stream, stream_unfold, stream_get
from Timer import Timer

def count_from(n:int)->Stream:
    def thunk()->Stream:
        return count_from(n + 1)
    return make_stream(n, thunk)

def sift(n:int, st:Stream)->Stream:
    hd, tl = stream_unfold(st)
    while (0 == (hd % n)):
        hd, tl = stream_unfold(tl)
    def thunk()->Stream:
        return sift(n, tl)
    return make_stream(hd, thunk)

def sieve(st:Stream)->Stream:
    hd, tl = stream_unfold(st)
    def thunk()->Stream:
        return sieve(sift(hd, tl))
    return make_stream(hd, thunk)

primes = sieve(count_from(2))

N_1 = 2000 #6666

if __name__ == "__main__":
    sys.setrecursionlimit(10 ** 5)
    t = Timer()
    with t:
        print(stream_get(primes, N_1))
    import sys
