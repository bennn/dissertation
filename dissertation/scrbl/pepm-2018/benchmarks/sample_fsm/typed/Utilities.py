from retic import List, Float, Int
import os, itertools
fname = os.path.join(os.path.dirname(__file__), "util-random-numbers.txt")
#TODO: Cannot type variable in retic
rand_num = itertools.cycle((float(line.strip()) for line in open(fname, "r")))

def accumulated_s(probabilities:List(Float))->List(Float):
    total = sum(probabilities)
    payoffs = probabilities
    result = []
    next = 0
    for element in payoffs:
        next += element
        result = result + [next/total]
    return result

def choose_randomly(probabilities:List(Float), speed:Int)->List(Int):

    s = accumulated_s(probabilities)
    res = []  ### changed here
    for n in range(speed):
        #r = random()
        r = next(rand_num)
        for i in range(len(s)):
            if r < s[i]:
                res = res + [i]   ### and here
                break
    return res  ### and here

def relative_average(l: List(Float), w: Float) -> Float:
    return sum(l) / w / len(l)

