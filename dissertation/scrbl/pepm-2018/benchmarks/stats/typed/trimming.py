from typed_math import pow, sqrt, exp, abs, fabs, log, round, pi

####################################
#######  TRIMMING FUNCTIONS  #######
####################################

def trimboth (l:List(float),proportiontocut:float)->List(float):
    """
Slices off the passed proportion of items from BOTH ends of the passed
list (i.e., with proportiontocut=0.1, slices 'leftmost' 10% AND 'rightmost'
10% of scores.  Assumes list is sorted by magnitude.  Slices off LESS if
proportion results in a non-integer slice index (i.e., conservatively
slices off proportiontocut).

Usage:   ltrimboth (l,proportiontocut)
Returns: trimmed version of list l
"""
    lowercut = int(proportiontocut*len(l))
    uppercut = len(l) - lowercut
    return l[lowercut:uppercut]


def trim1 (l:List(float),proportiontocut:float)->List(float):
    """
Slices off the passed proportion of items from ONE end of the passed
list (i.e., if proportiontocut=0.1, slices off 'leftmost' or 'rightmost'
10% of scores).  Slices off LESS if proportion results in a non-integer
slice index (i.e., conservatively slices off proportiontocut).

Usage:   ltrim1 (l,proportiontocut,tail='right')  or set tail='left'
Returns: trimmed version of list l
"""
    tail='right' #bg: was optional argument
    if tail == 'right':
        lowercut = 0
        uppercut = len(l) - int(proportiontocut*len(l))
    elif tail == 'left':
        lowercut = int(proportiontocut*len(l))
        uppercut = len(l)
    return l[lowercut:uppercut]

