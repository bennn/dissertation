import pstat
import copy
import frequency
import support
from typed_math import pow, sqrt, exp, abs, fabs, log, round, pi

####################################
#######  CENTRAL TENDENCY  #########
####################################

def geometricmean (inlist:List(float))->float:
    """
Calculates the geometric mean of the values in the passed list.
That is:  n-th root of (x1 * x2 * ... * xn).  Assumes a '1D' list.

Usage:   lgeometricmean(inlist)
"""
    mult = 1.0
    one_over_n = 1.0/len(inlist)
    for item in inlist:
        mult = mult * pow(item,one_over_n)
    return mult


def harmonicmean (inlist:List(float))->float:
    """
Calculates the harmonic mean of the values in the passed list.
That is:  n / (1/x1 + 1/x2 + ... + 1/xn).  Assumes a '1D' list.

Usage:   lharmonicmean(inlist)
"""
    sum = 0
    for item in inlist:
        sum = sum + 1.0/item
    return len(inlist) / sum


def mean (inlist:List(float))->float:
    """
Returns the arithematic mean of the values in the passed list.
Assumes a '1D' list, but will function on the 1st dim of an array(!).

Usage:   lmean(inlist)
"""
    sum = 0
    for item in inlist:
        sum = sum + item
    return sum/float(len(inlist))


def median(inlist:List(float))->float:
    """
Returns the computed median value of a list of numbers, given the
number of bins to use for the histogram (more bins brings the computed value
closer to the median score, default number of bins = 1000).  See G.W.
Heiman's Basic Stats (1st Edition), or CRC Probability & Statistics.

Usage:   lmedian (inlist, numbins=1000)
"""
    numbins=1000
    (hist, smallest, binsize, extras) = frequency.histogram(inlist,numbins,[min(inlist),max(inlist)]) # make histog
    cumhist = support.cumsum(hist)              # make cumulative histogram
    for i in range(len(cumhist)):        # get 1st(!) index holding 50%ile score
        if cumhist[i]>=len(inlist)/2.0:
            cfbin = i
            break
    LRL = smallest + binsize*cfbin        # get lower read limit of that bin
    cfbelow = cumhist[cfbin-1]
    freq = float(hist[cfbin])                # frequency IN the 50%ile bin
    _median = LRL + ((len(inlist)/2.0 - cfbelow)/float(freq))*binsize  # median formula
    return _median


def medianscore (inlist:List(float))->float:
    """
Returns the 'middle' score of the passed list.  If there is an even
number of scores, the mean of the 2 middle scores is returned.

Usage:   lmedianscore(inlist)
"""

    newlist = copy.deepcopy(inlist)
    newlist.sort()
    if len(newlist) % 2 == 0:   # if even number of scores, average middle 2
        index = len(newlist)//2  # integer division correct
        median = float(newlist[index] + newlist[index-1]) /2
    else:
        index = len(newlist)//2  # int divsion gives mid value when count from 0
        median = newlist[index]
    return median


def mode(inlist:List(float))->(int, List(float)):
    """
Returns a list of the modal (most common) score(s) in the passed
list.  If there is more than one such score, all are returned.  The
bin-count for the mode(s) is also returned.

Usage:   lmode(inlist)
Returns: bin-count for mode(s), a list of modal value(s)
"""

    scores = pstat.unique(inlist)
    scores.sort()
    freq = []
    for item in scores:
        freq.append(inlist.count(item))
    maxfreq = max(freq)
    _mode = []
    stillmore = 1
    while stillmore:
        try:
            indx = freq.index(maxfreq)
            _mode.append(scores[indx])
            del freq[indx]
            del scores[indx]
        except ValueError:
            stillmore=0
    return maxfreq, _mode


