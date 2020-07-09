import pstat
import copy
import support
from typed_math import pow, sqrt, exp, abs, fabs, log, round, pi

####################################
#######  FREQUENCY STATS  ##########
####################################

def itemfreq(inlist:List(float))->List(List(float)):
    """
Returns a list of pairs.  Each pair consists of one of the scores in inlist
and it's frequency count.  Assumes a 1D list is passed.

Usage:   litemfreq(inlist)
Returns: a 2D frequency table (col [0:n-1]=scores, col n=frequencies)
"""
    scores = pstat.unique(inlist)
    scores.sort()
    freq = []
    for item in scores:
        freq.append(inlist.count(item))
    return pstat.abut(scores, freq)


def scoreatpercentile (inlist:List(float), percent:float)->float:
    """
Returns the score at a given percentile relative to the distribution
given by inlist.

Usage:   lscoreatpercentile(inlist,percent)
"""
    if percent > 1:
        #print("\nDividing percent>1 by 100 in lscoreatpercentile().\n")
        percent = percent / 100.0
    targetcf = percent*len(inlist)
    h, lrl, binsize, extras = histogram(inlist,10,[0,max(inlist)])
    cumhist = support.cumsum(copy.deepcopy(h))
    for i in range(len(cumhist)):
        if cumhist[i] >= targetcf:
            break
    score = binsize * ((targetcf - cumhist[i-1]) / float(h[i])) + (lrl+binsize*i)
    return score


def percentileofscore (inlist:List(float), score:int)->float:
    """
Returns the percentile value of a score relative to the distribution
given by inlist.  Formula depends on the values used to histogram the data(!).

Usage:   lpercentileofscore(inlist,score,histbins=10,defaultlimits=None)
"""
    histbins=10 #bg: was default argument
    defaultlimits=[0,max(inlist)] #None #bg: was a default argument
    h, lrl, binsize, extras = histogram(inlist,histbins,defaultlimits)
    cumhist = support.cumsum(copy.deepcopy(h))
    i = int((score - lrl)/float(binsize))
    pct = (cumhist[i-1]+((score-(lrl+binsize*i))/float(binsize))*h[i])/float(len(inlist)) * 100
    return pct


def histogram (inlist:List(float),numbins:int,defaultreallimits:(float,float))->(List(int),float,float,int):
    """
Returns (i) a list of histogram bin counts, (ii) the smallest value
of the histogram binning, and (iii) the bin width (the last 2 are not
necessarily integers).  Default number of bins is 10.  If no sequence object
is given for defaultreallimits, the routine picks (usually non-pretty) bins
spanning all the numbers in the inlist.

Usage:   lhistogram (inlist, numbins=10, defaultreallimits=None,suppressoutput=0)
Returns: list of bin values, lowerreallimit, binsize, extrapoints
"""
    printextras=0 #bg: was default argument
    if (defaultreallimits != None):
        if type(defaultreallimits) not in [list,tuple] or len(defaultreallimits)==1: # only one limit given, assumed to be lower one & upper is calc'd
            lowerreallimit = defaultreallimits
            upperreallimit = 1.000001 * max(inlist)
        else: # assume both limits given
            lowerreallimit = defaultreallimits[0]
            upperreallimit = defaultreallimits[1]
        binsize = (upperreallimit-lowerreallimit)/float(numbins)
    else:     # no limits given for histogram, both must be calc'd
        estbinwidth=(max(inlist)-min(inlist))/float(numbins) +1e-6 #1=>cover all
        binsize = ((max(inlist)-min(inlist)+estbinwidth))/float(numbins)
        lowerreallimit = min(inlist) - binsize/2 #lower real limit,1st bin
    bins = [0]*(numbins)
    extrapoints = 0
    for num in inlist:
        try:
            if (num-lowerreallimit) < 0:
                extrapoints = extrapoints + 1
            else:
                bintoincrement = int((num-lowerreallimit)/float(binsize))
                bins[bintoincrement] = bins[bintoincrement] + 1
        except:
            extrapoints = extrapoints + 1
    if (extrapoints > 0 and printextras == 1):
        print('\nPoints outside given histogram range =',extrapoints)
    return (bins, lowerreallimit, binsize, extrapoints)


def cumfreq(inlist:List(float))->(List(int),float,float,int):
    """
Returns a cumulative frequency histogram, using the histogram function.

Usage:   lcumfreq(inlist,numbins=10,defaultreallimits=None)
Returns: list of cumfreq bin values, lowerreallimit, binsize, extrapoints
"""
    numbins=10 #bg: was optional argument
    defaultreallimits=[0,max(inlist)] #None #bg# was optional argument
    h,l,b,e = histogram(inlist,numbins,defaultreallimits)
    cumhist = support.cumsum(copy.deepcopy(h))
    return cumhist,l,b,e

def relfreq(inlist:List(float))->(List(float),float,float,int):
    """
Returns a relative frequency histogram, using the histogram function.

Usage:   lrelfreq(inlist,numbins=10,defaultreallimits=None)
Returns: list of cumfreq bin values, lowerreallimit, binsize, extrapoints
"""
    numbins=10 #bg: was optional argument
    defaultreallimits=[0,max(inlist)] #None #bg: was optional argument
    h,l,b,e = histogram(inlist,numbins,defaultreallimits)
    #bg#h=dyn(h)
    h = h
    for i in range(len(h)):
        h[i] = h[i]/float(len(inlist))
    return h,l,b,e


