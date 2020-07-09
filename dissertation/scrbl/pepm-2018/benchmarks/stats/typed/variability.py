import central_tendency
import support
import pstat               # required 3rd party module
import copy  # required python modules
from typed_math import pow, sqrt, exp, abs, fabs, log, round, pi

####################################
#####  VARIABILITY FUNCTIONS  ######
####################################

def obrientransform(args:List(List(float)))->List(List(float)):
    """
Computes a transform on input data (any number of columns).  Used to
test for homogeneity of variance prior to running one-way stats.  From
Maxwell and Delaney, p.112.

Usage:   lobrientransform(*args)
Returns: transformed data for use in an ANOVA
"""
    TINY = 1e-10
    k = len(args)
    n = [0]*k
    v = [0.0]*k
    m = [0.0]*k
    nargs = []
    for i in range(k):
        nargs.append(copy.deepcopy(args[i]))
        n[i] = len(nargs[i])
        v[i] = var([float(na) for na in nargs[i]])
        m[i] = central_tendency.mean([float(na) for na in nargs[i]])
    for j in range(k):
        for i in range(n[j]):
            t1 = (n[j]-1.5)*n[j]*(nargs[j][i]-m[j])**2
            t2 = 0.5*v[j]*(n[j]-1.0)
            t3 = (n[j]-1.0)*(n[j]-2.0)
            nargs[j][i] = (t1-t2) / float(t3)
    check = 1
    for j in range(k):
        if v[j] - central_tendency.mean(nargs[j]) > TINY:
            check = 0
    if check != 1:
        raise ValueError('Problem in obrientransform.')
    else:
        return nargs


def samplevar (inlist:List(float))->float:
    """
Returns the variance of the values in the passed list using
N for the denominator (i.e., DESCRIBES the sample variance only).

Usage:   lsamplevar(inlist)
"""
    n = len(inlist)
    mn = central_tendency.mean(inlist)
    deviations = []
    for item in inlist:
        deviations.append(item-mn)
    return support.ss(deviations)/float(n)


def samplestdev (inlist:List(float))->float:
    """
Returns the standard deviation of the values in the passed list using
N for the denominator (i.e., DESCRIBES the sample stdev only).

Usage:   lsamplestdev(inlist)
"""
    return sqrt(samplevar(inlist))


def cov (x:List(float),y:List(float))->float:
    """
Returns the estimated covariance of the values in the passed
array (i.e., N-1).  Dimension can equal None (ravel array first), an
integer (the dimension over which to operate), or a sequence (operate
over multiple dimensions).  Set keepdims=1 to return an array with the
same number of dimensions as inarray.

Usage:   lcov(x,y,keepdims=0)
"""
    keepdims=0 #bg: was optional argument
    n = len(x)
    xmn = central_tendency.mean(x)
    ymn = central_tendency.mean(y)
    xdeviations = [0]*len(x)
    ydeviations = [0]*len(y)
    for i in range(len(x)):
        xdeviations[i] = x[i] - xmn
        ydeviations[i] = y[i] - ymn
    ss = 0.0
    for i in range(len(xdeviations)):
        ss = ss + xdeviations[i]*ydeviations[i]
    return ss/float(n-1)


def var (inlist:List(float))->float:
    """
Returns the variance of the values in the passed list using N-1
for the denominator (i.e., for estimating population variance).

Usage:   lvar(inlist)
"""
    n = len(inlist)
    mn = central_tendency.mean(inlist)
    #bg#deviations = dyn([0]*len(inlist))
    deviations = [0]*len(inlist)
    for i in range(len(inlist)):
        deviations[i] = inlist[i] - mn
    return support.ss(deviations)/float(n-1)


def stdev (inlist:List(float))->float:
    """
Returns the standard deviation of the values in the passed list
using N-1 in the denominator (i.e., to estimate population stdev).

Usage:   lstdev(inlist)
"""
    return sqrt(var(inlist))


def sterr(inlist:List(float))->float:
    """
Returns the standard error of the values in the passed list using N-1
in the denominator (i.e., to estimate population standard error).

Usage:   lsterr(inlist)
"""
    return stdev(inlist) / float(sqrt(len(inlist)))


def sem (inlist:List(float))->float:
    """
Returns the estimated standard error of the mean (sx-bar) of the
values in the passed list.  sem = stdev / sqrt(n)

Usage:   lsem(inlist)
"""
    sd = stdev(inlist)
    n = len(inlist)
    return sd/sqrt(n)


def z (inlist:List(float), score:float)->float:
    """
Returns the z-score for a given input score, given that score and the
list from which that score came.  Not appropriate for population calculations.

Usage:   lz(inlist, score)
"""
    _z = (score-central_tendency.mean(inlist))/samplestdev(inlist)
    return _z


def zs (inlist:List(float))->List(float):
    """
Returns a list of z-scores, one for each score in the passed list.

Usage:   lzs(inlist)
"""
    zscores = []
    for item in inlist:
        zscores.append(z(inlist,item))
    return zscores


