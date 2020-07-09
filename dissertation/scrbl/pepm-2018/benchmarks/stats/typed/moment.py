from typed_math import pow, sqrt, exp, abs, fabs, log, round, pi
import central_tendency
import variability

####################################
############  MOMENTS  #############
####################################

def moment(inlist:List(float),moment:int)->float:
    """
Calculates the nth moment about the mean for a sample (defaults to
the 1st moment).  Used to calculate coefficients of skewness and kurtosis.

Usage:   lmoment(inlist,moment=1)
Returns: appropriate moment (r) from ... 1/n * SUM((inlist(i)-mean)**r)
"""
    if moment == 1:
        return 0.0
    else:
        mn = central_tendency.mean(inlist)
        n = len(inlist)
        s = 0
        for x in inlist:
            s = s + (x-mn)**moment
        return s/float(n)


def variation(inlist:List(float))->float:
    """
Returns the coefficient of variation, as defined in CRC Standard
Probability and Statistics, p.6.

Usage:   lvariation(inlist)
"""
    return 100.0*variability.samplestdev(inlist)/float(central_tendency.mean(inlist))


def skew(inlist:List(float))->float:
    """
Returns the skewness of a distribution, as defined in Numerical
Recipies (alternate defn in CRC Standard Probability and Statistics, p.6.)

Usage:   lskew(inlist)
"""
    return moment(inlist,3)/pow(moment(inlist,2),1.5)


def kurtosis(inlist:List(float))->float:
    """
Returns the kurtosis of a distribution, as defined in Numerical
Recipies (alternate defn in CRC Standard Probability and Statistics, p.6.)

Usage:   lkurtosis(inlist)
"""
    return moment(inlist,4)/pow(moment(inlist,2),2.0)


def describe(inlist:List(float))->(int, (float, float), float, float, float, float):
    """
Returns some descriptive statistics of the passed list (assumed to be 1D).

Usage:   ldescribe(inlist)
Returns: n, mean, standard deviation, skew, kurtosis
"""
    n = len(inlist)
    mm = (min(inlist),max(inlist))
    m = central_tendency.mean(inlist)
    sd = variability.stdev(inlist)
    sk = skew(inlist)
    kurt = kurtosis(inlist)
    return n, mm, m, sd, sk, kurt


