import pstat               # required 3rd party module
import central_tendency
import support
import probability
import variability
from typed_math import pow, sqrt, exp, abs, fabs, log, round, pi

####################################
#####  CORRELATION FUNCTIONS  ######
####################################

def pearsonr(x:List(float),y:List(float))->(float,float):
    """
Calculates a Pearson correlation coefficient and the associated
probability value.  Taken from Heiman's Basic Statistics for the Behav.
Sci (2nd), p.195.

Usage:   lpearsonr(x,y)      where x and y are equal-length lists
Returns: Pearson's r value, two-tailed p-value
"""
    TINY = 1.0e-30
    if len(x) != len(y):
        raise ValueError('Input values not paired in pearsonr.  Aborting.',x,y)
    n = len(x)
    x = list(map(float,x))
    y = list(map(float,y))
    xmean = central_tendency.mean(x)
    ymean = central_tendency.mean(y)
    r_num = n*(support.summult(x,y)) - sum(x)*sum(y)
    r_den = sqrt((n*support.ss(x) - support.square_of_sums(x))*(n*support.ss(y)-support.square_of_sums(y)))
    r = (r_num / r_den)  # denominator already a float
    df = n-2
    t = r*sqrt(df/((1.0-r+TINY)*(1.0+r+TINY)))
    prob = probability.betai(0.5*df,0.5,df/float(df+t*t))
    return r, prob


def lincc(x:float,y:float)->float:
    """
Calculates Lin's concordance correlation coefficient.

Usage:   alincc(x,y)    where x, y are equal-length arrays
Returns: Lin's CC
"""
    covar = lcov(x,y)*(len(x)-1)/float(len(x))  # correct denom to n
    xvar = lvar(x)*(len(x)-1)/float(len(x))  # correct denom to n
    yvar = lvar(y)*(len(y)-1)/float(len(y))  # correct denom to n
    _lincc = (2 * covar) / ((xvar+yvar) +((amean(x)-amean(y))**2))
    return _lincc


def spearmanr(x:List(float),y:List(float))->(float,float):
    """
Calculates a Spearman rank-order correlation coefficient.  Taken
from Heiman's Basic Statistics for the Behav. Sci (1st), p.192.

Usage:   lspearmanr(x,y)      where x and y are equal-length lists
Returns: Spearman's r, two-tailed p-value
"""
    TINY = 1e-30
    if len(x) != len(y):
        raise ValueError('Input values not paired in spearmanr.  Aborting.')
    n = len(x)
    rankx = rankdata(x)
    ranky = rankdata(y)
    dsq = sumdiffsquared(rankx,ranky)
    rs = 1 - 6*dsq / float(n*(n**2-1))
    t = rs * sqrt((n-2) / ((rs+1.0)*(1.0-rs)))
    df = n-2
    probrs = probability.betai(0.5*df,0.5,df/(df+t*t))  # t already a float
# probability values for rs are from part 2 of the spearman function in
# Numerical Recipies, p.510.  They are close to tables, but not exact. (?)
    return rs, probrs


def pointbiserialr(x:List(float),y:List(float))->(float,float):
    """
Calculates a point-biserial correlation coefficient and the associated
probability value.  Taken from Heiman's Basic Statistics for the Behav.
Sci (1st), p.194.

Usage:   lpointbiserialr(x,y)      where x,y are equal-length lists
Returns: Point-biserial r, two-tailed p-value
"""
    TINY = 1e-30
    if len(x) != len(y):
        raise ValueError('INPUT VALUES NOT PAIRED IN pointbiserialr.  ABORTING.')
    data = pstat.abut(x,y)
    categories = pstat.unique(x)
    if len(categories) != 2:
        raise ValueError("Exactly 2 categories required for pointbiserialr().")
    else:   # there are 2 categories, continue
        codemap = pstat.abut(categories,list(range(2)))
        recoded = pstat.recode(data,codemap,0)
        _x = pstat.linexand(data,0,categories[0])
        _y = pstat.linexand(data,0,categories[1])
        xmean = central_tendency.mean(pstat.colex(_x,1))
        ymean = central_tendency.mean(pstat.colex(_y,1))
        n = len(data)
        adjust = sqrt((len(_x)/float(n))*(len(_y)/float(n)))
        rpb = (ymean - xmean)/variability.samplestdev(pstat.colex(data,1))*adjust
        df = n-2
        t = rpb*sqrt(df/((1.0-rpb+TINY)*(1.0+rpb+TINY)))
        prob = probability.betai(0.5*df,0.5,df/(df+t*t))  # t already a float
        return rpb, prob


def kendalltau(x:List(float),y:List(float))->(float,float):
    """
Calculates Kendall's tau ... correlation of ordinal data.  Adapted
from function kendl1 in Numerical Recipies.  Needs good test-routine.@@@

Usage:   lkendalltau(x,y)
Returns: Kendall's tau, two-tailed p-value
"""
    n1 = 0
    n2 = 0
    iss = 0
    for j in range(len(x)-1):
        for k in range(j,len(y)):
            a1 = x[j] - x[k]
            a2 = y[j] - y[k]
            aa = a1 * a2
            if (aa):             # neither list has a tie
                n1 = n1 + 1
                n2 = n2 + 1
                if aa > 0:
                    iss = iss + 1
                else:
                    iss = iss -1
            else:
                if (a1):
                    n1 = n1 + 1
                else:
                    n2 = n2 + 1
    tau = iss / sqrt(n1*n2)
    svar = (4.0*len(x)+10.0) / (9.0*len(x)*(len(x)-1))
    _z = tau / sqrt(svar)
    prob = probability.erfcc(abs(_z)/1.4142136)
    return tau, prob


def linregress(x:List(float),y:List(float))->(float,float,float,float,float):
    """
Calculates a regression line on x,y pairs.

Usage:   llinregress(x,y)      x,y are equal-length lists of x-y coordinates
Returns: slope, intercept, r, two-tailed prob, sterr-of-estimate
"""
    TINY = 1.0e-20
    if len(x) != len(y):
        raise ValueError('Input values not paired in linregress.  Aborting.')
    n = len(x)
    x = list(map(float,x))
    y = list(map(float,y))
    xmean = central_tendency.mean(x)
    ymean = central_tendency.mean(y)
    r_num = float(n*(support.summult(x,y)) - sum(x)*sum(y))
    r_den = sqrt((n*support.ss(x) - support.square_of_sums(x))*(n*support.ss(y)-support.square_of_sums(y)))
    r = r_num / r_den
    z = 0.5*log((1.0+r+TINY)/(1.0-r+TINY))
    df = n-2
    t = r*sqrt(df/((1.0-r+TINY)*(1.0+r+TINY)))
    prob = probability.betai(0.5*df,0.5,df/(df+t*t))
    slope = r_num / float(n*support.ss(x) - support.square_of_sums(x))
    intercept = ymean - slope*xmean
    sterrest = sqrt(1-r*r)*variability.samplestdev(y)
    return slope, intercept, r, prob, sterrest


