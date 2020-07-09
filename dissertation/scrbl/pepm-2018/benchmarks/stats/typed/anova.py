import pstat               # required 3rd party module
import central_tendency
import variability
import probability
import support
from typed_math import pow, sqrt, exp, abs, fabs, log, round, pi

####################################
#######  ANOVA CALCULATIONS  #######
####################################

def F_oneway(lists:List(List(float)))->(float,float):
    """
Performs a 1-way ANOVA, returning an F-value and probability given
any number of groups.  From Heiman, pp.394-7.

Usage:   F_oneway(*lists)    where *lists is any number of lists, one per
                                  treatment group
Returns: F value, one-tailed p-value
"""
    a = len(lists)           # ANOVA on 'a' groups, each in it's own list
    means = [0.]*a
    vars = [0.]*a
    ns = [0]*a
    alldata = []
    tmp = list(map(list,lists))
    means = list(map(central_tendency.mean,tmp))
    vars = list(map(variability.var,tmp))
    ns = list(map(len,lists))
    for i in range(len(lists)):
        alldata = alldata + lists[i]
    bign = len(alldata)
    sstot = support.ss(alldata)-(support.square_of_sums(alldata)/float(bign))
    ssbn = 0
    for _list in lists:
        ssbn = ssbn + support.square_of_sums(_list)/float(len(_list))
    ssbn = ssbn - (support.square_of_sums(alldata)/float(bign))
    sswn = sstot-ssbn
    dfbn = a-1
    dfwn = bign - a
    msb = ssbn/float(dfbn)
    msw = sswn/float(dfwn)
    f = msb/msw
    prob = probability.fprob(dfbn,dfwn,f)
    return f, prob


def F_value (ER:float,EF:float,dfnum:float,dfden:float)->float:
    """
Returns an F-statistic given the following:
        ER  = error associated with the null hypothesis (the Restricted model)
        EF  = error associated with the alternate hypothesis (the Full model)
        dfR-dfF = degrees of freedom of the numerator
        dfF = degrees of freedom associated with the denominator/Full model

Usage:   lF_value(ER,EF,dfnum,dfden)
"""
    return ((ER-EF)/float(dfnum) / (EF/float(dfden)))


