import pstat
import central_tendency
import probability
import variability
import support
from typed_math import pow, sqrt, exp, abs, fabs, log, round, pi

####################################
#####  INFERENTIAL STATISTICS  #####
####################################

def ttest_1samp(a:List(float),popmean:int)->(float,float):
    """
Calculates the t-obtained for the independent samples T-test on ONE group
of scores a, given a population mean.  If printit=1, results are printed
to the screen.  If printit='filename', the results are output to 'filename'
using the given writemode (default=append).  Returns t-value, and prob.

Usage:   lttest_1samp(a,popmean,Name='Sample',printit=0,writemode='a')
Returns: t-value, two-tailed prob
"""
    printit=0     #bg: optional arg
    name='Sample' #bg: optional arg
    writemode='a' #bg: optional arg
    x = central_tendency.mean(a)
    v = variability.var(a)
    n = len(a)
    df = n-1
    svar = ((n-1)*v)/float(df)
    t = (x-popmean)/sqrt(svar*(1.0/n))
    prob = probability.betai(0.5*df,0.5,float(df)/(df+t*t))

    if printit != 0:
        statname = 'Single-sample T-test.'
        outputpairedstats(printit,writemode,
                          'Population','--',popmean,0,0,0,
                          name,n,x,v,min(a),max(a),
                          statname,t,prob)
    return t,prob


def ttest_ind (a:List(float), b:List(float))->(float,float):
    """
Calculates the t-obtained T-test on TWO INDEPENDENT samples of
scores a, and b.  From Numerical Recipies, p.483.  If printit=1, results
are printed to the screen.  If printit='filename', the results are output
to 'filename' using the given writemode (default=append).  Returns t-value,
and prob.

Usage:   lttest_ind(a,b,printit=0,name1='Samp1',name2='Samp2',writemode='a')
Returns: t-value, two-tailed prob
"""
    printit=0; name1='Samp1'; name2='Samp2'; writemode='a'; #bg: optional args
    x1 = central_tendency.mean(a)
    x2 = central_tendency.mean(b)
    v1 = variability.stdev(a)**2
    v2 = variability.stdev(b)**2
    n1 = len(a)
    n2 = len(b)
    df = n1+n2-2
    svar = ((n1-1)*v1+(n2-1)*v2)/float(df)
    t = (x1-x2)/sqrt(svar*(1.0/n1 + 1.0/n2))
    prob = probability.betai(0.5*df,0.5,df/(df+t*t))

    if printit != 0:
        statname = 'Independent samples T-test.'
        outputpairedstats(printit,writemode,
                          name1,n1,x1,v1,min(a),max(a),
                          name2,n2,x2,v2,min(b),max(b),
                          statname,t,prob)
    return t,prob


def ttest_rel (a:List(float),b:List(float))->(float,float):
    """
Calculates the t-obtained T-test on TWO RELATED samples of scores,
a and b.  From Numerical Recipies, p.483.  If printit=1, results are
printed to the screen.  If printit='filename', the results are output to
'filename' using the given writemode (default=append).  Returns t-value,
and prob.

Usage:   lttest_rel(a,b,printit=0,name1='Sample1',name2='Sample2',writemode='a')
Returns: t-value, two-tailed prob
"""
    printit=0;name1='Sample1';name2='Sample2';writemode='a' #bg: optional arg
    if len(a)!=len(b):
        raise ValueError('Unequal length lists in ttest_rel.')
    x1 = central_tendency.mean(a)
    x2 = central_tendency.mean(b)
    v1 = variability.var(a)
    v2 = variability.var(b)
    n = len(a)
    cov = 0
    for i in range(len(a)):
        cov = cov + (a[i]-x1) * (b[i]-x2)
    df = n-1
    _cov = cov / float(df)
    sd = sqrt((v1+v2 - 2.0*_cov)/float(n))
    t = (x1-x2)/sd
    prob = probability.betai(0.5*df,0.5,df/(df+t*t))

    if printit != 0:
        statname = 'Related samples T-test.'
        outputpairedstats(printit,writemode,
                          name1,n,x1,v1,min(a),max(a),
                          name2,n,x2,v2,min(b),max(b),
                          statname,t,prob)
    return t, prob

def chisquare(f_obs:List(float))->(float,float):
    """
Calculates a one-way chi square for list of observed frequencies and returns
the result.  If no expected frequencies are given, the total N is assumed to
be equally distributed across all groups.

Usage:   lchisquare(f_obs, f_exp=None)   f_obs = list of observed cell freq.
Returns: chisquare-statistic, associated p-value
"""
    f_exp=None #bg: optional arg
    k = len(f_obs)                 # number of groups
    if f_exp == None:
        f_exp = [sum(f_obs)/float(k)] * len(f_obs) # create k bins with = freq.
    chisq = 0
    for i in range(len(f_obs)):
        chisq = chisq + (f_obs[i]-f_exp[i])**2 / float(f_exp[i])
    return chisq, probability.chisqprob(chisq, k-1)


def ks_2samp (data1:List(float),data2:List(float))->(float,float):
    """
Computes the Kolmogorov-Smirnof statistic on 2 samples.  From
Numerical Recipies in C, page 493.

Usage:   lks_2samp(data1,data2)   data1&2 are lists of values for 2 conditions
Returns: KS D-value, associated p-value
"""
    j1 = 0
    j2 = 0
    fn1 = 0.0
    fn2 = 0.0
    n1 = len(data1)
    n2 = len(data2)
    en1 = n1
    en2 = n2
    d = 0.0
    data1.sort()
    data2.sort()
    while j1 < n1 and j2 < n2:
        d1=data1[j1]
        d2=data2[j2]
        if d1 <= d2:
            fn1 = (j1)/float(en1)
            j1 = j1 + 1
        if d2 <= d1:
            fn2 = (j2)/float(en2)
            j2 = j2 + 1
        dt = (fn2-fn1)
        if fabs(dt) > fabs(d):
            d = dt
    try:
        en = sqrt(en1*en2/float(en1+en2))
        prob = ksprob((en+0.12+0.11/en)*abs(d))
    except:
        prob = 1.0
    return d, prob


def mannwhitneyu(x:List(float),y:List(float))->(float,float):
    """
Calculates a Mann-Whitney U statistic on the provided scores and
returns the result.  Use only when the n in each condition is < 20 and
you have 2 independent samples of ranks.  NOTE: Mann-Whitney U is
significant if the u-obtained is LESS THAN or equal to the critical
value of U found in the tables.  Equivalent to Kruskal-Wallis H with
just 2 groups.

Usage:   lmannwhitneyu(data)
Returns: u-statistic, one-tailed p-value (i.e., p(z(U)))
"""
    n1 = len(x)
    n2 = len(y)
    ranked = support.rankdata(x+y)
    rankx = ranked[0:n1]       # get the x-ranks
    ranky = ranked[n1:]        # the rest are y-ranks
    u1 = n1*n2 + (n1*(n1+1))/2.0 - sum(rankx)  # calc U for x
    u2 = n1*n2 - u1                            # remainder is U for y
    bigu = max(u1,u2)
    smallu = min(u1,u2)
    proportion = bigu/float(n1*n2)
    T = sqrt(tiecorrect(ranked))  # correction factor for tied scores
    if T == 0:
        raise ValueError('All numbers are identical in lmannwhitneyu')
    sd = sqrt(T*n1*n2*(n1+n2+1)/12.0)
    z = abs((bigu-n1*n2/2.0) / sd)  # normal approximation for prob calc
    return smallu, 1.0 - probability.zprob(z) #, proportion


def tiecorrect(rankvals:List(float))->float:
    """
Corrects for ties in Mann Whitney U and Kruskal Wallis H tests.  See
Siegel, S. (1956) Nonparametric Statistics for the Behavioral Sciences.
New York: McGraw-Hill.  Code adapted from |Stat rankind.c code.

Usage:   ltiecorrect(rankvals)
Returns: T correction factor for U or H
"""
    sorted,posn = support.shellsort(rankvals)
    n = len(sorted)
    T = 0.0
    i = 0
    while (i<n-1):
        if sorted[i] == sorted[i+1]:
            nties = 1
            while (i<n-1) and (sorted[i] == sorted[i+1]):
                nties = nties +1
                i = i +1
            T = T + nties**3 - nties
        i = i+1
    T = T / float(n**3-n)
    return 1.0 - T


def ranksums(x:List(float), y:List(float))->(float,float):
    """
Calculates the rank sums statistic on the provided scores and
returns the result.  Use only when the n in each condition is > 20 and you
have 2 independent samples of ranks.

Usage:   lranksums(x,y)
Returns: a z-statistic, two-tailed p-value
"""
    n1 = len(x)
    n2 = len(y)
    alldata = x+y
    ranked = support.rankdata(alldata)
    x = ranked[:n1]
    y = ranked[n1:]
    s = sum(x)
    expected = n1*(n1+n2+1) / 2.0
    _z = (s - expected) / sqrt(n1*n2*(n1+n2+1)/12.0)
    prob = 2*(1.0 -probability.zprob(abs(_z)))
    return _z, prob


def wilcoxont(x:List(float), y:List(float))->(float,float):
    """
Calculates the Wilcoxon T-test for related samples and returns the
result.  A non-parametric T-test.

Usage:   lwilcoxont(x,y)
Returns: a t-statistic, two-tail probability estimate
"""
    if len(x) != len(y):
        raise ValueError('Unequal N in wilcoxont.  Aborting.')
    d=[]
    for i in range(len(x)):
        diff = x[i] - y[i]
        if diff != 0:
            d.append(diff)
    count = len(d)
    absd = list(map(abs,d))
    absranked = support.rankdata(absd)
    r_plus = 0.0
    r_minus = 0.0
    for i in range(len(absd)):
        if d[i] < 0:
            r_minus = r_minus + absranked[i]
        else:
            r_plus = r_plus + absranked[i]
    wt = min(r_plus, r_minus)
    mn = count * (count+1) * 0.25
    se =  sqrt(count*(count+1)*(2.0*count+1.0)/24.0)
    _z = fabs(wt-mn) / se
    prob = 2*(1.0 -probability.zprob(abs(_z)))
    return wt, prob


def kruskalwallish(args:List(List(float)))->(float,float):
    """
The Kruskal-Wallis H-test is a non-parametric ANOVA for 3 or more
groups, requiring at least 5 subjects in each group.  This function
calculates the Kruskal-Wallis H-test for 3 or more independent samples
and returns the result.

Usage:   lkruskalwallish(*args)
Returns: H-statistic (corrected for ties), associated p-value
"""
    args = list(args)
    n = [0]*len(args)
    all = []
    n = list(map(len,args))
    for i in range(len(args)):
        all = all + args[i]
    ranked = support.rankdata(all)
    T = tiecorrect(ranked)
    for i in range(len(args)):
        args[i] = ranked[0:n[i]]
        del ranked[0:n[i]]
    rsums = []
    for i in range(len(args)):
        rsums.append(sum(args[i])**2)
        rsums[i] = rsums[i] / float(n[i])
    ssbn = sum(rsums)
    totaln = sum(n)
    h = 12.0 / (totaln*(totaln+1)) * ssbn - 3*(totaln+1)
    df = len(args) - 1
    if T == 0:
        raise ValueError('All numbers are identical in lkruskalwallish')
    h = h / float(T)
    return h, probability.chisqprob(h,df)

def friedmanchisquare(args:List(List(float)))->(float,float):
    """
Friedman Chi-Square is a non-parametric, one-way within-subjects
ANOVA.  This function calculates the Friedman Chi-square test for repeated
measures and returns the result, along with the associated probability
value.  It assumes 3 or more repeated measures.  Only 3 levels requires a
minimum of 10 subjects in the study.  Four levels requires 5 subjects per
level(??).

Usage:   lfriedmanchisquare(*args)
Returns: chi-square statistic, associated p-value
"""
    k = len(args)
    if k < 3:
        raise ValueError('Less than 3 levels.  Friedman test not appropriate.')
    if k > 3:
        raise ValueError('bg: specialized this code to 3 levels for Reticulated experiment')
    n = len(args[0])
    #bg#data = pstat.abut(l0,l1,l2)
    data = pstat.abut(pstat.abut(args[0], args[1]),args[2])
    for i in range(len(data)):
        data[i] = support.rankdata(data[i])
    ssbn = 0
    for i in range(k):
        ssbn = ssbn + sum(args[i])**2
    chisq = 12.0 / (k*n*(k+1)) * ssbn - 3*n*(k+1)
    return chisq, probability.chisqprob(chisq,k-1)


