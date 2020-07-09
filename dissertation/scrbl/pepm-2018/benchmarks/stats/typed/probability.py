from typed_math import pow, sqrt, exp, abs, fabs, log, round, pi

####################################
####  PROBABILITY CALCULATIONS  ####
####################################

#bg: lifted `ex` to top-level
def ex(x:float,BIG:float)->float:
    if x < -BIG:
        return 0.0
    else:
        return exp(x)

def chisqprob(chisq:float,df:int)->float:
    """
Returns the (1-tailed) probability value associated with the provided
chi-square value and df.  Adapted from chisq.c in Gary Perlman's |Stat.

Usage:   lchisqprob(chisq,df)
"""
    BIG = 20.0
    if chisq <=0 or df < 1:
        return 1.0
    a = 0.5 * chisq
    if df%2 == 0:
        even = 1
    else:
        even = 0
    if df > 1:
        y = ex(-a,BIG)
    if even:
        s = y
    else:
        s = 2.0 * zprob(-sqrt(chisq))
    if (df > 2):
        chisq = 0.5 * (df - 1.0)
        if even:
            _z = 1.0
        else:
            _z = 0.5
        if a > BIG:
            if even:
                e = 0.0
            else:
                e = log(sqrt(pi))
            c = log(a)
            while (_z <= chisq):
                e = log(_z) + e
                s = s + ex(c*_z-a-e,BIG)
                _z = _z + 1.0
            return s
        else:
            if even:
                e = 1.0
            else:
                e = 1.0 / sqrt(pi) / sqrt(a)
            c = 0.0
            while (_z <= chisq):
                e = e * (a/float(_z))
                c = c + e
                _z = _z + 1.0
            return (c*y+s)
    else:
        return s


def erfcc(x:float)->float:
    """
Returns the complementary error function erfc(x) with fractional
error everywhere less than 1.2e-7.  Adapted from Numerical Recipies.

Usage:   lerfcc(x)
"""
    _z = abs(x)
    t = 1.0 / (1.0+0.5*_z)
    ans = t * exp(-_z*_z-1.26551223 + t*(1.00002368+t*(0.37409196+t*(0.09678418+t*(-0.18628806+t*(0.27886807+t*(-1.13520398+t*(1.48851587+t*(-0.82215223+t*0.17087277)))))))))
    if x >= 0:
        return ans
    else:
        return 2.0 - ans


def zprob(_z:float)->float:
    """
Returns the area under the normal curve 'to the left of' the given z value.
Thus,
    for z<0, zprob(z) = 1-tail probability
    for z>0, 1.0-zprob(z) = 1-tail probability
    for any z, 2.0*(1.0-zprob(abs(z))) = 2-tail probability
Adapted from z.c in Gary Perlman's |Stat.

Usage:   lzprob(z)
"""
    Z_MAX = 6.0    # maximum meaningful z-value
    if _z == 0.0:
        x = 0.0
    else:
        y = 0.5 * fabs(_z)
        if y >= (Z_MAX*0.5):
            x = 1.0
        elif (y < 1.0):
            w = y*y
            x = ((((((((0.000124818987 * w
                        -0.001075204047) * w +0.005198775019) * w
                      -0.019198292004) * w +0.059054035642) * w
                    -0.151968751364) * w +0.319152932694) * w
                  -0.531923007300) * w +0.797884560593) * y * 2.0
        else:
            y = y - 2.0
            x = (((((((((((((-0.000045255659 * y
                             +0.000152529290) * y -0.000019538132) * y
                           -0.000676904986) * y +0.001390604284) * y
                         -0.000794620820) * y -0.002034254874) * y
                       +0.006549791214) * y -0.010557625006) * y
                     +0.011630447319) * y -0.009279453341) * y
                   +0.005353579108) * y -0.002141268741) * y
                 +0.000535310849) * y +0.999936657524
    if _z > 0.0:
        prob = ((x+1.0)*0.5)
    else:
        prob = ((1.0-x)*0.5)
    return prob


def ksprob(alam:float)->float:
    """
Computes a Kolmolgorov-Smirnov t-test significance level.  Adapted from
Numerical Recipies.

Usage:   lksprob(alam)
"""
    fac = 2.0
    sum = 0.0
    termbf = 0.0
    a2 = -2.0*alam*alam
    for j in range(1,201):
        term = fac*exp(a2*j*j)
        sum = sum + term
        if fabs(term) <= (0.001*termbf) or fabs(term) < (1.0e-8*sum):
            return sum
        fac = -fac
        termbf = fabs(term)
    return 1.0             # Get here only if fails to converge; was 0.0!!


def fprob (dfnum:float, dfden:float, F:float)->float:
    """
Returns the (1-tailed) significance level (p-value) of an F
statistic given the degrees of freedom for the numerator (dfR-dfF) and
the degrees of freedom for the denominator (dfF).

Usage:   lfprob(dfnum, dfden, F)   where usually dfnum=dfbn, dfden=dfwn
"""
    p = betai(0.5*dfden, 0.5*dfnum, dfden/float(dfden+dfnum*F))
    return p


def betacf(a:float,b:float,x:float)->float:
    """
This function evaluates the continued fraction form of the incomplete
Beta function, betai.  (Adapted from: Numerical Recipies in C.)

Usage:   lbetacf(a,b,x)
"""
    ITMAX = 200
    EPS = 3.0e-7

    bm = az = am = 1.0
    qab = a+b
    qap = a+1.0
    qam = a-1.0
    bz = 1.0-qab*x/qap
    for i in range(ITMAX+1):
        em = float(i+1)
        tem = em + em
        d = em*(b-em)*x/((qam+tem)*(a+tem))
        ap = az + d*am
        bp = bz+d*bm
        d = -(a+em)*(qab+em)*x/((qap+tem)*(a+tem))
        app = ap+d*az
        bpp = bp+d*bz
        aold = az
        am = ap/bpp
        bm = bp/bpp
        az = app/bpp
        bz = 1.0
        if (abs(az-aold)<(EPS*abs(az))):
            return az
    print('a or b too big, or ITMAX too small in Betacf.')
    #mike addition
    raise Exception

def gammln(xx:float)->float:
    """
Returns the gamma function of xx.
    Gamma(z) = Integral(0,infinity) of t^(z-1)exp(-t) dt.
(Adapted from: Numerical Recipies in C.)

Usage:   lgammln(xx)
"""

    coeff = [76.18009173, -86.50532033, 24.01409822, -1.231739516,
             0.120858003e-2, -0.536382e-5]
    x = xx - 1.0
    tmp = x + 5.5
    tmp = tmp - (x+0.5)*log(tmp)
    ser = 1.0
    for j in range(len(coeff)):
        x = x + 1
        ser = ser + coeff[j]/x
    return -tmp + log(2.50662827465*ser)


def betai(a:float,b:float,x:float)->float:
    """
Returns the incomplete beta function:

    I-sub-x(a,b) = 1/B(a,b)*(Integral(0,x) of t^(a-1)(1-t)^(b-1) dt)

where a,b>0 and B(a,b) = G(a)*G(b)/(G(a+b)) where G(a) is the gamma
function of a.  The continued fraction formulation is implemented here,
using the betacf function.  (Adapted from: Numerical Recipies in C.)

Usage:   lbetai(a,b,x)
"""
    if (x<0.0 or x>1.0):
        raise ValueError('Bad x in lbetai')
    if (x==0.0 or x==1.0):
        bt = 0.0
    else:
        bt = exp(gammln(a+b)-gammln(a)-gammln(b)+a*log(x)+b*
                      log(1.0-x))
    if (x<(a+1.0)/(a+b+2.0)):
        return bt*betacf(a,b,x)/float(a)
    else:
        return 1.0-bt*betacf(b,a,1.0-x)/float(b)



