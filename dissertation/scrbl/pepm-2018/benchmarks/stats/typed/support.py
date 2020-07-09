import pstat
import copy
from typed_math import pow, sqrt, exp, abs, fabs, log, round, pi


####################################
########  SUPPORT FUNCTIONS  #######
####################################


def sum (inlist:List(float))->float:
    """
Returns the sum of the items in the passed list.

Usage:   lsum(inlist)
"""
    s = 0
    for item in inlist:
        s = s + item
    return s


def cumsum (inlist:List(int))->List(int):
    """
Returns a list consisting of the cumulative sum of the items in the
passed list.

Usage:   lcumsum(inlist)
"""
    newlist = copy.deepcopy(inlist)
    for i in range(1,len(newlist)):
        newlist[i] = newlist[i] + newlist[i-1]
    return newlist


def ss(inlist:List(float))->float:
    """
Squares each value in the passed list, adds up these squares and
returns the result.

Usage:   lss(inlist)
"""
    _ss = 0
    for item in inlist:
        _ss = _ss + item*item
    return _ss


def summult (list1:List(float),list2:List(float))->float:
    """
Multiplies elements in list1 and list2, element by element, and
returns the sum of all resulting multiplications.  Must provide equal
length lists.

Usage:   lsummult(list1,list2)
"""
    if len(list1) != len(list2):
        raise ValueError("Lists not equal length in summult.")
    s = 0
    for item1,item2 in pstat.abut(list1,list2):
        s = s + item1*item2
    return s


def sumdiffsquared(x:List(float),y:List(float))->float:
    """
Takes pairwise differences of the values in lists x and y, squares
these differences, and returns the sum of these squares.

Usage:   lsumdiffsquared(x,y)
Returns: sum[(x[i]-y[i])**2]
"""
    sds = 0
    for i in range(len(x)):
        sds = sds + (x[i]-y[i])**2
    return sds


def square_of_sums(inlist:List(float))->float:
    """
Adds the values in the passed list, squares the sum, and returns
the result.

Usage:   lsquare_of_sums(inlist)
Returns: sum(inlist[i])**2
"""
    s = sum(inlist)
    return float(s)*s


def shellsort(inlist:List(float))->(List(float),List(int)):
    """
Shellsort algorithm.  Sorts a 1D-list.

Usage:   lshellsort(inlist)
Returns: sorted-inlist, sorting-index-vector (for original list)
"""
    n = len(inlist)
    svec = copy.deepcopy(inlist)
    ivec = list(range(n))
    gap = n//2   # integer division needed
    while gap >0:
        for i in range(gap,n):
            for j in range(i-gap,-1,-gap):
                while j>=0 and svec[j]>svec[j+gap]:
                    temp        = svec[j]
                    svec[j]     = svec[j+gap]
                    svec[j+gap] = temp
                    itemp       = ivec[j]
                    ivec[j]     = ivec[j+gap]
                    ivec[j+gap] = itemp
        gap = gap // 2  # integer division needed
# svec is now sorted inlist, and ivec has the order svec[i] = vec[ivec[i]]
    return svec, ivec


def rankdata(inlist:List(float))->List(float):
    """
Ranks the data in inlist, dealing with ties appropritely.  Assumes
a 1D inlist.  Adapted from Gary Perlman's |Stat ranksort.

Usage:   lrankdata(inlist)
Returns: a list of length equal to inlist, containing rank scores
"""
    n = len(inlist)
    svec, ivec = shellsort(inlist)
    sumranks = 0
    dupcount = 0
    newlist = [0.]*n
    for i in range(n):
        sumranks = sumranks + i
        dupcount = dupcount + 1
        if i==n-1 or svec[i] != svec[i+1]:
            averank = sumranks / float(dupcount) + 1
            for j in range(i-dupcount+1,i+1):
                newlist[ivec[j]] = averank
            sumranks = 0
            dupcount = 0
    return newlist

