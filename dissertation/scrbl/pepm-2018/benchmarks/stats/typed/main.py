from Timer import Timer
import anova
import central_tendency
import correlation
import frequency
import inferential
import moment
import probability
import pstat
import support
import trimming
import variability

##bg: use to test correctness
#def print_debug(*args):
#  if len(args) > 1:
#    a1 = args[1]
#    for a in args[2:]:
#      if not a1 == a:
#        raise ValueError("%s %s" % (a1, a))
#  return None

#bg: use for actual runs
def print_noop(*args):
  return None

print = print_noop
t = Timer()

LIST_SIZE = 500

with t:
    l = list(map(float,range(1,LIST_SIZE)))
    lf = list(map(float,range(1,LIST_SIZE)))
    lf[2] = 3.0
    ll = [l]*5

    print('\nCENTRAL TENDENCY')
    print('geometricmean:',central_tendency.geometricmean(l), central_tendency.geometricmean(lf), central_tendency.geometricmean(l), central_tendency.geometricmean(lf))
    print('harmonicmean:',central_tendency.harmonicmean(l), central_tendency.harmonicmean(lf), central_tendency.harmonicmean(l), central_tendency.harmonicmean(lf))
    print('mean:',central_tendency.mean(l), central_tendency.mean(lf), central_tendency.mean(l), central_tendency.mean(lf))
    print('median:',central_tendency.median(l),central_tendency.median(lf),central_tendency.median(l),central_tendency.median(lf))
    print('medianscore:',central_tendency.medianscore(l),central_tendency.medianscore(lf),central_tendency.medianscore(l),central_tendency.medianscore(lf))
    print('mode:',central_tendency.mode(l),central_tendency.mode(l))

    print('\nMOMENTS')
    print('moment:',moment.moment(l,2),moment.moment(lf,2),moment.moment(l,2),moment.moment(lf,2))
    print('variation:',moment.variation(l),moment.variation(l),moment.variation(lf),moment.variation(lf))
    print('skew:',moment.skew(l),moment.skew(lf),moment.skew(l),moment.skew(lf))
    print('kurtosis:',moment.kurtosis(l),moment.kurtosis(lf),moment.kurtosis(l),moment.kurtosis(lf))
    print('describe:')
    print(moment.describe(l))
    print(moment.describe(lf))

    print('\nFREQUENCY')
    print('freqtable:')
    print('itemfreq:')
    print(frequency.itemfreq(l))
    print(frequency.itemfreq(l))
    print('scoreatpercentile:',frequency.scoreatpercentile(l,40),frequency.scoreatpercentile(lf,40),frequency.scoreatpercentile(l,40),frequency.scoreatpercentile(lf,40))
    print('percentileofscore:',frequency.percentileofscore(l,12),frequency.percentileofscore(lf,12),frequency.percentileofscore(l,12),frequency.percentileofscore(lf,12))
    print('histogram:',frequency.histogram(l,10,[0,max(l)]),frequency.histogram(l,10,[0,max(l)]))
    print('cumfreq:')
    print(frequency.cumfreq(l))
    print(frequency.cumfreq(lf))
    print(frequency.cumfreq(l))
    print(frequency.cumfreq(lf))
    print('relfreq:')
    print(frequency.relfreq(l))
    print(frequency.relfreq(lf))
    print(frequency.relfreq(l))
    print(frequency.relfreq(lf))

    print('\nVARIATION')
    print('obrientransform:')

    l = [float(f) for f in list(range(1,LIST_SIZE))]
    ll = [l]*5

    print(variability.obrientransform([l,l,l,l,l]))

    print('samplevar:',variability.samplevar(l),variability.samplevar(l))
    print('samplestdev:',variability.samplestdev(l),variability.samplestdev(l))
    print('var:',variability.var(l),variability.var(l))
    print('stdev:',variability.stdev(l),variability.stdev(l))
    print('sterr:',variability.sterr(l),variability.sterr(l))
    print('sem:',variability.sem(l),variability.sem(l))
    print('z:',variability.z(l,4),variability.z(l,4))
    print('zs:')
    print(variability.zs(l))
    print(variability.zs(l))

    print('\nTRIMMING')
    print('trimboth:')
    print(trimming.trimboth(l,.2))
    print(trimming.trimboth(lf,.2))
    print(trimming.trimboth(l,.2))
    print(trimming.trimboth(lf,.2))
    print('trim1:')
    print(trimming.trim1(l,.2))
    print(trimming.trim1(lf,.2))
    print(trimming.trim1(l,.2))
    print(trimming.trim1(lf,.2))

    print('\nCORRELATION')
    #execfile('testpairedstats.py')

    l = [float(f) for f in list(range(1,LIST_SIZE))]
    ll = [l]*5

    m = [float(f) for f in list(range(4,LIST_SIZE+3))]
    #bg#m = dyn([float(f) for f in list(range(4,24))])
    m[10] = 34.

    pb = [0.]*(int(LIST_SIZE/2) - 1) + [1.]*int(LIST_SIZE/2)
    #bg#pb = dyn([0.]*9 + [1.]*11)

    print('pearsonr:')
    print(correlation.pearsonr(l,m))
    print(correlation.pearsonr(l,l))
    print('spearmanr:')
    print('pointbiserialr:')
    print(correlation.pointbiserialr(pb,l))
    print(correlation.pointbiserialr(pb,l))
    print('kendalltau:')
    print(correlation.kendalltau(l,m))
    print(correlation.kendalltau(l,l))
    print('linregress:')
    print(correlation.linregress(l,m))
    print(correlation.linregress(l,l))

    print('\nINFERENTIAL')
    print('ttest_1samp:')
    print(inferential.ttest_1samp(l,12))
    print(inferential.ttest_1samp(l,12))
    print('ttest_ind:')
    print(inferential.ttest_ind(l,m))
    print(inferential.ttest_ind(l,l))
    print('chisquare:')
    print(inferential.chisquare(l))
    print(inferential.chisquare(l))
    print('ks_2samp:')
    print(inferential.ks_2samp(l,m))
    print(inferential.ks_2samp(l,l))

    print('mannwhitneyu:')
    print(inferential.mannwhitneyu(l,m))
    print(inferential.mannwhitneyu(l,l))
    print('ranksums:')
    print(inferential.ranksums(l,m))
    print(inferential.ranksums(l,l))
    print('wilcoxont:')
    print(inferential.wilcoxont(l,m))
    print('kruskalwallish:')
    print(inferential.kruskalwallish([l,m,l]))
    print(len(l), len(m))
    print(inferential.kruskalwallish([l,l,l]))
    print('friedmanchisquare:')
    print(inferential.friedmanchisquare([l,m,l]))
    print(inferential.friedmanchisquare([l,l,l]))

    l = [float(x) for x in range(1,LIST_SIZE)]
    ll = [l]*5

    m = [float(x) for x in range(4,LIST_SIZE+3)]
    m[10] = 34. 

    print('\n\nF_oneway:')
    print(anova.F_oneway([l,m]))
    print(anova.F_oneway([l,l]))
    #print 'F_value:',stats.F_value(l),stats.F_value(l)

    print('\nSUPPORT')
    print('sum:',support.sum(l),support.sum(lf),support.sum(l),support.sum(lf))
    print('cumsum:')
    print(support.cumsum([int(x) for x in l]))
    print(support.cumsum([int(x) for x in lf]))
    print('ss:',support.ss(l),support.ss(lf),support.ss(l),support.ss(lf))
    print('summult:',support.summult(l,m),support.summult(lf,m),support.summult(l,l),support.summult(lf,l))
    print('sumsquared:',support.square_of_sums(l),support.square_of_sums(lf),support.square_of_sums(l),support.square_of_sums(lf))
    print('sumdiffsquared:',support.sumdiffsquared(l,m),support.sumdiffsquared(lf,m),support.sumdiffsquared(l,l),support.sumdiffsquared(lf,l))
    print('shellsort:')
    print(support.shellsort(m))
    print(support.shellsort(l))
    print('rankdata:')
    print(support.rankdata(m))
    print(support.rankdata(l))

