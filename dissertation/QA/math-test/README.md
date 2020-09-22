math-test
===

Hypothesis:
 adding Transient should speed up programs that use the math library

H1. Convert lib to Transient, untyped client faster
H2. Convert lib to Transient, typed client a LITTLE slower (not a lot)

2019-12-09 : cannot test H2 because prototype cannot safely import Transient
 to typed, so do that later


Programs
---

First thoughts ...

- synth : (yes) one configuration from the synth benchmark
- jpeg : (yes) untyped configurations from benchmark, uses part of math library
- lnm : (no) only uses 'mean' from math/statistics not worth it
- funkytown : (no) Vincent's original synth code ... difficult b/c need a benchmark input ... may be good to just update the math codes for synth
- determinance : (no) does not use math library
- array-map : micro-benchmark using array-andmap / ormap
- stamps : 

... I found other programs later, they're in the results

All Math
---

2019-12-17 : here's a list of all packages that depend on a "math-*" package,
 found by `raco pkg catalog-copy --from-config DIR` and this script

```
    #lang racket/base
    (require file/glob racket/file racket/path racket/format)
    (for ((fn (in-glob "pkg/*")))
      (define h (file->value fn))
      (define name (hash-ref h 'name (or (file-name-from-path fn) (path->string fn))))
      (when (or (for/or ((d (in-list (hash-ref h 'dependencies '())))) (regexp-match? #rx"math-" (~a d)))
                (regexp-match? #rx"math-" (~a (hash-ref h 'build-failure-log #f))))
        (displayln name)))
```

77 total,
9 possible winners,


```
data-frame : MAYBE https://github.com/alex-hhh/data-frame https://github.com/alex-hhh/ActivityLog2

drbayes : maybe/no, distributions ... cannot find slow test
j : maybe/no, array, now get a benchmark NOT enough tests
rilouworld : yes matrix-transpose https://github.com/euhmeuh/rilouworld/tree/master
ruckus : maybe/no, matrix, lang/interpolation https://github.com/cbiffle/ruckus/tree/master
lindenmayer : maybe/no, matrix in 3d file ... nogood for 3d-with-leaves
metapict : maybe, try curve NOPE a wash
rsound : maybe/no, has fft and draw tests ... these are mini tests https://github.com/jbclements/RSound/tree/master
wavelet-transform-haar-1d : yes
```

```
Quaternion : no, math/flonum
alexknauth-music : no, math/number-theory
aoc-racket : no, unused
bazaar : no, personal utils, math/statistics
beautiful-racket-demo : no, example, math/number-theory
benchmark : no, statistics
binaryio : no, math/flonum
continued-fractions : no, math/number-theory
contract-profile : no, math/statistics
csp : no, number-theory
data-doc : no, doc
data-enumerate-lib : no, distributions number-theory
data-test : no, base number-theory statistics
derp-3 : no, statistics
ecmascript : no, flonum
fpbench : flonum
games : no, base
generic-bind : no, number-theory
glm : no, flonum
glossolalia : unlikely, distributions .... makes and uses a triangle-dist
gm-pepm-2018 : no, statistics
graph-doc : no
graph-lib : no
graphics : no, unused
gtp-checkup : no, already checked
gtp-pict : no, number-theory
gtp-plot : no, statistics
gtp-util : no, statistics
haiku-enum : no, base
herbie : no, flonum bigfloat
interactive-syntax : no thanks, don't think we can run editor examples
logo : no, looks unused
lti-freq-domain-toolbox : no, base, but looks interesting
main-distribution-test : no, math-test
map-widget : no, base flonum
math : no
math-doc : no
math-lib : no
math-test : no
measures-with-dimensions : no, bigfloat
nlopt : no, flonum
pict3d : no, bigfloat distributions
pict3d-orig : no, deprecated
pidec : no, number-theory
plot-bestfit : no, flonum
plot-gui-lib : no, flonum base distributions statistics
plot-lib : no, ditto
pmap : no, only namespace-require for parallel workers
pydrnlp : no, statistics
racket-doc : no
racket-lang-org : no
rascas : no, number-theory
redex-benchmark : no
redex-examples : no
redex-lib : no
require-typed-check : no, unused
rml-core : no, statistics
rml-decisiontrees : no, statistics
rml-knn : no, unused
rml-neural : no, flonum
softposit-herbie : no, flonum
softposit-rkt : no, bigfloat
t-test : no, special-functions statistics
trivial : no, unused
typed-racket-test : no
tzgeolookup : no, flonum
unstable-flonum-doc : no, flonum, only docs anyway
zeromq : no, base
```


Results
---

TODO adapt math library code to use "locally defensive", can't currently
 at least because of `require/untyped-contract`

TODO use one copy of math!!! no sense doubling array & then matrix

Natural = racket/typed racket master, 8e88408 / e90a2b9e
Transient = racket master / TR adbf3f

| benchmark | natural | transient |   X |
|---
|     synth |   12689 |      1511 |  10 |
|      jpeg |     778 |       227 |   2 |
| array-map |    2877 |        27 | 100 |
| s:anemone |  195700 |      9170 | 100 |
|   s:penta |    3720 |      1440 |   2 |
|  s:branch |    9470 |      4840 |   2 |
| rilou-xor |  152000 |      5370 |  30 |
| 7 wavelet }   40000 |      3000 |  10 |
|  aoc-2015 | timeout |     10489 |     |

| l:3d-with }    3170 |      3000 |   0 |
| 5 wavelet }    3000 |      2250 |   0 |
|  ruckus-i |      17 |        14 |   0 |
|  ruckus-h |   13158 |     13500 |   0 |
|  metapict |     176 |       157 |   0 |

synth
deep lib, deep client = 809 821 834 771 733
deep lib, untyped client = 11440 11040 11004 11923 11672
shallow lib, untyped client = 1645 1664 1558 1576 1539
shallow lib, deep client = 7823 7885 9002 7955 8279

jpeg
deep lib, untyped client = 463 470
shallow lib, untyped client = 99 97
shallow lib, deep client = 431 421

