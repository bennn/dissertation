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

- synth : (yes) one configuration from the synth benchmark
- jpeg : (yes) untyped configurations from benchmark, uses part of math library
- lnm : (no) only uses 'mean' from math/statistics not worth it
- funkytown : (no) Vincent's original synth code ... difficult b/c need a benchmark input ... may be good to just update the math codes for synth
- determinance : (no) does not use math library
- array-map : micro-benchmark using array-andmap / ormap
- stamps : 

Surely there must be others!



Results
---

TODO adapt math library code to use "locally defensive", can't currently
 at least because of `require/untyped-contract`

TODO use one copy of math!!! no sense doubling array & then matrix

Natural = racket/typed racket master, 8e88408 / e90a2b9e
Transient = racket master / TR adbf3f

| benchmark | natural | transient
|---
|     synth |   12689 |      1511
|      jpeg |     778 |       227
| array-map |    2877 |        27
| s:anemone |  195700 |      9170
|   s:penta |    3720 |      1440
|  s:branch |    9470 |      4840


