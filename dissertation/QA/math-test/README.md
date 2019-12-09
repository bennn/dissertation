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
- determinance : 
- stamps


Results
---

Natural = racket/typed racket master, 8e88408 / e90a2b9e
Transient = racket master / TR adbf3f

| benchmark | natural | transient
|     synth |   12689 | 1511
|      jpeg |     778 | 227




