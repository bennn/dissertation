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

- synth : one configuration from the synth benchmark
- jpeg
- lnm


Results
---

synth
  - natural, 7.5.0.6++, 12689
  - transient, adbf3f, 1511


