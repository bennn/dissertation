QA
===

Code and data to answer questions from the thesis committee

- - -

#### math-array

Q. Can we NOW find programs that depend on the math library, and see if
   using transient will help?
A. math-array/README.md


#### baseline, no-opt

Q. Can you measure relative to Racket without type-based optimizations?

- RacketCS has a type-opt pass
  https://github.com/racket/ChezScheme/commit/30033287ad504058bc6810b1ef4ff5ecbe927dac
- and `(optimize-level 3)` in Chez
- and other optimization controls in Chez:
  https://cisco.github.io/ChezScheme/csug9.4/system.html#./system:s111

So, try some experiments to see whether these make a difference for Racket / TR
 and whether we need two baselines (either way, put what we learned in the
 dissertation)
