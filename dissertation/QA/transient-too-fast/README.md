2020-08-11
transient is too fast in some ratios, here is the incredible table in the
 dissertation today


```
  \hbox{Benchmark} & \hbox{deep/untyped} & \hbox{shallow/untyped} \\
  sieve & \hbox{0.97} & \hbox{1.00} \\
  forth & \hbox{0.65} & \hbox{1.59} \\
  fsm & \hbox{0.54} & \hbox{0.06} \\
  fsmoo & \hbox{0.88} & \hbox{0.15} \\
  mbta & \hbox{1.63} & \hbox{1.01} \\
  morsecode & \hbox{0.73} & \hbox{0.98} \\
  zombie & \hbox{1.79} & \hbox{0.82} \\
  dungeon & \hbox{0.99} & \hbox{1.13} \\
  jpeg & \hbox{0.40} & \hbox{1.03} \\
  zordoz & \hbox{1.35} & \hbox{1.36} \\
  lnm & \hbox{0.64} & \hbox{1.00} \\
  suffixtree & \hbox{0.69} & \hbox{0.61} \\
  kcfa & \hbox{1.04} & \hbox{1.02} \\
  snake & \hbox{0.96} & \hbox{1.08} \\
  take5 & \hbox{0.97} & \hbox{1.29} \\
  acquire & \hbox{1.22} & \hbox{0.95} \\
  tetris & \hbox{0.97} & \hbox{1.01} \\
  synth & \hbox{0.96} & \hbox{1.20}
```

Transient should NEVER be faster than fully-typed. Never.

The Deep data is based on BC, the Shallow on CS, so that's a problem, but
 these ratios are crazy.

- fsmoo
  should be ~ deep/untyped = 0.59 shal/untyped = 0.64
  deep is a little slow, shallow TOO FAST
  current shallow-untyped ~ 2000ms (!!!!) shallow-typed ~ 304ms (ok)

- fsm 



