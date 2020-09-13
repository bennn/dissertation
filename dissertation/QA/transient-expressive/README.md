transient-expressive
===

Can transient express new programs?

Well of course. Any code that needed a chaperone at the boundary can be used now.

Is there any wild evidence that this is useful?


### github

Searched for `typed/racket/no-check`, found 900+ results, most are copies of
 the scribble file.

A few looked promising, but no there is nothing here:

- https://github.com/gcrowder/programming-language-classifier
  : Has data files from different PLs

- https://github.com/takikawa/tr-both
  : Experimental language, supposed to have: typed syntax, untyped behavior,
  and optional typed behavior. No-check helps the implementation, don't need
  transient.

- https://github.com/racket/gnats-bugs/blob/7e4bb9a65cd4783bef9936b576c5e06a5da3fb01/all/14821
  : gnats-bug about no-check behavior

- https://github.com/LeifAndersen/experimental-methods-in-pl/
  : goddam another fork of TR


### gitlab

Another search, `typed/racket/no-check`, 0 results.



### mailing list

#### case study, JSON

https://groups.google.com/g/racket-users/c/6KQxpfMLTn0/m/lil_6qSMDAAJ

can transient avoid the O(n)? well yes. but is it better after?


#### case study, TR stream

motivation
: https://groups.google.com/g/racket-users/c/1N6bXSQmmHQ/m/m23l2aOvAQAJ

work-around Deep package
: https://github.com/AlexKnauth/typed-racket-stream

can transient avoid the dance between macros and functions?


#### case study, msgpack

https://groups.google.com/g/racket-users/c/6KQxpfMLTn0/m/lil_6qSMDAAJ



#### 2020-08-18 -- 2017-11-28

150 messages so far,
6 winners

- https://groups.google.com/g/racket-users/c/IKTFoqwQ6yQ/m/vBGhck4TAgAJ
  X : static error, can't occurrence a set! var
- https://groups.google.com/g/racket-users/c/jtmVDFCGL28/m/jwl4hsjtBQAJ
  O : cannot protect opaque value, FIXED by transient
- https://groups.google.com/g/racket-users/c/2X5olKMV3C4/m/mJhsp9ZWBgAJ
  O : inference gets precise type, cast does not forget it, FIXED by transient b/c old type forgotten
- https://groups.google.com/g/racket-users/c/8xkpjpNntRo/m/mexP1a6OBgAJ
  X : need to require/typed vector-sort, suggestion (Vectorof Any) fails even with transient for the particular code
- https://groups.google.com/g/racket-users/c/UD20HadJ9Ec/m/Lmuw0U8mBwAJ
  O : set! has no apparent effect, b/c Deep puts a contract around and that makes a copy! transient = no copy
- https://groups.google.com/g/racket-users/c/JEEuTQc1YjE/m/dobsO63XBwAJ
  X : type cannot contract, contains free variables
- https://groups.google.com/g/racket-users/c/oiFYAxK48Yc/m/Y2mjC-m2AQAJ
  X : trouble expressing polymorphic class, static-only problem
- https://groups.google.com/g/racket-users/c/6lo-duvGX5E/m/c2RDJdKXAQAJ
  X : mutable data, confused about supertypes
- https://groups.google.com/g/racket-users/c/Y7bVyl8sBuc/m/cRu5bufzAAAJ
  X : for macros and immutable vectors
- https://groups.google.com/g/racket-users/c/o8uqVXGFIQ0/m/WDGYFp2NAwAJ
  X : require/typed #:struct, but the library does not export struct
- https://groups.google.com/g/racket-users/c/ZbYRQCy93dY/m/kF_Ek0VvAQAJ
  O : parametric contract changes result, transient avoids
- https://groups.google.com/g/racket-users/c/ZO4tNKOYv74/m/otfia-S7DQAJ
  X : type error, `apply` on function that requires 2 args
- https://groups.google.com/g/racket-users/c/i9jVuzfDGt4/m/Nhk71Z1WBwAJ
  X : syntax error in Racket v6.0 (vs 6.1 and later)
- https://groups.google.com/g/racket-users/c/8YS0vxj4ZBc/m/l1mbb3NnBwAJ
  X : help getting (Listof String) out of a JSExpr union, why O(n) cost
- https://groups.google.com/g/racket-users/c/plrpS2ZCWNA/m/trGDdbi-BAAJ
  X : low-level segfault, fixed for 7.4 release
- https://groups.google.com/g/racket-users/c/ozT9sVpfPZE/m/lXm9jkTuCQAJ
  X : about how to use annotations / inst
- https://groups.google.com/g/racket-users/c/BDrrgW0axGQ/m/P31NxeGHAAAJ
  O : require/typed case-> with 2 arity-1 cases, ok for transient
- https://groups.google.com/g/racket-users/c/0tOGWZ9O57c/m/jRXJYkUdAQAJ
  X : for loop, not enough types / unsupported
- https://groups.google.com/g/racket-users/c/79Cm-nyceXE/m/U78Eey0RDwAJ
  X : curry, needs type annotation
- https://groups.google.com/g/racket-users/c/TuHMHdZKhgI/m/x3jAwKtRDgAJ
  X : TR does not do type dispatch
- https://groups.google.com/g/racket-users/c/fSLxP8YW7Mw/m/FV8_UsKdAgAJ
  X : for/fold #:result expansion trouble
- https://groups.google.com/g/racket-users/c/mUOiv9zop70/m/5QT8Fo6pAQAJ
  X : 3 troubles with random, runs ok, usually TR faster than transient,
      orig program : transient < tr, ~30sec
      use positive? : transient > tr, ~30sec
      positive? and c-p-r-g once : transient > tr, ~10sec
      (wow)
- https://groups.google.com/g/racket-users/c/Ma9Fh72gfQg/m/F5v_kdvVBAAJ
  X : typecheck error, poly structs
- https://groups.google.com/g/racket-users/c/tMy9lma7W18/m/iJimGR_OBQAJ
  X : contract for prefab, fixed in later version
- https://groups.google.com/g/racket-users/c/8THiLChLlQg/m/UkwMzqLaCwAJ
  X : type-check succeeds, surprising, but nothing for transient to change
- https://groups.google.com/g/racket-users/c/7hL-zpOdaT0/m/OxQhnGTMBgAJ
  X : type-check surprises
- https://groups.google.com/g/racket-users/c/cCQ6dRNybDg/m/CKXgX1PyBgAJ
  O : any to proc, no "higher order value" problems anymore
- https://groups.google.com/g/racket-users/c/5QKSeAF9ddU/m/P74JrMZVCwAJ
  X : how to make a sequence, resolved on list
- https://groups.google.com/g/racket-users/c/TS-a1XA4_qc/m/5Vd6Ukd7EAAJ
  X : for loop problem, for*/or unsupported?
- https://groups.google.com/g/racket-users/c/Sl7_eoHZFeI/m/KN0WhoVLDAAJ
  X : type check, cannot polymorphic struct at boundary


