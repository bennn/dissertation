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

#### 2020-08-18 -- 2020-01-01

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

