Hello,

I'm trying to get something similar to the following code working:

#lang typed/racket

(: f (-> Integer Any))
(define (f x) (+ 1 x))

(: g (All (a) (-> Integer a)))
(define (g x) (f x))

Of course, this gets me a type error: 

;Type Checker: type mismatch
;   expected: a
;   given: Any
;   in: (f x)

Now, I try to throw a cast into g:

(: g (All (a) (-> Integer a)))
(define (g x) (cast (f x) a))

This gives the following type error:

;Type Checker: Type a could not be converted to a contract because it contains free variables.
;   in: a

Does this mean that I can never cast to types containing type variables?

My original problem comes from playing around with eval, which returns AnyValues.  Ideally, I would like to be able to retrieve the first returned value (done), and then convert it to a type variable bound by one of the arguments:

(: my-super-func (All (a) (-> (Type1 a) a)))
(define (my-super-func arg)
  ...
  (get-first-value (eval some-stuff))) ; how to cast to a ?

-
Sergiu
