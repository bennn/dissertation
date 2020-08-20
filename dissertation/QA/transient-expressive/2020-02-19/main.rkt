#lang typed/racket #:transient

(: f (-> Integer Any))
(define (f x) (+ 1 x))

(: g (All (a) (-> Integer a)))
(define (g x) (cast (f x) a))

