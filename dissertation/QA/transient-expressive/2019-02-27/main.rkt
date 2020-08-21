#lang typed/racket  #:transient


(define-predicate nneg-flonum? Nonnegative-Flonum) 
;; comment it out for no-check... 
; (define (nneg-flonum? n) #t) 

(define xxx (current-pseudo-random-generator))

(define (run-trials [trials : Natural]) 
(exact->inexact 
(/ (for/sum : Natural 
([i (in-range trials)]) 
(define a (assert (random xxx) positive?)) 
(define b (assert (random xxx) positive?)) 
(define c (assert (random xxx) positive?)) 
(if (and (< (abs (- a b)) c) 
(< c (sqrt (+ (* a a) (* b b))))) 
1 
0)) 
trials))) 


(time 
(for ([i : Natural (in-range 26)]) 
(define trials (expt 2 i)) 
(printf "~v trials: ~v\n" 
trials (run-trials trials)))) 
