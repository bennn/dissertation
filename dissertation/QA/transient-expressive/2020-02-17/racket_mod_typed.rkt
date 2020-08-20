#lang typed/racket #:transient

(provide (struct-out s2)) 
(provide list-of-s2) 
(provide set-list-of-s2!) 

(struct s2 ([a : Natural])) 
(define list-of-s2 '()) 
(define (set-list-of-s2! [los : (Listof s2)]) 
(set! list-of-s2 los)) 
