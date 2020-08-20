#lang typed/racket #:transient

(define-type Store (Mutable-HashTable Integer Value)) 
(define-type Value (U Real Boolean String)) 

(define top-store (cast (make-hash (list 
(cons -1 14) 
(cons 1 #t) 
(cons 2 #f))) 
Store)) 

(hash-set! top-store 5 1234)

