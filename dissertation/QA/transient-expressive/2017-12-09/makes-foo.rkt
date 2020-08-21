#lang typed/racket  #:transient

(provide (struct-out Foo) 
get-a-foo) 

(struct Foo ([a : String]) #:prefab) 

(define (get-a-foo) 
(Foo "oeuth")) 
