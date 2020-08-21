#lang typed/racket #:transient

(require/typed racket/list 
[index-of (All (T) ((Listof T) T -> (U False Natural)))]) 

(index-of '(n s e w) 'n)
