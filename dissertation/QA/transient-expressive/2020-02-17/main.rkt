#lang racket 

(require "racket_mod.rkt") 
(require "racket_mod_typed.rkt") 

(define los (list (s 1) (s 2))) 
(set-list-of-s! los) 
(displayln list-of-s) 

(define los2 (list (s2 1) (s2 2))) 
(set-list-of-s2! los2) 
(displayln list-of-s2) 

