#lang racket

(provide (struct-out s)) 
(provide list-of-s) 
(provide set-list-of-s!) 

(struct s (a)) 
(define list-of-s '()) 
(define (set-list-of-s! los) 
(set! list-of-s los)) 
