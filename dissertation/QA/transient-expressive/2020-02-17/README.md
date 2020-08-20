

Hello everybody, 
I'm trying to gradually type my script to make it a proper app (yes 
I'm a static-ish guy) and I have an issue (Racket 7.6 CS). 

=================================================== 
racket_mod.rkt: 
#lang racket 

(provide (struct-out s)) 
(provide list-of-s) 
(provide set-list-of-s!) 

(struct s (a)) 
(define list-of-s '()) 
(define (set-list-of-s! los) 
(set! list-of-s los)) 
=================================================== 

racket_mod_typed.rkt: 
#lang typed/racket 

(provide (struct-out s2)) 
(provide list-of-s2) 
(provide set-list-of-s2!) 

(struct s2 ([a : Natural])) 
(define list-of-s2 '()) 
(define (set-list-of-s2! [los : (Listof s2)]) 
(set! list-of-s2 los)) 
=================================================== 
racket_main.rkt: 
#lang racket 

(require "racket_mod.rkt") 
(require "racket_mod_typed.rkt") 

(define los (list (s 1) (s 2))) 
(set-list-of-s! los) 
(displayln list-of-s) 

(define los2 (list (s2 1) (s2 2))) 
(set-list-of-s2! los2) 
(displayln list-of-s2) 
=================================================== 

list-of-s2 is empty and list-of-s is not, the only difference seems to 
be the type annotations. 
Can someone help me ? :) 

Cheers and thanks, 
Bertrand 
