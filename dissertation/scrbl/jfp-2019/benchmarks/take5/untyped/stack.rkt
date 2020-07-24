#lang racket/base

;; a representation of the visible stacks

(provide

 ;; Card -> Stack
 create-stack

 ;; Stack -> Card
 top

 ;; Card Stack -> Stack
 push

 ;; Stack -> N
 length

 ;; Stack -> N
 ;; sum up the bulls shown on the cards of the stack
 bulls)

(require
  "../base/untyped.rkt"
  "card.rkt")
(require (prefix-in list: (only-in racket/base length)))
(require (only-in racket/list first))

;; ---------------------------------------------------------------------------------------------------

(define (create-stack c) (list c))
(define top first)
(define (push c s)
  (cons c s))
(define length list:length)
(define (bulls s) (foldr + 0 (map card-bulls s)))

