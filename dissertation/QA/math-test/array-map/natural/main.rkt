#lang racket/base
(require "math/array.rkt")

(define (main)
  (let ((a (make-array #(600 600) 0)))
    (array-andmap equal? a a)
    (array-ormap equal? a a)
    (void)))

(time (main))
