#lang racket/base

(require ppict/2)

(provide ppict)

(define (ppict pp cp*)
  (for/fold ((acc pp))
            ((cp (in-list cp*)))
    (ppict-do acc #:go (car cp) (cdr cp))))
