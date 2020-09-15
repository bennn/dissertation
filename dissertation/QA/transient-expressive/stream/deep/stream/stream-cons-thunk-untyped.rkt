#lang racket/base

(provide stream-cons/thunk empty-stream)

(require racket/stream)

(define (stream-cons/thunk a b)
  (stream-cons (a) (b)))

