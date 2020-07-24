#lang racket/base

(provide assert)

(define (assert v p)
  (if (p v) v (raise-user-error 'assert "~a" (object-name p))))
