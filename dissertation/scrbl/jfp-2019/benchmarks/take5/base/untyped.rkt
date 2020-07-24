#lang racket/base

(provide
  bulls?
  assert
  index?
)

;; -----------------------------------------------------------------------------

(require (only-in racket/unsafe/ops unsafe-fx>=))

(define (index? x) (and (fixnum? x) (unsafe-fx>= x 0) (fixnum? (* x 4))))

(define (assert v p?)
  (if (p? v) v (raise-user-error 'assert "(~a ~a)" p? v)))

(define (bulls? b)
  (and (exact-nonnegative-integer? b) (< 1 b) (< b 8)))
