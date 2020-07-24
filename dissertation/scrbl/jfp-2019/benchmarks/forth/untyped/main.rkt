#lang racket/base

(require (only-in "eval.rkt"
  forth-eval*
))
(require (only-in racket/file file->lines))

;; =============================================================================

(define LOOPS 10)

(define (main lines)
  (for ((i (in-range LOOPS)))
    (define-values [_e _s] (forth-eval* lines))
    (void)))

(define lines (file->lines "../base/history-100.txt"))

(time (main lines))
