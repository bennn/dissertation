#lang racket/base

;; a representation for game cards

(provide
 (struct-out card)

 ;; Card Card -> Boolean
 >-face

 ;; Card Card -> Face
 ;; assume for (--face c d) assume (>-face c d)
 --face)

(require
  "../base/untyped.rkt"
)

;; ---------------------------------------------------------------------------------------------------

;; -- card.rkt
(struct card (
 face
 bulls)
#:transparent)

(define (>-face c d)
  (> (card-face c) (card-face d)))

(define (face? f)
  (and (exact-nonnegative-integer? f) (< 0 f) (< f 105)))

(define (--face c d)
  (assert (- (card-face c) (card-face d)) face?))
