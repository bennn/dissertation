#lang typed/racket/base

;; a representation for game cards

(provide
 (struct-out card)

 ;; Card Card -> Boolean
 >-face

 ;; Card Card -> Face
 ;; assume for (--face c d) assume (>-face c d)
 --face)

(require "basics-types.rkt")

;; ---------------------------------------------------------------------------------------------------

;; -- card.rkt
(struct card (
 [face : Face]
 [bulls : Bulls])
#:transparent)
(define-type Card card)

(: >-face (-> Card Card Boolean))
(define (>-face c d)
  (> (card-face c) (card-face d)))

(: face? (-> Any Boolean : #:+ Face))
(define (face? f)
  (and (exact-nonnegative-integer? f) (< 0 f) (< f 105)))

(: --face (-> Card Card Face))
(define (--face c d)
  (assert (- (card-face c) (card-face d)) face?))
