#lang racket/base

;; a representation of the complete deck of cards 

(provide
 ;;   { [Listof X] -> [Listof X] }
 ;;   { -> Bulls }
 ;; -> CardPool
 ;; create and organize the pool of cards in a random order so that
 ;; the dealer can hand cards to players and create the initial deck
 ;;
 ;; so we can keep things deterministic 
 ;; the first optional argument is a shuffle algorithm for lists
 ;; the second optional argument generates bulls 
 create-card-pool)

;; -----------------------------------------------------------------------------

(require
  racket/class
  "../base/untyped.rkt"
  (only-in racket/list shuffle first rest))
(require "card.rkt")
(require (only-in "basics.rkt"
  FACE
  HAND
  MIN-BULL
  MAX-BULL
))

;; ---------------------------------------------------------------------------------------------------
(define (create-card-pool (shuffle shuffle) (random-bulls random-bulls))
  (new card-pool% (shuffle shuffle) (random-bulls random-bulls)))

(define rng (vector->pseudo-random-generator '#(12 34 56 78 90 01)))

;; -> Bulls
;; pick a random number of BULLS 
(define (random-bulls)
  (random MIN-BULL (+ MAX-BULL 1) rng))

(define card-pool%
  (class object%
    (init-field (shuffle shuffle) (random-bulls random-bulls))
    (super-new)

    (define my-cards
      (shuffle (build-list FACE (lambda (i) (card (+ i 1) (random-bulls))))))

    (define/public (draw-card)
      (begin0 (first my-cards)
              (set! my-cards (rest my-cards))))

    (define/public (draw-hand)
      (build-list HAND (lambda (_) (draw-card))))))
