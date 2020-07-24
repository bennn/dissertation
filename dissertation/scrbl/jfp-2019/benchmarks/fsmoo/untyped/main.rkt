#lang racket
(random-seed 7480)

;; Run a Simulation of Interacting Automata

;; =============================================================================
(require "../base/untyped.rkt" "population.rkt" "utilities.rkt")

(define (payoff? x)
  (and (real? x) (<= 0 x)))

;; -> Void
;; effect: run timed simulation, create and display plot of average payoffs
;; effect: measure time needed for the simulation
(define (main)
   (simulation->lines
    (evolve (build-random-population 300) 500 100 20))
   (void))

;; [Listof Payoff] -> [Listof [List Real Real]]
;; turn average payoffs into a list of Cartesian points 
(define (simulation->lines data)
  (for/list ([d (in-list data)][n (in-naturals)]) (list n d)))

;; Population N N N -> [Listof Payoff]
;; computes the list of average payoffs over the evolution of population p for
;; c cycles of of match-ups with r rounds per match and at birth/death rate of s
(define (evolve p c s r)
  (let evolve ([c c] [s s] [r r])
  (cond
    [(zero? c) '()]
    [else (send p match-up* r)
          (define pp (send p payoffs))
          (send p death-birth s)
          (cons
            (assert (relative-average pp r) payoff?)
            (evolve (- c 1) s r))])))

(time (main))
