#lang typed/racket
(random-seed 7480)

;; Run a Simulation of Interacting Automata
;; Run a Simulation of Interacting Automata

;; =============================================================================
(require
  "automata-adapted.rkt"
  "population-adapted.rkt"
  require-typed-check
)
(require/typed/check "utilities.rkt"
 (relative-average (-> [Listof Real] Real Real))
)

(: payoff? (-> Any Boolean : #:+ Payoff))
(define (payoff? x)
  (and (real? x) (<= 0 x)))

;; effect: run timed simulation, create and display plot of average payoffs
;; effect: measure time needed for the simulation
(define (main)
   (simulation->lines
    (evolve (build-random-population 300) 500 100 20))
   (void))

(: simulation->lines (-> [Listof Payoff] (Listof (List Integer Real))))
;; turn average payoffs into a list of Cartesian points 
(define (simulation->lines data)
    (for/list : [Listof [List Integer Real]]
      ([d : Payoff (in-list data)][n : Integer (in-naturals)])
      (list n d)))

(: evolve (-> oPopulation Natural Natural Natural [Listof Payoff]))
;; computes the list of average payoffs over the evolution of population p for
;; c cycles of of match-ups with r rounds per match and at birth/death rate of s
(define (evolve p c s r)
  (let evolve ([c : Natural c] [s : Natural s] [r : Natural r])
  (cond
    [(zero? c) '()]
    [else (send p match-up* r)
          ;; Note: r is typed as State even though State is not exported 
          (define pp (send p payoffs))
          (send p death-birth s)
          ;; Note: s same as r
          ({inst cons Payoff [Listof Payoff]}
           (assert (relative-average pp r) payoff?)
           ;; Note: evolve is assigned (-> ... [Listof Probability])
           ;; even though it is explicitly typed ... [Listof Payoff]
           (evolve (- c 1) s r))])))

(time (main))
