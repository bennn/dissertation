#lang racket/base
(require
  math/statistics
  gtp-util)

;; fsm data

(define s-u (mean (map time-string->cpu-time '(
"cpu time: 436 real time: 437 gc time: 8"
"cpu time: 443 real time: 445 gc time: 8"
"cpu time: 456 real time: 458 gc time: 8"
"cpu time: 488 real time: 490 gc time: 8"
"cpu time: 453 real time: 455 gc time: 9"
))))

(define s-t (mean (map time-string->cpu-time '(
"cpu time: 133 real time: 134 gc time: 2"
"cpu time: 124 real time: 126 gc time: 2"
"cpu time: 136 real time: 139 gc time: 2"
"cpu time: 140 real time: 141 gc time: 3"
"cpu time: 139 real time: 139 gc time: 3"
))))

(define t-u (mean (map time-string->cpu-time '(
"cpu time: 523 real time: 562 gc time: 10"
"cpu time: 450 real time: 454 gc time: 8"
"cpu time: 434 real time: 436 gc time: 9"
"cpu time: 435 real time: 437 gc time: 8"
"cpu time: 429 real time: 431 gc time: 8"
))))

(define t-t (mean (map time-string->cpu-time '(
"cpu time: 135 real time: 139 gc time: 2"
"cpu time: 135 real time: 139 gc time: 2"
"cpu time: 143 real time: 146 gc time: 2"
"cpu time: 141 real time: 143 gc time: 2"
"cpu time: 136 real time: 137 gc time: 2"
))))

(printf "FSM deep/untyped = ~a~n shal/untyped = ~a~n" (rnd (/ t-t t-u)) (rnd (/ s-t s-u)))
;; FSM deep/untyped = 0.30 shal/untyped = 0.30

