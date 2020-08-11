#lang racket/base
(require
  math/statistics
  gtp-util)

;; fsmoo data from laptop,
;; the paper should agree with this! Nearly the same ratios

(define s-u (mean (map time-string->cpu-time '(
"cpu time: 815 real time: 819 gc time: 82"
"cpu time: 810 real time: 815 gc time: 72"
"cpu time: 778 real time: 781 gc time: 78"
"cpu time: 817 real time: 831 gc time: 76"
"cpu time: 819 real time: 823 gc time: 76"
))))

(define s-t (mean '(
527
506
505
520
520)))

(define t-u (mean (map time-string->cpu-time '(
"cpu time: 873 real time: 888 gc time: 79"
"cpu time: 885 real time: 904 gc time: 88"
"cpu time: 865 real time: 902 gc time: 79"
"cpu time: 839 real time: 844 gc time: 74"
"cpu time: 794 real time: 798 gc time: 74"
))))

(define t-t (mean (map time-string->cpu-time '(
"cpu time: 499 real time: 502 gc time: 26"
"cpu time: 512 real time: 515 gc time: 29"
"cpu time: 511 real time: 514 gc time: 27"
"cpu time: 503 real time: 506 gc time: 28"
"cpu time: 494 real time: 496 gc time: 27"
))))

(printf "deep/untyped = ~a~n shal/untyped = ~a~n" (rnd (/ t-t t-u)) (rnd (/ s-t s-u)))

