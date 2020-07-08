#lang at-exp racket/base

(provide
  MAIN-BENCHMARKS
  NUM-MAIN-BENCHMARKS
  RT
  MAX-OVERHEAD

  render-exact-runtime-plot*
  exact-runtime-category
)

(require
;  "script/config.rkt"
;  "script/benchmark-info.rkt"
;  (prefix-in pi: "script/performance-info.rkt")
;  "script/render.rkt"
;  "script/util.rkt"
;  (only-in "script/plot.rkt"
;    *CONFIGURATION-X-JITTER*
;    *OVERHEAD-MAX*)
  (only-in racket/class
    class new super-new object% define/public)
  (only-in racket/list
    add-between
    partition)
  racket/format
  racket/string
  pict
;  scribble/acmart
;  scribble/core
;  scribble/example
;  scribble/html-properties
;  scribble/latex-properties
;  scriblib/autobib
;  scriblib/figure
;  setup/main-collects
  (for-syntax racket/base syntax/parse))

;; -----------------------------------------------------------------------------

(define EXHAUSTIVE-BENCHMARKS '())

@(define MAIN-BENCHMARKS (append EXHAUSTIVE-BENCHMARKS SAMPLE-BENCHMARKS))
@(define NUM-MAIN-BENCHMARKS (length MAIN-BENCHMARKS))
@(define RT (get-ratios-table MAIN-BENCHMARKS))

(define MAX-OVERHEAD 10)

(define (percent-slower-than-typed pre-bm)
  (define pi (pi:benchmark->performance-info (->benchmark pre-bm)))
  (define total (pi:num-configurations pi))
  (define num-good ((pi:deliverable (pi:typed/python-ratio pi)) pi))
  (round (pct (- total num-good) total)))

(define (render-exact-runtime-plot* bm-name*)
  (blank))

(define (exact-runtime-category TODO)
  TODO)

(define (bm-desc TODO)
  TODO)



