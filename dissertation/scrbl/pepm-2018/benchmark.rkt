#lang at-exp racket/base

(provide
  MAIN-BENCHMARKS
  NUM-MAIN-BENCHMARKS
  RT
  MAX-OVERHEAD
)

;; -----------------------------------------------------------------------------

@(define MAIN-BENCHMARKS (append EXHAUSTIVE-BENCHMARKS SAMPLE-BENCHMARKS))
@(define NUM-MAIN-BENCHMARKS (length MAIN-BENCHMARKS))
@(define RT (get-ratios-table MAIN-BENCHMARKS))

(define MAX-OVERHEAD 10)

