#lang racket/base

(provide
  (all-from-out
    scribble-abbrevs
    classicthesis/lang
    gtp-plot/configuration-info
    gtp-plot/typed-racket-info
    gtp-plot/performance-info
    gtp-plot/plot
    gtp-util
    greenman-thesis/gtp-benchmarks
    racket/format
    scriblib/figure
    scribble/example)

  ~cite

  citet

  jointwork
  summary
  bm

)

(require
  (only-in racket/list
    add-between
    partition)
  racket/format
  classicthesis/lang
  racket/format
  racket/string
  gtp-plot/configuration-info
  gtp-plot/typed-racket-info
  gtp-plot/performance-info
  gtp-plot/plot
  gtp-util
  greenman-thesis/gtp-benchmarks
  scribble/example
  scribble-abbrevs
  scriblib/figure
  setup/main-collects
  (for-syntax racket/base syntax/parse))

;; =============================================================================

(define (~cite . txt*)
  (exact (list "~\\citep{" txt* "}")))

(define (citet . txt*)
  (exact (list "\\citet{" txt* "}")))

(define (jointwork #:people* people* #:paper* [paper* '()])
  (nested-inset
    (emph (list
            "This chapter is based on joint work with: "
            (oxfordize people*)
            (~cite (string-join paper* ","))))))

(define (summary . txt*)
  (nested-inset txt*))

(define (nested-inset . content)
  (nested #:style 'inset content))

(define bm tt)
