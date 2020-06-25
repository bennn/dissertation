#lang racket/base

(provide
  (all-from-out
    scribble-abbrevs
    classicthesis/lang
    gtp-util
    greenman-thesis/gtp-benchmarks
    scriblib/figure
    scribble/example)

  ~cite

  citet

  jointwork
  summary

)

(require
  (only-in racket/list
    add-between
    partition)
  classicthesis/lang
  racket/format
  racket/string
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
