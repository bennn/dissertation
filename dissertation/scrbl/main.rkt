#lang at-exp racket/base

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
    racket/list
    scriblib/figure
    scribble/example)

  ~cite

  citet

  jointwork
  summary
  bm
  id

  definition
  sraapproximation
  ddeliverable
  kstep

)

(require
  (only-in racket/list
    add-between
    partition
    take)
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
  (only-in scribble/core
    make-element
    make-paragraph
    plain
    element)
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

(define id ~a)

(define (definition term . defn*)
  (make-paragraph plain
    (list
      (exact "\\vspace{1ex}\n")
      (bold "Definition")
      (cons (element #f (list " (" (emph term) ") ")) defn*)
      (exact "\\vspace{1ex}\n"))))

(define (ddeliverable [D "D"])
  (define d-str
    (cond
     [(string? D)
      D]
     [(and (real? D) (positive? D))
      (number->string D)]
     [else
      (raise-argument-error 'ddeliverable "(or/c positive-real? string?)" D)]))
  (elem ($ d-str) "-deliverable"))

(define (kstep [k "k"] [d "D"])
  (make-element plain @list{@${k}-step @${d}-deliverable}))

(define (sraapproximation r s [pct #f])
  (define pct-elem
    (if pct
      (elem ($ (~a pct)) "%-")
      (elem)))
  (define r-elem
    (if (real? r) (~a r) r))
  (define s-elem
    (if (real? s) (~a s) s))
  (elem pct-elem ($ r-elem ", " s-elem) "-approximation"))

