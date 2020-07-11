#lang at-exp racket/base

(provide
  (all-from-out
    scribble-abbrevs
    classicthesis/lang
    gtp-plot
    gtp-util
    racket/format
    racket/list
    scriblib/figure
    scribble/example)

  PYBENCH
  unknown-author

  ~cite

  citet

  jointwork
  disclaimer
  summary
  bm
  id
  library
  Section-ref
  section-ref
  Chapter-ref
  chapter-ref

  definition
  sraapproximation
  ddeliverable
  kstep
  rkt
  overhead-long-caption

  stransient
  sguarded
  snatural

  parag
  configuration-lattice

  github-commit
  GTP
  bm-desc
  make-lib
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
  gtp-plot
  gtp-util
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

(define PYBENCH (hyperlink "https://pyperformance.readthedocs.io/" @tt{pyperformance}))
(define unknown-author "unknown")

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

(define (disclaimer . arg*)
  (nested-inset
    (list (bold "Note:") ~ arg*)))

(define (summary . txt*)
  (nested-inset txt*))

(define (nested-inset . content)
  (nested #:style 'inset content))

(define (latex-escape str)
  (string-replace str "_" "\\_"))

(define (bm str)
  (exact (list "\\textsf{" (latex-escape str) "}")))

(define (library str)
  (tt str))

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

(define overhead-long-caption
  @elem{
  The x-axis ranges over slowdown factors,
  the y-axis counts configurations,
  and a point (x, y) shows the proportion of @ddeliverable{x} configurations.
  })

(define sguarded "guarded")
(define snatural sguarded)
(define stransient "transient")

(define (Section-ref s)
  (elem "Chapter" ~ (secref s)))

(define Chapter-ref Section-ref)

(define (section-ref s)
  (elem "chapter" ~ (secref s)))

(define chapter-ref section-ref)

(define (parag . x)
  (apply elem #:style "paragraph" x))

(define (configuration-lattice n)
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error 'configuration-lattice "natural?" n))
  (performance-lattice n))

(define (github-commit user repo commit)
  (define url-str
    (format "https://github.com/~a/~a/commit/~a" user repo commit))
  (define short-commit
    (substring commit 0 7))
  (hyperlink url-str @tt[short-commit]))

(define GTP
  (exact "\\textsc{gtp}"))

(struct lib [name url] #:prefab)

(define make-lib lib)

(define (render-lib lb)
  (define u (lib-url lb))
  (define name (tt (lib-name lb)))
  (if u
    (hyperlink u name)
    name))

(define (bm-desc bm-name*
                 #:author author-str
                 #:origin origin-str
                 #:purpose purpose-str
                 #:depends [lib* #f]
                 #:url [url-str #f]
                 . descr)
  (define name-str
    (if (list? bm-name*)
      (add-between bm-name* ", ")
      bm-name*))
  (exact (elem
    "\\benchmark{"
    name-str
    "}{"
    (if (list? author-str)
      (add-between author-str ", ")
      author-str)
    "}{"
    (if url-str (hyperlink url-str origin-str) origin-str)
    "}{"
    purpose-str
    "}{"
    (apply elem descr)
    "}{"
    "}{"
    (if lib*
      (add-between (map render-lib lib*) ", ")
      "None")
    "}\n\n")))

(define rkt tt)



