#lang at-exp racket/base

(provide
  (all-from-out
    scribble-abbrevs
    classicthesis/lang
    gtp-plot
    gtp-util
    greenman-thesis/util
    racket/format
    racket/math
    racket/list
    scriblib/figure
    scribble/example)

  PYBENCH
  SLOCCOUNT
  unknown-author
  x-axis y-axis
  x-axes y-axes

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
  Figures-ref
  figures-ref

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
  github-issue

  GTP
  bm-desc
  make-lib
  render-overhead-plot*

  make-example-program-pict
)

(require
  (only-in racket/list
    add-between
    partition
    take
    last)
  classicthesis/lang
  racket/format
  racket/string
  greenman-thesis/util
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
  pict
  racket/format
  racket/math
  with-cache
  (for-syntax racket/base syntax/parse))

;; =============================================================================

(define PYBENCH (hyperlink "https://pyperformance.readthedocs.io/" @tt{pyperformance}))
(define SLOCCOUNT (hyperlink "https://dwheeler.com/sloccount/" @elem{David A. Wheeler's @tt{sloccount}}))
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
  (exact (list "\\textsf{" (latex-escape (~a str)) "}")))

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

(define sguarded "guarded")
(define snatural sguarded)
(define stransient "transient")

(define (axes q)
  (elem ($ q) "-axes"))

(define x-axes
  (axes "x"))

(define y-axes
  (axes "y"))

(define (axis q)
  (elem ($ q) "-axis"))

(define x-axis
  (axis "x"))

(define y-axis
  (axis "y"))

(define overhead-long-caption
  @elem{
  The @|x-axis| ranges over slowdown factors,
  the @|y-axis| counts configurations,
  and a point (x, y) shows the proportion of @ddeliverable{x} configurations.
  })

(define (Section-ref s)
  (elem "Chapter" ~ (secref s)))

(define Chapter-ref Section-ref)

(define (section-ref s)
  (elem "chapter" ~ (secref s)))

(define chapter-ref section-ref)

(define (make-figures-ref first-char)
  (define inner-make-figs-ref
    (case-lambda
      [(tag*)
       (if (string=? first-char "F")
         (apply Figure-ref tag*)
         (apply figure-ref tag*))]
      [(base-tag num-tags)
       (inner-make-figs-ref
         (for/list ((i (in-range num-tags)))
           (format "~a:~a" base-tag i)))]))
  inner-make-figs-ref)

(define Figures-ref
  (make-figures-ref "F"))

(define figures-ref
  (make-figures-ref "f"))

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

(define (github-issue user repo issue-name)
  (define url-str
    (format "https://github.com/~a/~a/issues/~a" user repo issue-name))
  (define short-str
    (format "~a/~a #~a" user repo issue-name))
  (hyperlink url-str (tt short-str)))

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

(define (render-overhead-plot* base-tag caption-short caption-long f-render all-bm-name* cache-dir)
  (define page* (take* all-bm-name* overhead-plots-per-page))
  (define num-pages (length page*))
  (parameterize ((*GRID-NUM-COLUMNS* 1)
                 (*GRID-X* thesis-max-page-width)
                 (*GRID-Y-SKIP* overhead-y-sep)
                 (*OVERHEAD-SHOW-RATIO* #f)
                 (*FONT-SIZE* 12)
                 (*OVERHEAD-LINE-WIDTH* 0.1))
    (for/list ((bm-name* (in-list page*))
               (page-num (in-naturals)))
      (define tag
        (if (= num-pages 1)
          base-tag
          (string-append base-tag ":" (number->string page-num))))
      (define cap
        (if (= num-pages 1)
          (list caption-short caption-long)
          (let ((short (format "~a (~a/~a)." caption-short (+ 1 page-num) num-pages)))
            (if (zero? page-num)
              (list short " " caption-long)
              short))))
      (define grid-y
        (let ((len (length bm-name*)))
          (+ (* overhead-plot-y len)
             (* overhead-y-sep (- len 1)))))
      (define pp
        (let ()
          (define (render-thunk)
            (parameterize ([*GRID-Y* grid-y])
              (grid-plot f-render bm-name*)))
          (if cache-dir
            (parameterize ([*current-cache-directory* cache-dir]
                           [*current-cache-keys* (list (λ () bm-name*))]
                           [*with-cache-fasl?* #f])
              (with-cache (cachefile (string-append tag ".rktd"))
                render-thunk))
            (render-thunk))))
      (figure* tag cap pp))))

(define (make-module-pict color)
  (define w (/ thesis-max-page-width 12))
  (file-icon w (* 1.5 w) color))

(define (make-migratable-pict)
  (make-module-pict "black"))

(define (make-contextual-pict)
  (make-module-pict "gray"))

(define (make-configuration-pict n make-pict)
  (apply hb-append 4 (build-list n (lambda (_i) (make-pict)))))

(define (make-example-program-pict)
  (hb-append
    20
    (make-configuration-pict 4 make-migratable-pict)
    (make-configuration-pict 2 make-contextual-pict)))

