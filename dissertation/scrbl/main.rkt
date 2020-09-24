#lang at-exp racket/base

(provide
  (all-from-out
    scribble-abbrevs
    greenman-thesis/classicthesis/lang
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

  render-lib

  shorturl

  ~cite
  (rename-out (~cite ~citep))

  citet

  jointwork
  disclaimer
  summary
  bm
  codett
  code-nested
  id
  library
  Section-ref
  section-ref
  Chapter-ref
  chapter-ref
  Table-ref
  table-ref
  Figures-ref
  figures-ref
  (rename-out
    [Section-ref Sectionref]
    [section-ref sectionref]
    [Chapter-ref Chapterref]
    [chapter-ref chapterref]
    [Figure-ref Figureref]
    [figure-ref figureref]
    [Table-ref Tableref]
    [table-ref tableref])

  noindent
  leavevmode
  equation
  latex-label
  latex-ref

  definition
  sraapproximation
  ddeliverable
  kstep
  futurework
  exercise
  rkt
  overhead-long-caption
  exact-long-caption

  sdeep
  sguarded
  snatural
  sshallow
  stransient
  scm
  sts
  ename cname fname aname nname tname

  sDeep
  sGuarded
  sNatural
  sShallow
  sTransient
  sCM
  sTS

  parag
  configuration-lattice

  github-commit
  github-issue
  github-pull

  GTP
  bm-desc
  make-lib
  render-overhead-plot*

  example-type-shape
  make-example-program-pict
  gtp-url
)

(require
  (only-in racket/list
    add-between
    partition
    take
    take-right
    last)
  greenman-thesis/classicthesis/lang
  racket/format
  racket/string
  greenman-thesis/util
  gtp-plot
  gtp-util
  scribble/example
  scribble-abbrevs
  (only-in scribble/core
    make-style
    make-element
    make-paragraph
    make-compound-paragraph
    make-nested-flow
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

(define (jointwork #:people* people* #:paper* [paper* '()] #:extra [e-elem #f])
  (nested-inset
    (emph (list
            "This chapter is based on joint work with: "
            (oxfordize people*)
            (~cite (string-join paper* ",")) "."
            (if e-elem (list " " e-elem) '())))))

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

(define codett tt)

(define library tt)

(define id ~a)

(define (latex-label tag)
  (make-element (make-style "label" '(exact-chars)) tag))

(define (latex-ref tag)
  (make-element (make-style "ref" '(exact-chars)) tag))

(define leavevmode
  (make-element (make-style "leavevmode" '()) ""))

(define noindent
  (make-element (make-style "noindent" '()) ""))

;; TODO add name
;(define (theorem title #:tag tag . content)
;  (nested #:style "theorem"
;    (para (latex-label tag) content)

(define (equation tag content)
  (nested #:style "equation"
    (para (latex-label tag) content)))

;; equation ref
(define (_rogramref first-letter tag)
  (exact (format "\\~arogramref{~a}" first-letter tag)))

(define (Programref tag)
  (_rogramref "P" tag))

(define (programref tag)
  (_rogramref "p" tag))

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

(define (futurework . str*)
  (exercise* 'RQ str*))

;; TODO index of exercises?
(define (exercise difficulty . str*)
  (exercise* difficulty str*))

(define (exercise* d str*)
  (nested #:style (make-style 'code-inset '()) #;never-indents?
    (list @emph{Exercise}
          ~
          (format-difficulty d)
          ~
          str*)))

(define (format-difficulty d)
  (exact "(" (difficulty->tex d) ")"))

(define (difficulty->tex d)
  (define s "\\(\\star\\)")
  (case d
    ((1) s)
    ((2) (string-append s s))
    ((3) (string-append s s s))
    ((RQ) (string-append s s s "\\(\\,\\)\\ldots"))
    (else (raise-argument-error 'difficulty->tex "difficulty?" d))))

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

(define (string-1-titlecase str)
  (if (< 0 (string-length str))
    (string-append (string-upcase (substring str 0 1)) (substring str 1))
    str))

(define sdeep "deep")
(define sguarded "guarded")
(define snatural "natural")
(define sshallow "shallow")
(define stransient "transient")
(define sts "type soundness")
(define scm "complete monitoring")

(define ename (exact "\\ename{}"))
(define aname (exact "\\aname{}"))
(define tname (exact "\\tname{}"))
(define fname (exact "\\fname{}"))
(define cname (exact "\\cname{}"))
(define nname (exact "\\nname{}"))

(define (shorturl a b)
  (hyperlink (string-append a b) b))

(define sDeep (string-titlecase sdeep))
(define sGuarded (string-titlecase sguarded))
(define sNatural (string-titlecase snatural))
(define sShallow (string-titlecase sshallow))
(define sTransient (string-titlecase stransient))
(define sTS (string-1-titlecase sts))
(define sCM (string-1-titlecase sts))

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

(define exact-long-caption
  @elem{
  The @|x-axis| ranges over the number of active typed units,
  the @|y-axis| shows exact running times,
  and a point (x, y) shows one running time for one configuration with @${x}
  types.
  })

(define (Section-ref s)
  (elem "Chapter" ~ (seclink s)))

(define (section-ref s)
  (elem "chapter" ~ (seclink s)))

(define Chapter-ref Section-ref)

(define chapter-ref section-ref)

(define (able-ref first-letter lbl)
  (elem (list first-letter "able" ~ (elem #:style "ref" lbl))))

(define (Table-ref s)
  (able-ref "T" s))

(define (table-ref s)
  (able-ref "t" s))

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

(define ((make-github-url kind) user repo name)
  (define url-str
    (format "https://github.com/~a/~a/~a/~a" user repo kind name))
  (define short-str
    (format "~a/~a #~a" user repo name))
  (hyperlink url-str (tt short-str)))

(define github-issue (make-github-url "issues"))
(define github-pull (make-github-url "pull"))

(define GTP
  (exact "\\textsc{gtp}"))

(define gtp-url
  @format-url{https://docs.racket-lang.org/gtp-benchmarks/index.html})

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
                 (*LEGEND-Y-SKIP* 0)
                 (*OVERHEAD-SHOW-RATIO* #f)
                 (*AUTO-POINT-ALPHA?* #f)
                 (*POINT-ALPHA* 0.4)
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
          (let ((fig-num (format " (~a/~a)." (+ 1 page-num) num-pages)))
            (if (zero? page-num)
              (list caption-short fig-num " " caption-long)
              (list caption-short fig-num)))))
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

(define (code-nested . content)
  (nested #:style 'code-inset (add-between (map codett content) (elem "\n"))))

(define (example-type-shape #:type t-str #:shape s-str #:cost c-str)
  @exact{\begin{tabular}[t]{lcl}
    \(\stype\) & \(=\) & @codett[t-str]
  \\
    \(\tagof{\stype}\) & \(=\) & @codett[s-str]
  \end{tabular}})
@; \\
@;   cost & \(\approx\) & \(@|c-str|\)


