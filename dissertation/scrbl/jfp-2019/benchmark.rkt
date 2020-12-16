#lang racket/base

;; Define & describe benchmark programs

(provide
  ;; -- all `define-benchmark`
  (struct-out benchmark)

  ALL-BENCHMARKS
  ;; (Listof Benchmark)
  num-benchmarks
  ;; natural?

  benchmark->num-modules
  ;; (-> Benchmark Natural)

  benchmark<?
  ;; (-> Benchmark Benchmark Boolean)

  (all-from-out greenman-thesis/jfp-2019/parameter)

  MAX-OVERHEAD

  render-static-information
  get-ratios-table
  render-ratios-table
  make-render-overhead-plot
  make-render-path-plot
  render-exact-plot
  render-relative-overhead-plot
  render-relative-exact-plot
  make-render-validate-plot
  make-render-worst-case-validate-plot*
  render-scatterplot-example
  benchmark-name->performance-info
  benchmark->num-modules
  performance-info->sample-info

  cache-dir

  deliverable*
  default-rkt-version
  gtp-version
  transient-rkt-version
)

(require
  (only-in racket/path
    file-name-from-path
    find-relative-path)
  racket/list
  racket/format
  (only-in racket/random
    random-sample)
  (only-in racket/file
    file->value)
  racket/match
  racket/serialize
  racket/runtime-path
  (for-syntax racket/base syntax/parse)
  with-cache
  (only-in scribble/base bold centered hyperlink tabular hspace tt linebreak)
  greenman-thesis/jfp-2019/parameter
  greenman-thesis/util
  (only-in greenman-thesis bm performance-info->worst-case-sample-info)
  gtp-plot
  gtp-util
  file/glob
)

;; =============================================================================

(define-runtime-path HERE ".")
(define benchmark-dir (build-path HERE "benchmarks"))
(define data-dir (build-path HERE "data"))
(define cache-dir (build-path HERE "with-cache"))

(define default-rkt-version "7.7")
(define gtp-version "6.0")
(define transient-rkt-version "7.8.0.5")
(define untyped "untyped")
(define typed "typed")
(define transient "transient")

(define MAX-OVERHEAD (*HI*))

(struct benchmark (
  name         ;; Symbol, canonical name of benchmark, like `fsm`
  author       ;; Elem, original author of benchmark
  num-adaptor  ;; Natural, number of adaptor modules in the benchmark
  origin       ;; Elem, quick description of where the benchmark came from (educational, synthetic, lib)
  purpose      ;; String, short description of what the benchmark does
  lib*         ;; (U #f (Listof Elem)), external libraries the benchmark uses
  adjlist
) #:prefab )

(define *ALL-BENCHMARKS* (box '()))
(define *NUM-OO-BENCHMARKS* (box 0))

(define library tt)

(define-syntax-rule (WARNING msg arg* ...)
  (begin
    (display "WARNING: ")
    (printf msg arg* ...)
    (newline)))

(define (benchmark->num-modules b)
  (length (benchmark-adjlist b)))

(define-syntax-rule (define-oo-benchmark stx* ...)
  (begin
    (set-box! *NUM-OO-BENCHMARKS* (+ 1 (unbox *NUM-OO-BENCHMARKS*)))
    (define-benchmark stx* ...)))

(define-syntax (define-benchmark stx)
  (syntax-parse stx
   [(_ name:id parent:id)
    #'(begin
        (define name
          (make-benchmark 'name
                          (benchmark-author parent)
                          (benchmark-num-adaptor parent)
                          (benchmark-origin parent)
                          (benchmark-purpose parent)
                          (benchmark-lib* parent)))
        (register-benchmark! name)
        (provide name))]
   [(_ name:id #:author author
               #:num-adaptor num-adaptor:nat
               #:origin origin
               #:purpose purpose:str
               (~optional (~seq #:external-libraries maybe-lib*)))
    #:with lib* (or (attribute maybe-lib*) #'#f)
    #'(begin
        (define name
          (make-benchmark 'name author num-adaptor origin purpose lib*))
        (register-benchmark! name)
        (provide name))]))

(define (make-benchmark name author num-adaptor origin purpose lib*)
  (define adjlist
    (map (compose1 path-string->string file-name-from-path)
         (glob (build-path benchmark-dir (~a name) untyped "*rkt"))))
  (benchmark name author num-adaptor origin purpose lib* adjlist))

(define (register-benchmark! name)
  (set-box! *ALL-BENCHMARKS* (cons name (unbox *ALL-BENCHMARKS*)))
  (*NUM-BENCHMARKS* (+ 1 (*NUM-BENCHMARKS*)))
  (*TOTAL-NUM-CONFIGURATIONS* (+ (expt 2 (benchmark->num-modules name))
                                 (*TOTAL-NUM-CONFIGURATIONS*))))

(define-benchmark sieve
  #:author "Ben Greenman"
  #:num-adaptor 0
  #:origin "Synthetic"
  #:purpose "Generate prime numbers"
)
(define-benchmark morsecode
  #:author "John Clements and Neil Van Dyke"
  #:num-adaptor 0
  #:origin (hyperlink "https://github.com/jbclements/morse-code-trainer/tree/master/morse-code-trainer" "Library")
  #:purpose "Morse code trainer"
)
(define-benchmark mbta
  #:author "Matthias Felleisen"
  #:num-adaptor 0
  #:origin "Educational"
  #:purpose "Interactive map"
  #:external-libraries (list (hyperlink "http://github.com/stchang/graph" (library "graph")))
)
(define-benchmark zordoz
  #:author "Ben Greenman"
  #:num-adaptor 0
  #:origin (hyperlink "http://github.com/bennn/zordoz" "Library")
  #:purpose "Explore Racket bytecode"
  #:external-libraries (list (hyperlink "http://docs.racket-lang.org/raco/decompile.html#%28mod-path._compiler%2Fdecompile%29" (library "compiler-lib")))
)
(define-benchmark suffixtree
  #:author "Danny Yoo"
  #:num-adaptor 1
  #:origin (hyperlink "https://github.com/dyoo/suffixtree" "Library")
  #:purpose "Ukkonen's suffix tree algorithm"
)
(define-benchmark lnm
  #:author "Ben Greenman"
  #:num-adaptor 0
  #:origin "Synthetic"
  #:purpose "Graphing"
  #:external-libraries (list (hyperlink "https://docs.racket-lang.org/plot/" (library "plot"))
                             ", "
                             (hyperlink "https://docs.racket-lang.org/math/stats.html" (library "math/statistics")))
)
(define-benchmark jpeg
  #:author "Andy Wingo"
  #:num-adaptor 0
  #:origin (hyperlink "https://github.com/wingo/racket-jpeg" "Library")
  #:purpose "JPEG parser"
  #:external-libraries (list (hyperlink "https://docs.racket-lang.org/math/array.html" (library "math/array"))
                             ", "
                             (hyperlink "https://docs.racket-lang.org/r6rs/R6RS_Libraries.html#(mod-path._rnrs%2Fbytevectors-6)" (library "rnrs/bytevectors-6")))
)
(define-benchmark kcfa
  #:author "Matt Might"
  #:num-adaptor 4
  #:origin (hyperlink "http://matt.might.net/articles/implementation-of-kcfa-and-0cfa/" "Educational")
  #:purpose "Explanation of k-CFA"
)
(define-benchmark zombie
  #:author "David Van Horn"
  #:num-adaptor 1
  #:origin (hyperlink "https://github.com/philnguyen/soft-contract" "Research")
  #:purpose "Game"
)
(define-benchmark snake
  #:author "David Van Horn"
  #:num-adaptor 1
  #:origin (hyperlink "https://github.com/philnguyen/soft-contract" "Research")
  #:purpose "Game"
)
(define-benchmark tetris
  #:author "David Van Horn"
  #:num-adaptor 1
  #:origin (hyperlink "https://github.com/philnguyen/soft-contract" "Research")
  #:purpose "Game"
)
(define-benchmark synth
  #:author "Vincent St. Amour \\& Neil Toronto"
  #:num-adaptor 1
  #:origin (hyperlink "http://github.com/stamourv/synth" "Application")
  #:purpose "Music synthesis DSL"
)
(define-benchmark gregor
  #:author "Jon Zeppieri"
  #:num-adaptor 2
  #:origin (hyperlink "https://docs.racket-lang.org/gregor/index.html" "Library")
  #:purpose "Date and time library"
  #:external-libraries
    (list (hyperlink "https://docs.racket-lang.org/cldr-core/index.html" (library "cldr"))
          ", "
          (hyperlink "https://docs.racket-lang.org/tzinfo/index.html" (library "tzinfo")))
)
(define-oo-benchmark dungeon
  #:author "Vincent St. Amour"
  #:num-adaptor 0
  #:origin "Application"
  #:purpose "Maze generator"
)
(define-oo-benchmark take5
  #:author "Matthias Felleisen"
  #:num-adaptor 1
  #:origin "Educational"
  #:purpose "Game"
)
(define-oo-benchmark forth
  #:author "Ben Greenman"
  #:num-adaptor 0
  #:origin (hyperlink "http://docs.racket-lang.org/forth/index.html" "Library")
  #:purpose "Forth interpreter"
)
(define-oo-benchmark acquire
  #:author "Matthias Felleisen"
  #:num-adaptor 3
  #:origin (hyperlink "https://github.com/mfelleisen/Acquire" "Educational")
  #:purpose "Game"
)
(define-benchmark fsm
  #:author "Linh Chi Nguyen" ; and matthias
  #:num-adaptor 1
  #:origin (hyperlink "https://github.com/mfelleisen/sample-fsm" "Economics Research")
  #:purpose "Economy Simulator"
)
(define-oo-benchmark fsmoo fsm)
(define-benchmark quadU
  #:author "Matthew Butterick"
  #:num-adaptor 2
  #:origin (hyperlink "https://github.com/mbutterick/quad" "Application")
  #:purpose "Typesetting"
  #:external-libraries (list (hyperlink "https://github.com/mbutterick/csp" (library "csp")))
)
(define-benchmark quadT quadU)

;; -----------------------------------------------------------------------------
;; --- Assertions & Predicates

(define (benchmark<? b1 b2)
  (define m1 (benchmark->num-modules b1))
  (define m2 (benchmark->num-modules b2))
  (or (< m1 m2)
      (and (= m1 m2)
           (symbol<? (benchmark-name b1) (benchmark-name b2)))))

(define (assoc/fail str pair*)
  (or
    (for/first ([p (in-list pair*)]
                #:when (equal? str (car p)))
      (cdr p))
    (raise-user-error 'assoc/fail "No match for '~s' in ~s" str pair*)))

(define missing-benchmark-error
  (let ([msg "Missing descriptions for benchmark(s) '~a'"])
    (lambda (name*)
      (if (*DRAFT?*)
        (WARNING msg name*)
        (raise-user-error 'benchmark msg name*)))))

(define unknown-benchmark-error
  (let ([msg "Got descriptions for unknown benchmarks '~a'. Register them at the top of 'typed-racket.rkt'"])
    (lambda  (name*)
      (if (*DRAFT?*)
        (WARNING msg name*)
        (raise-user-error 'benchmark msg name*)))))

(define ALL-BENCHMARKS (sort (unbox *ALL-BENCHMARKS*) benchmark<?))

(define num-benchmarks (length ALL-BENCHMARKS))

(define (benchmark-name->data-file bm-name version)
  (define pp
    (let* ((name bm-name)
           (patt (format "~a-*rktd" name)))
      (glob-first (build-path data-dir version patt))))
  (if (file-exists? pp)
    pp
    (raise-argument-error 'benchmark-name->data-file "directory-exists?" pp)))

(define (benchmark-name->performance-info bm-name version #:full-name? [full-name? #f])
  (when (and (eq? bm-name 'zordoz)
             (string=? version "6.4"))
    (error 'die))
  (define data-dir (benchmark-name->data-file bm-name version))
  (define extra-name (and full-name? (string->symbol (format "~a-~a" bm-name version))))
  (make-typed-racket-info data-dir #:name extra-name))

(define (performance-info->sample-info pi #:replacement? [replace? #f])
  (define num-in-sample
    (* SAMPLE-RATE (performance-info->num-units pi)))
  (define rand-gen
    (vector->pseudo-random-generator '#(2020 07 13 18 54 00)))
  (define sample*
    (for/list ((_i (in-range NUM-SAMPLE-TRIALS)))
      (random-sample (in-configurations pi) num-in-sample rand-gen #:replacement? replace?)))
  (make-sample-info pi sample*))

(define STATIC-INFO-TITLE*
  (list "Benchmark" (bold "N") "SLOC"))
  #;("Untyped LOC" "Typed LOC" "adaptors" "boundaries" "exports")

(define (render-static-information bm*)
  (define name*
    (map benchmark-name bm*))
  (centered
    (tabular
      #:sep (hspace 2)
      #:style 'block
      #:row-properties '(l bottom-border 1)
      #:column-properties (cons 'left (make-list (- (length STATIC-INFO-TITLE*) 1) 'right))
      (list* (map (λ (_) "") STATIC-INFO-TITLE*)
             STATIC-INFO-TITLE*
             (parameterize ([*current-cache-directory* cache-dir]
                            [*current-cache-keys* (list (λ () name*))]
                            [*with-cache-fasl?* #f])
               (define target "static-table.rktd")
               (with-cache (cachefile target)
                 (λ ()
                   (map render-static-row bm*))))))))

(define (render-static-row bb)
  (define name (benchmark-name bb))
  (cons
    (bm name)
    (map ~a (list
      (benchmark->num-modules bb)
      (benchmark->sloc name)))))

(define (benchmark->sloc name)
  (for/sum ((rkt (in-glob (build-path benchmark-dir (~a name) typed "*.rkt"))))
    (racket-sloc rkt)))

(define racket-sloc (make-lang-sloc "lisp"))

(define RATIOS-TITLE
  (list "Benchmark" "typed/untyped"))

(define (render-ratios-table row*)
  (centered
    (tabular
      #:sep (hspace 2)
      #:style 'block
      #:row-properties '(bottom-border 1)
      #:column-properties '(left right)
      (list* RATIOS-TITLE
             (map cdr row*)))))

(define (get-ratios-table bm*)
  (define name*
    (for/list ((bm (in-list bm*)))
      (if (benchmark? bm)
        (benchmark-name bm)
        bm)))
  (parameterize ([*current-cache-directory* cache-dir]
                 [*current-cache-keys* (list (λ () name*))]
                 [*with-cache-fasl?* #f])
    (with-cache (cachefile "ratios-table.rktd")
      (λ ()
        (for/list ([name (in-list name*)])
          (render-ratios-row name (benchmark-name->performance-info name default-rkt-version)))))))

(define (ratios-table-row r* sym)
  (or
    (for/or ([r (in-list r*)]
             #:when (eq? (car r) sym))
      r)
    (raise-argument-error 'ratios-table-row "benchmark name" 1 r* sym)))

(define (render-ratios-row name pi)
  (list name
        (bm name)
        (rnd (typed/baseline-ratio pi))))

(define (make-render-overhead-plot rkt-version)
  (lambda (bm-name)
    (define pi (benchmark-name->performance-info bm-name rkt-version))
    (define sample? (sample-info? pi))
    (define f (if sample? samples-plot overhead-plot))
    (log-bg-thesis-info "rendering (~a ~s)" (object-name f) pi)
    (parameterize ((*OVERHEAD-MAX* MAX-OVERHEAD))
      (f pi))))

(define (make-render-path-plot rkt-version)
  (lambda (n+d)
    (define bm-name (car n+d))
    (define max-dist (cdr n+d))
    (define pi (benchmark-name->performance-info bm-name rkt-version))
    (define pi%path (typed-racket-info%best-typed-path pi max-dist))
    (log-bg-thesis-info "rendering (overhead-plot ~s)" pi)
    (parameterize ((*OVERHEAD-MAX* MAX-OVERHEAD)
                   (*OVERHEAD-SHOW-RATIO* #f))
      (overhead-plot (list pi pi%path)))))

(define (render-exact-plot bm-name)
  (define pi (benchmark-name->performance-info bm-name default-rkt-version))
  (define f exact-runtime-plot)
  (log-bg-thesis-info "rendering (~a ~s)" (object-name f) pi)
  (f pi))

(define (render-relative-overhead-plot bm-name+v*)
  (define bm-name (car bm-name+v*))
  (define-values [v0 v1] (values (cadr bm-name+v*) (cddr bm-name+v*)))
  (define pi-0 (benchmark-name->performance-info bm-name v0 #:full-name? #t))
  (define pi-1 (benchmark-name->performance-info bm-name v1 #:full-name? #t))
  (parameterize ((*OVERHEAD-MAX* MAX-OVERHEAD))
    (overhead-plot (list pi-0 pi-1))))

(define (render-relative-exact-plot bm-name+v*)
  (define bm-name (car bm-name+v*))
  (define-values [v0 v1] (values (cadr bm-name+v*) (cddr bm-name+v*)))
  (define pi-0 (benchmark-name->performance-info bm-name v0 #:full-name? #t))
  (define pi-1 (benchmark-name->performance-info bm-name v1 #:full-name? #t))
  (parameterize ((*OVERHEAD-MAX* MAX-OVERHEAD)
                 (*OVERHEAD-FREEZE-BODY* #true))
    (exact-runtime-plot (list pi-0 pi-1))))

(define (make-render-validate-plot rkt-version)
  (lambda (bm-name)
    (define pi (benchmark-name->performance-info bm-name rkt-version))
    (define si (performance-info->sample-info pi #:replacement? #f))
    (log-bg-thesis-info "rendering validate-samples-plot for ~a" bm-name)
    (parameterize ((*OVERHEAD-MAX* MAX-OVERHEAD))
      (validate-samples-plot pi si))))

(define (make-render-worst-case-validate-plot* rkt-version)
  (lambda (bm-name)
    (define pi (benchmark-name->performance-info bm-name rkt-version))
    (log-bg-thesis-info "rendering validate-samples-plot for ~a" bm-name)
    (for/list ((si (in-list (performance-info->worst-case-sample-info pi))))
      (parameterize ((*OVERHEAD-MAX* MAX-OVERHEAD))
        (validate-samples-plot pi si)))))

(define (render-scatterplot-example bm-name)
  (define pi-collapse
    (make-typed-racket-info (glob-first (build-path data-dir "collapsible" (format "~a-collapse.rktd" bm-name)))
                            #:name bm-name))
  (define pi-base
    (make-typed-racket-info (glob-first (build-path data-dir "collapsible" (format "~a-6.12.rktd" bm-name)))
                            #:name bm-name))
  (define w (* 55/100 thesis-max-page-width))
  (parameterize ((*FONT-SIZE* 12)
                 (*OVERHEAD-PLOT-WIDTH* w)
                 (*OVERHEAD-PLOT-HEIGHT* w)
                 (*POINT-SIZE* 30)
                 (*POINT-SYMBOL* 'dot)
                 (*POINT-ALPHA* 0.5)
                 (*POINT-COLOR* 6))
    (relative-scatterplot pi-collapse pi-base)))

(define (deliverable* D v bm*)
  (define name* (map benchmark-name bm*))
  (parameterize ([*current-cache-directory* cache-dir]
                 [*current-cache-keys* (list (lambda () name*))]
                 [*with-cache-fasl?* #f])
    (with-cache (cachefile (format "deliverable-count-~a.rktd" D))
      (lambda ()
        (for/sum ((nm (in-list name*)))
          ((deliverable D) (benchmark-name->performance-info nm v)))))))
