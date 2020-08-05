#lang racket/base

;; Define & describe benchmark programs

(provide
  get-ratios-table
  render-ratios-table
  ratios-row-name
  ratios-row-deep
  ratios-row-shallow
  SHALLOW-CURRENT-BENCHMARK*

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
  (only-in greenman-thesis
    sdeep
    sshallow
    stransient
    bm)
  (only-in greenman-thesis/jfp-2019/main
    benchmark-name->performance-info
    default-rkt-version)
  gtp-plot
  gtp-util
  file/glob
)

;; -----------------------------------------------------------------------------

(define-runtime-path HERE ".")
(define cache-dir (build-path HERE "with-cache"))

(define SHALLOW-CURRENT-BENCHMARK* '(
  sieve
  forth
  fsm
  fsmoo
  mbta
  morsecode
  zombie
  dungeon
  jpeg
  zordoz
  lnm
  suffixtree
  kcfa
  snake
  take5
  acquire
  synth
))

(define RATIOS-TITLE
  (list "Benchmark"
        (string-append sdeep "/untyped")
        (string-append sshallow "/untyped")))

(define (render-ratios-table row*)
  ;; TODO copied from JFP
  (centered
    (tabular
      #:sep (hspace 2)
      #:style 'block
      #:row-properties '(bottom-border 1)
      #:column-properties '(left right)
      (list* RATIOS-TITLE
             (map cdr row*)))))

(define (get-ratios-table name*)
  ;; TODO copied from JFP
  (parameterize ([*use-cache?* #f] ;; TODO enable cache
                 [*current-cache-directory* cache-dir]
                 [*current-cache-keys* (list (λ () name*))]
                 [*with-cache-fasl?* #f])
    (with-cache (cachefile "ratios-table.rktd")
      (λ ()
        (for/list ([name (in-list name*)])
          (render-ratios-row name
                             (benchmark-name->performance-info name default-rkt-version)
                             (benchmark-name->performance-info name stransient)))))))

(define (render-ratios-row name pi-deep pi-shallow)
  (list name
        (bm name)
        (rnd (typed/baseline-ratio pi-deep))
        (rnd (typed/baseline-ratio pi-shallow))))

(define ratios-row-name cadr)

(define (ratios-row-deep r)
  (string->number (caddr r)))

(define (ratios-row-shallow r)
  (string->number (cadddr r)))
