#lang racket/base

;; Define & describe benchmark programs

(provide
  (rename-out [cache-dir s:cache-dir])
  get-ratios-table
  render-ratios-table
  ratios-row-name
  ratios-row-deep
  ratios-row-shallow

  get-blame-table
  render-blame-table
  blame-row-name
  blame-row-blame
  blame-row-deep
  blame-row-shallow

  get-mixed-worst-table
  render-mixed-worst-table

  SHALLOW-CURRENT-BENCHMARK*

  NSA-num-cores
  NSA-core-name
  NSA-core-speed
  NSA-RAM
  NSA-timeout-minutes
  )

(require
  (only-in math/statistics
    mean)
  (only-in racket/path
    file-name-from-path
    find-relative-path)
  racket/list
  racket/format
  (only-in racket/random
    random-sample)
  (only-in racket/file
    file->lines
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
(define data-dir (build-path HERE "data"))

(define blame "blame")

(define NSA-num-cores 4)
(define NSA-core-name "i7-4790")
(define NSA-core-speed "3.60GHz")
(define NSA-RAM "16GB")
(define NSA-timeout-minutes 10)

(define SHALLOW-CURRENT-BENCHMARK* '(
  sieve
;  forth
  fsm
  fsmoo
  mbta
  morsecode
;  zombie
;  dungeon
;  jpeg
;  zordoz
  lnm
;  suffixtree
  kcfa
;  snake
;  take5
;  acquire
;  tetris
;  synth
;  gregor
;  quadT
))

(define RATIOS-TITLE
  (list "Benchmark"
        (string-append sdeep "/untyped")
        (string-append sshallow "/untyped")))

(define (render-ratios-table row*)
  ;; TODO copied from JFP ... should we use tex instead?
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
  (parameterize ([*current-cache-directory* cache-dir]
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

;; ---

(define BLAME-TITLE
  (list "Benchmark"
        "s.blame/untyped"
        (string-append sshallow "/untyped")
        (string-append sdeep " worst-case")))

(define (render-blame-table row*)
  ;; TODO abstraction
  (centered
    (tabular
      #:sep (hspace 2)
      #:style 'block
      #:row-properties '(bottom-border 1)
      #:column-properties '(left right)
      (list* BLAME-TITLE
             (map cdr row*)))))

(define (get-blame-table name*)
  ;; TODO copied from above
  (parameterize ([*current-cache-directory* cache-dir]
                 [*current-cache-keys* (list (λ () name*))]
                 [*with-cache-fasl?* #f])
    (with-cache (cachefile "blame-table.rktd")
      (λ ()
        (for/list ([name (in-list name*)])
          (make-blame-row name
                          (benchmark-name->blame-info name)
                          (benchmark-name->performance-info name stransient)
                          (benchmark-name->performance-info name default-rkt-version)))))))

(define (make-blame-row name bd pi-shallow pi-deep)
  (list name
        (bm name)
        (format-blame-data bd (performance-info->untyped-runtime pi-shallow))
        (rnd (typed/baseline-ratio pi-shallow))
        (rnd (max-overhead pi-deep))))

(define (benchmark-name->blame-info name)
  (define blame-file (build-path data-dir blame (format "~a.rktd" name)))
  (define data-line*
    (let ((l* (file->lines blame-file)))
      (unless (string=? "#lang gtp-measure/output/file" (car l*))
        (raise-argument-error 'benchmark-name->blame-info "#lang gtp-measure file" blame-file))
      (cdr l*)))
  (cond
    [(and (null? (cdr data-line*))
          (string=? "timeout 666000" (car data-line*)))
     'timeout]
    [(and (null? (cdr data-line*))
          (regexp-match? #rx"^/bin/sh: line [0-9]: [0-9]+ Killed" (car data-line*)))
     'oom]
    [else
     (for/list ((dl (in-list data-line*)))
       (time-string->cpu-time dl))]))

(define (format-blame-data bd u-t)
  (case bd
    ((timeout) "timeout")
    ((oom) "out of memory")
    (else (rnd (/ (mean bd) u-t)))))

(define (blame-data? x)
  (or (listof-real? x)
      (eq? x 'timeout)
      (eq? x 'oom)))

(define (listof-real? x)
  (and (list? x) (andmap real? x)))

(define blame-row-name cadr)

(define (blame-row-blame r)
   (define v (caddr r))
   (or (string->number v)
       v))

(define (blame-row-shallow r)
  (string->number (caddr (cdr r))))

(define (blame-row-deep r)
  (string->number (cadddr (cdr r))))

;; ---

(define MIXED-WORST-TITLE
  (list "Benchmark"
        "worst before"
        "worst after"))

(define (render-mixed-worst-table row*)
  ;; TODO abstraction
  (centered
    (tabular
      #:sep (hspace 2)
      #:style 'block
      #:row-properties '(bottom-border 1)
      #:column-properties '(left right)
      (list* MIXED-WORST-TITLE
             (map cdr row*)))))

(define (get-mixed-worst-table name*)
  ;; TODO copied from above
  (parameterize ([*current-cache-directory* cache-dir]
                 [*current-cache-keys* (list (λ () name*))]
                 [*with-cache-fasl?* #f])
    (with-cache (cachefile "mixed-worst-table.rktd")
      (λ ()
        (for/list ([name (in-list name*)])
          (make-mixed-worst-row name
                          (benchmark-name->performance-info name stransient)
                          (benchmark-name->performance-info name default-rkt-version)))))))

(define (make-mixed-worst-row name pi-shallow pi-deep)
  (define s-max (max-overhead pi-shallow))
  (define d-max (max-overhead pi-deep))
  (list name
        (bm name)
        (rnd (max s-max d-max))
        (rnd (min s-max d-max))))

