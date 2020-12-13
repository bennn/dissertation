#lang racket/base

;; Define & describe benchmark programs

(provide
  (rename-out [cache-dir s:cache-dir])
  get-ratios-table
  render-ratios-table
  ratios-row-name
  ratios-row-deep
  ratios-row-shallow

  find-lowest-3dpath-D
  get-3d-table

  get-blame-table
  render-blame-table
  blame-row-name
  blame-row-blame
  blame-row-deep
  blame-row-shallow

  get-mixed-path-table
  render-mixed-path-table

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
  (only-in racket/math
    exact-round
    exact-floor)
  (only-in math/number-theory factorial)
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
  (only-in racket/sequence
    sequence-map)
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
(define 3d "3d")

(define NSA-num-cores 4)
(define NSA-core-name "i7-4790")
(define NSA-core-speed "3.60GHz")
(define NSA-RAM "16GB")
(define NSA-timeout-minutes 10)

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
  tetris
  synth
  gregor
  quadT
  quadU
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
  (define timeout-ms (* 10 ;; min
                        60 ;; min->sec
                        1000)) ;; sec->ms
  (case bd
    ((timeout) (format "timeout (>~a)" (exact-floor (/ timeout-ms u-t))))
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
  (define str (caddr (cdr r)))
  (string->number str))

(define (blame-row-deep r)
  (define str (cadddr (cdr r)))
  (string->number str))

;; ---

(define MIXED-WORST-TITLE
  (list "Benchmark"
        "worst before"
        "worst after"
        "   improvement"))

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
  (define worst-before (max s-max d-max))
  (define worst-after (min s-max d-max))
  (define x-improved (/ worst-before worst-after))
  (list name
        (bm name)
        (string-append (rnd worst-before) "x")
        (string-append (rnd worst-after) "x")
        (let ((v (exact-floor x-improved)))
          (if (= v 1)
            "<2x"
            (format "~ax" v)))))

;; ---

(define MIXED-PATH-TITLE
  (list "Benchmark"
        "Deep %"
        "Shallow %"
        "D. or S. %"))

(define (render-mixed-path-table row*)
  ;; TODO abstraction
  (centered
    (tabular
      #:sep (hspace 2)
      #:style 'block
      #:row-properties '(bottom-border 1)
      #:column-properties '(left right)
      (list* MIXED-PATH-TITLE
             (map cdr row*)))))

(define (get-mixed-path-table D name*)
  ;; TODO copied from above
  (parameterize ([*current-cache-directory* cache-dir]
                 [*current-cache-keys* (list (λ () (cons D name*)))]
                 [*with-cache-fasl?* #f])
    (with-cache (cachefile "mixed-path-table.rktd")
      (λ ()
        (for/list ([name (in-list name*)])
          (make-mixed-path-row D name
                          (benchmark-name->performance-info name stransient)
                          (benchmark-name->performance-info name default-rkt-version)))))))

(define (make-mixed-path-row D name pi-shallow pi-deep)
  (define total-paths (factorial (performance-info->num-units pi-deep)))
  (define (fmt n0 n1)
    (~a (exact-round (pct n0 n1))))
  (list name
        (bm name)
        (fmt (count-deliv-path D pi-deep) total-paths)
        (fmt (count-deliv-path D pi-shallow) total-paths)
        (fmt (count-deliv-path D pi-shallow pi-deep) total-paths)))

;; D = 2 data
;;    ((sieve)
;;     (list name (bm name) "0" "0" "0"))
;;    ((forth)
;;     (list name (bm name) "0" "0" "17"))
;;    ((fsm)
;;     (list name (bm name) "33" "0" "100"))
;;    ((fsmoo)
;;     (list name (bm name) "0" "0" "50"))
;;    ((mbta)
;;     (list name (bm name) "100" "100" "100"))
;;    ((morsecode)
;;     (list name (bm name) "100" "0" "100"))
;;    ((zombie)
;;     (list name (bm name) "0" "0" "0"))
;;    ((dungeon)
;;     (list name (bm name) "0" "0" "0"))
;;    ((jpeg)
;;     (list name (bm name) "0" "100" "100"))
;;    ((zordoz)
;;     (list name (bm name) "50" "0" "67"))
;;    ((lnm)
;;     (list name (bm name) "100" "100" "100"))
;;    ((suffixtree)
;;     (list name (bm name) "0" "0" "0"))
;;    ((kcfa)
;;     (list name (bm name) "0" "100" "100"))
;;    ((snake)
;;     (list name (bm name) "0" "0" "0"))
;;    ((take5)
;;     (list name (bm name) "0" "0" "16"))

;; D = 3
;;   ((sieve)
;;    (list name (bm name) "0" "0" "100"))
;;   ((forth)
;;    (list name (bm name) "0" "0" "50"))
;;   ((fsm)
;;    (list name (bm name) "100" "100" "100"))
;;   ((fsmoo)
;;    (list name (bm name) "0" "0" "50"))
;;   ((mbta)
;;    (list name (bm name) "100" "100" "100"))
;;   ((morsecode)
;;    (list name (bm name) "100" "100" "100"))
;;   ((zombie)
;;    (list name (bm name) "0" "0" "50"))
;;   ((dungeon)
;;    (list name (bm name) "0" "0" "67"))
;;   ((jpeg)
;;    (list name (bm name) "0" "100" "100"))
;;   ((zordoz)
;;    (list name (bm name) "100" "100" "100"))
;;   ((lnm)
;;    (list name (bm name) "100" "100" "100"))
;;   ((suffixtree)
;;    (list name (bm name) "0" "0" "12"))
;;   ((kcfa)
;;    (list name (bm name) "33" "100" "100"))
;;   ((snake)
;;    (list name (bm name) "0" "0" "0"))
;;   ((take5)
;;    (list name (bm name) "0" "100" "100"))
;;    (else
;;      (raise-argument-error 'make-mixed-path-row "simple-bm-name?" name))))

(define (count-deliv-path D . pi*)
  (when (null? pi*)
    (raise-argument-error 'count-deliv-path "(non-empty-listof performance-info?)" pi*))
  (for*/sum ((cfg-id* (all-paths (car pi*)))
             #:when (andmap (lambda (cfg-id)
                              (<= (apply min (map (lambda (pi) (overhead+ pi cfg-id)) pi*)) D))
                            cfg-id*))
      1))

(define (overhead+ pi cfg-id)
  (/ (performance-info->runtime pi cfg-id)
     (performance-info->untyped-runtime pi)))

(define (performance-info->runtime pi cfg-id)
  (or
    (for/first ((cfg (in-configurations pi))
                #:when (equal? cfg-id (configuration-info->id cfg)))
      (configuration-info->mean-runtime cfg))
    (raise-arguments-error 'performance-info->runtime "cfg not found" "pi" pi "cfg" cfg-id)))

(define (all-paths pi)
  (define num-units (performance-info->num-units pi))
  (all-paths-from (natural->bitstring 0 #:bits num-units)))

(define (all-paths-from str)
  (sequence-map
    permutation->path
    (in-permutations (range (string-length str)))))

(define (permutation->path index*)
  (define L (length index*))
  ;; Create the path in reverse order
  (for/fold ([acc (list (bitstring-init L #:hi? #t))])
            ([i (in-list index*)])
    (cons (bitstring-flip (car acc) i) acc)))

(define (bitstring-init n #:hi? [hi? #f])
  (make-string n (if hi? #\1 #\0)))

(define (bitstring-flip str i)
  (define new (if (equal? #\0 (string-ref str i)) "1" "0"))
  (string-append (substring str 0 i)
                 new
                 (substring str (add1 i) (string-length str))))

(define (find-lowest-3dpath-D bm-name)
  (string->number (rnd
  (parameterize ([*current-cache-directory* cache-dir]
                 [*current-cache-keys* (list (λ () bm-name))]
                 [*with-cache-fasl?* #f])
    (with-cache (cachefile "mixed-bestpath.rktd")
      (λ ()
        (define pi (benchmark-name->performance-info3d bm-name))
        (define total-paths (factorial (performance-info->num-units pi)))
        (or
          (for/or ((pre-d (in-range 10)))
            (define D (+ 1 (/ pre-d 10)))
            (and (= total-paths (count-deliv-path D pi))
                 D))
          (raise-arguments-error 'both "cannot find D for 100% paths" "bm" bm-name))))))))

(define (filename-3d bm-name)
  (define orig-filename
    (let ((m* (glob (build-path data-dir 3d (format "~a-*.rktd" bm-name)))))
      (if (or (null? m*) (not (null? (cdr m*))))
        (raise-arguments-error 'benchmark-name->performance-info3d "expected one match" "bm" bm-name "match*" m*)
        (car m*))))
  (define best-filename (path-add-extension orig-filename ".3d"))
  (values orig-filename best-filename))

(define (benchmark-name->performance-info3d bm-name)
  (define-values [orig-filename best-filename] (filename-3d bm-name))
  (define best-cfg*
    (with-input-from-file
      orig-filename
      (lambda ()
        (void (read-line)) ;; ignore lang
        (define (read-cfg) (string->value (read-line)))
        (define untyped-cfg (read-cfg))
        (define num-units (string-length (car untyped-cfg)))
        (cons
          untyped-cfg
          (for/list ((i (in-range (- (expt 2 num-units) 1))))
            (define cfg0 (read-cfg))
            (define num-types (for/sum ((c (in-string (car cfg0))) #:unless (eq? #\0 c)) 1))
            (define cfg* (for/list ((j (in-range (- (expt 2 num-types) 1)))) (read-cfg)))
            (car (sort (cons cfg0 cfg*) <=2 #:key cfg->simple-time+mix #:cache-keys? #true)))))))
  (void
    (with-output-to-file
      best-filename
      #:exists 'replace
      (lambda ()
        (displayln "#lang gtp-measure/output/typed-untyped")
        (for ((ln (in-list best-cfg*)))
          (writeln ln)))))
  (make-typed-racket-info best-filename #:name (string->symbol (format "~a-3d" bm-name))))

(define (cfg->simple-time val)
  (string->number (rnd (mean (map time-string->cpu-time (cadr val))))))

(define (cfg->simple-time+mix val)
  (cons (cfg->simple-time val) (has-mix? (car val))))

(define (has-mix? str)
  (define c* (string->list str))
  (for/and ((xxx (in-list '(#\0 #\1 #\2))))
    (memq xxx c*)))

(define (<=2 p0 p1)
  (or (<= (car p0) (car p1))
      (and (= (car p0) (car p1))
           (<= (cdr p0) (cdr p1)))))

(define (get-3d-table bm-name*)
  (parameterize ([*current-cache-directory* cache-dir]
                 [*current-cache-keys* (list (λ () bm-name*))]
                 [*with-cache-fasl?* #f])
    (with-cache (cachefile "mixed-3d-best.rktd")
      (λ ()
        (for/list ((bm (in-list bm-name*)))
          (define pi (benchmark-name->performance-info3d bm))
          (define total-configs (performance-info->num-configurations pi))
          (define-values [_orig best-filename] (filename-3d bm))
          (define num-good
            (with-input-from-file
              best-filename
              (lambda ()
                (void (read-line))
                (for/sum ((cfg (in-lines))
                          #:when (has-mix? (car (string->value cfg))))
                  1))))
          (list bm (rnd (pct num-good total-configs))))))))

