#lang racket/base

;; Extra script to validate samples
;; - pick top benchmarks with huge (all-best, all-worst) intervals
;;   * [take5 tetris synth quadU quadT]
;; - plot dangers in sampling experiment
;;   * take N samples, for a big N
;;   * look at the gaps from the interval to the true proportion
;;   * plot all gaps in a histogram
;;
;; negative and positive gaps are different ... keep track, to distinguish
;;  accurate vs. inaccurate intervals
;;

(require
  file/glob
  (only-in racket/random random-sample)
  (only-in math/statistics mean stddev/mean)
  racket/list
  pict pict-abbrevs
  (only-in gtp-util string->value time-string->cpu-time rnd)
  gtp-plot/typed-racket-info gtp-plot/performance-info
  (only-in plot/utils linear-seq)
  (only-in racket/math exact-round)
  plot/no-gui
  with-cache)


(define bm*
  '(take5 tetris synth quadU quadT))

(define rkt-version
  "7.7")

(define dmax 20)
(define num-d 200)

(define num-experiments 800)
(define sample-rate 10)
(define num-samples 10)

(define *CONFIDENCE-LEVEL* (make-parameter 95))

(define (find-data bm-name)
  (define path* (glob (build-path "jfp-2019" "data" rkt-version (format "~a-*.rktd" bm-name))))
  (cond
    [(or (null? path*) (not (null? (cdr path*))))
     (raise-arguments-error 'find-data "ambiguous data file" "bm" bm-name "match*" path*)]
    [else
     (car path*)]))

;; path-string -> listof mean
(define (parse-data data-file)
  (with-input-from-file
    data-file
    (lambda ()
      (void (read-line))
      (for/list ((ln (in-lines)))
        (define v (string->value ln))
        (mean (map time-string->cpu-time (cadr v)))))))

(define (count-d-deliverable t* d baseline)
  (for/sum ((t (in-list t*)))
    (if (<= (/ t baseline) d) 1 0)))

(define (count-deliverable t*)
  (define baseline (car t*))
  (for/list ((d (in-list (linear-seq 1 dmax num-d))))
    (list d (count-d-deliverable t* d baseline))))

(define (log2 n)
  (let loop ((i 1))
    (cond
      [(< n i)
       (raise-argument-error 'log2 "off the rails" n)]
      [(= n (expt 2 i))
       i]
      [else
        (loop (+ i 1))])))

(define (lower-confidence n*)
  (- (mean n*) (error-bound n*)))

(define (upper-confidence n*)
  (+ (mean n*) (error-bound n*)))

(define (error-bound n*)
  (define cv
    (case (*CONFIDENCE-LEVEL*)
     [(95) 1.96]
     [(98) 2.326]
     [else (error 'error-bounds "Unknown confidence level '~a'" (*CONFIDENCE-LEVEL*))]))
  (confidence-offset n* #:cv cv))

(define (confidence-offset x* #:cv [cv 1.96])
  (define u (mean x*))
  (define n (length x*))
  (define s (stddev/mean u x*))
  (define cv-offset (/ (* cv s) (sqrt n)))
  (if (negative? cv-offset)
    (raise-user-error 'confidence-interval "got negative cv offset ~a\n" cv-offset)
    cv-offset))

(define (make-sample t* D* num-modules)
  (define num-configs (expt 2 num-modules))
  (define baseline (car t*))
  (define num-in-sample (* num-modules sample-rate))
  (define s*
    (for/list ((_s (in-range num-samples)))
      (random-sample t* num-in-sample #:replacement? #f)))
  (for/list ((dd (in-list D*)))
    (define curr-d (first dd))
    (define %-good* (map (lambda (s) (/ (count-d-deliverable s curr-d baseline) num-in-sample)) s*))
    (list (lower-confidence %-good*)
          (upper-confidence %-good*))))

(define (take-samples t* D* num-modules)
  (define parallel
    (for/list ((_i (in-range num-experiments)))
      (make-sample t* D* num-modules)))
  parallel)

(define (collect-sample-data bm)
  (let* ((data-file (find-data bm))
         (t* (parse-data data-file))
         (num-configs (length t*))
         (num-modules (log2 num-configs))
         (D* (count-deliverable t*))
         (true%* (map (lambda (d) (/ (cadr d) num-configs)) D*))
         (parallel (take-samples t* D* num-modules)))
    (cons true%* parallel)))

(define (eval-all-guesses sample-data)
  (define true%* (car sample-data))
  (define parallel (cdr sample-data))
  (for/fold ((acc (hash)))
            ((ppp (in-list parallel)))
    (for/fold ((acc acc))
              ((true% (in-list true%*))
               (ivl (in-list ppp)))
      (define lo% (first ivl))
      (define hi% (second ivl))
      (hist-add
        (hist-add
          acc
          (- hi% true%))
        (- true% lo%)))))

(define (eval-avg-dist sample-data)
  (define true%* (car sample-data))
  (define parallel (cdr sample-data))
  (for/fold ((acc (hash)))
            ((ppp (in-list parallel)))
    (define diff**
      (for/list ((ivl (in-list ppp))
                 (true% (in-list true%*)))
        (for/list ((guess% (in-list ivl)))
          (abs (- true% guess%)))))
    (define k (mean (apply append diff**)))
    (hist-add acc k)))

(define (hist-add H pre-k)
  (define k (exact-round (* pre-k 100)))
  (hash-update H k add1 (lambda () 0)))

(define (pair->vector pp)
  (vector (car pp) (cdr pp)))

(define (make-x-ticks x*)
  (define min-x (min* x*))
  (define max-x (max* x*))
  (define MAJOR-TICKS
    (sort (list min-x max-x -100 -50 -10 0 10 50 100) <))
  (define m-ticks
    (ticks (real*->ticks-layout MAJOR-TICKS)
           (lambda (ax-min ax-max pt*)
             (for/list ((pt (in-list pt*)))
               (number->string (pre-tick-value pt))))))
  m-ticks)

(define max-%-plot 10)

(define (make-avg-x-ticks x*)
  (define min-x (min* x*))
  (define max-x (max* x*))
  (define MAJOR-TICKS
    (sort (list min-x max-x (- max-%-plot) 0 max-%-plot) <))
  (define MINOR-TICKS
    (list -5 5))
  (define m-ticks
    (ticks (real*->ticks-layout MAJOR-TICKS)
           (ticks-format/units "%")))
  (ticks-add m-ticks MINOR-TICKS #f))

(define ((ticks-format/units units) ax-min ax-max pre-ticks)
  (for/list ([pt (in-list pre-ticks)])
    (define v (pre-tick-value pt))
    (if (= v ax-max)
      (format "~a~a" v units)
      (number->string v))))

(define ((real*->ticks-layout x*) ax-min ax-max)
  (for/list ([x (in-list x*)])
    (pre-tick x #t)))

;; -----------------------------------------------------------------------------

(module+ main
  (for ((bm (in-list bm*))
        #;(i (in-range 1)))


    (printf "~a go ...~n" bm)
    (parameterize ((*current-cache-directory* "sample")
                   (*current-cache-keys* (list (lambda () bm*)))
                   (*with-cache-fasl?* #f))
      (let* ((sample-data ;; (list D %-deliv (listof lo-% hi-%))
               (with-cache (cachefile (format "~a-sample-data.rktd" bm))
                 (lambda ()
                   (collect-sample-data bm))))
             (all
               (with-cache (cachefile (format "~a-all-guesses.rktd" bm))
                 (lambda ()
                   (eval-all-guesses sample-data))))
             #;(avg*
               (with-cache (cachefile (format "~a-avg-dist.rktd" bm))
                 (lambda ()
                   (eval-avg-dist sample-data))))
             (all* (hash->list all))

             )
        (define pp
          (parameterize ((plot-x-ticks (make-avg-x-ticks (map car all*)))
                         (plot-x-far-ticks no-ticks)
                         (plot-y-far-ticks no-ticks)
                         (plot-font-size 16))
            (plot-pict
              (rectangles
                (for/list ((a (in-list all*)))
                  (vector (ivl (car a) (+ 1 (car a)))
                          (ivl 0 (cdr a)))))
              #:width 400
              #:height 200
              #:x-min (- max-%-plot)
              #:x-max max-%-plot
              #:y-max (+ 1 (max* (map cdr all*)))
              #:x-label #f #:y-label #f)))
        (save-pict (cachefile (format "~a-vs.png" bm))
                   pp)))))



