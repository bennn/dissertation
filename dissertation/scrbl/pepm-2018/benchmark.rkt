#lang at-exp racket/base

(provide
  MAIN-BENCHMARKS
  NUM-MAIN-BENCHMARKS
  RT
  MAX-OVERHEAD
  NUM-ITERATIONS
  PYTHON
  SAMPLE-RATE
  NUM-SAMPLE-TRIALS
  NUM-EXHAUSTIVE-BENCHMARKS
  DLS-2014-BENCHMARK-NAMES
  POPL-2017-BENCHMARK-NAMES


  render-exact-runtime-plot*
  exact-runtime-category
  lib-desc
)

(require
;  "script/config.rkt"
;  "script/benchmark-info.rkt"
;  "script/render.rkt"
;  "script/util.rkt"
;  (only-in "script/plot.rkt"
;    *CONFIGURATION-X-JITTER*
;    *OVERHEAD-MAX*)
;  (only-in racket/class
;    class new super-new object% define/public)
;  (only-in racket/list
;    add-between
;    partition)
;  racket/format
;  racket/string
;  pict
;  scribble/acmart
;  scribble/core
;  scribble/example
;  scribble/html-properties
;  scribble/latex-properties
;  scriblib/autobib
;  scriblib/figure
;  setup/main-collects


  pict
  gtp-util
  gtp-plot/performance-info
  gtp-plot/reticulated-info
  (for-syntax racket/base syntax/parse))

;; -----------------------------------------------------------------------------

(define NUM-ITERATIONS 40)
(define PYTHON "Python 3.4.3")
(define SAMPLE-RATE 10)
(define NUM-SAMPLE-TRIALS 10)
(define MAX-OVERHEAD 10)

(define (->benchmark sym)
  (raise-user-error '->benchmark "not-implemented"))

(define (get-ratios-table bm*)
  (blank))

(define (percent-slower-than-typed pre-bm)
  (define pi (make-reticulated-info (->benchmark pre-bm)))
  (define total (performance-info->num-configurations pi))
  (define num-good ((deliverable (typed/baseline-ratio pi)) pi))
  (round (pct (- total num-good) total)))

(define (render-exact-runtime-plot* bm-name*)
  (blank))

(define (exact-runtime-category title bm* make-text)
  (raise-user-error 'exact-runtime-category "not-implemented"))

(struct lib [name url] #:prefab)

(define LIB-INDEX*
  (list (lib "copy" "https://docs.python.org/3/library/copy.html")
        (lib "fnmatch" "https://docs.python.org/3/library/fnmatch.html")
        (lib "itertools" "https://docs.python.org/3/library/itertools.html")
        (lib "math" "https://docs.python.org/3/library/math.html")
        (lib "operator" "https://docs.python.org/3/library/operator.html")
        (lib "os" "https://docs.python.org/3/library/os.html")
        (lib "os.path" "https://docs.python.org/3/library/os.html#module-os.path")
        (lib "random" "https://docs.python.org/3/library/random.html")
        (lib "re" "https://docs.python.org/3/library/re.html")
        (lib "shlex" "https://docs.python.org/3/library/shlex.html")
        (lib "socket" "https://docs.python.org/3/library/socket.html")
        (lib "struct" "https://docs.python.org/3/library/struct.html")
        (lib "urllib" "https://docs.python.org/3/library/urllib.html")))

(define (lib-desc name . why)
  (or (for/first ([l (in-list LIB-INDEX*)]
                  #:when (string=? name (lib-name l)))
        l)
      (begin
        (printf "WARNING no URL for library ~a, please add to `lib-index*` in `main.rkt`~n" name)
        (lib name #f))))

(define DLS-2014-BENCHMARK-NAMES '())
(define POPL-2017-BENCHMARK-NAMES '())
(define EXHAUSTIVE-BENCHMARKS '())
(define SAMPLE-BENCHMARKS '())

(define NUM-EXHAUSTIVE-BENCHMARKS (length EXHAUSTIVE-BENCHMARKS))

(define MAIN-BENCHMARKS (append EXHAUSTIVE-BENCHMARKS SAMPLE-BENCHMARKS))
(define NUM-MAIN-BENCHMARKS (length MAIN-BENCHMARKS))
(define RT (get-ratios-table MAIN-BENCHMARKS))

