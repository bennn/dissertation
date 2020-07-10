#lang at-exp racket/base

(provide
  STATIC-INFO-TITLE*
  MAIN-BENCHMARKS
  EXHAUSTIVE-BENCHMARKS VALIDATE-BENCHMARKS SAMPLE-BENCHMARKS
  NUM-MAIN-BENCHMARKS
  MAX-OVERHEAD
  NUM-ITERATIONS
  PYTHON
  SAMPLE-RATE
  NUM-SAMPLE-TRIALS
  NUM-EXHAUSTIVE-BENCHMARKS
  DLS-2014-BENCHMARK-NAMES
  DLS-2017-BENCHMARK-NAMES
  POPL-2017-BENCHMARK-NAMES
  render-static-information
  render-exact-runtime-plot*
  exact-runtime-category
  lib-desc
  u/p-ratio
  t/u-ratio
  t/p-ratio

  get-ratios-table
  ratios-table-row
  ratios-row-retic/python
  ratios-row-typed/retic
  render-ratios-table
)

(require
  (only-in scribble/base bold centered tabular hspace tt linebreak)
  file/glob
  gtp-util
  gtp-util/system
  gtp-plot/performance-info
  gtp-plot/reticulated-info
  json
  pict
  racket/format
  racket/list
  racket/set
  racket/string
  racket/path
  racket/runtime-path
  scribble-abbrevs
  with-cache
  (for-syntax racket/base syntax/parse))

;; -----------------------------------------------------------------------------

(define-runtime-path PEPM-HOME ".")
(define benchmark-dir (build-path PEPM-HOME "benchmarks"))
(define data-dir (build-path PEPM-HOME "data"))
(define karst-dir data-dir)
(define cache-dir (build-path PEPM-HOME "with-cache"))

(define NUM-ITERATIONS 40)
(define PYTHON "Python 3.4.3")
(define SAMPLE-RATE 10)
(define NUM-SAMPLE-TRIALS 10)
(define MAX-OVERHEAD 10)

(define TYPED "typed")
(define SAMPLE "sample")
(define SAMPLE-FILE-GLOB "sample*.tab")

(define u/p-ratio
  "retic/python ratio")

(define t/u-ratio
  "typed/retic ratio")

(define t/p-ratio
  "typed/python ratio")

(define (->karst-data sym)
  (define karst-path
    (path-add-extension (build-path karst-dir (symbol->string sym)) "_tab.gz"))
  (and (file-exists? karst-path) karst-path))

(define (->sample-data sym)
  (define prefix
    (build-path karst-dir SAMPLE (symbol->string sym)))
  (and (directory-exists? prefix)
       (glob (build-path prefix SAMPLE-FILE-GLOB))))

(define (->dataset x)
  ;; get Karst data
  (raise-user-error '->dataset "not implemented"))

(define RATIOS-TITLE
  (list "Benchmark" "retic/python" "typed/retic" "typed/python"))

(define (render-ratios-table row*)
  (centered
    (tabular
      #:sep (hspace 2)
      #:style 'block
      #:row-properties '(bottom-border 1)
      #:column-properties '(left right right right)
      (list* RATIOS-TITLE
             (map cdr row*)))))

(define (get-ratios-table name*)
  (parameterize ([*current-cache-directory* cache-dir]
                 [*current-cache-keys* (list (λ () name*))]
                 [*with-cache-fasl?* #f])
    (with-cache (cachefile "ratios-table.rktd")
      (λ ()
        (for/list ([name (in-list name*)])
          (render-ratios-row (benchmark-name->performance-info name)))))))

(define (ratios-table-row r* sym)
  (or
    (for/or ([r (in-list r*)]
             #:when (eq? (car r) sym))
      r)
    (raise-argument-error 'ratios-table-row "benchmark name" 1 r* sym)))

(define (ratios-row-retic/python r)
  (caddr r))

(define (ratios-row-typed/retic r)
  (cadddr r))

(define (ratios-row-typed/python r)
  (cadddr (cdr r)))

(define (render-ratios-row pi)
  (define n (performance-info->name pi))
  (list n
        (tt (symbol->string n))
        (rnd (untyped/baseline-ratio pi))
        (rnd (typed/untyped-ratio pi))
        (rnd (typed/baseline-ratio pi))))

(define (percent-slower-than-typed pre-bm)
  (define pi (make-reticulated-info (->dataset pre-bm)))
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

(define STATIC-INFO-TITLE*
  (list "Benchmark" (bold "N") "SLOC" "modules"
        "functions" "classes" "methods"))

;; TODO add cache
(define (render-static-information name*)
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
                   (map render-static-row name*))))))))

(define (render-static-row bm-name)
  (define src (benchmark-name->directory bm-name))
  (define py (benchmark-name->python-info bm-name))
  (define num-fun (python-info->num-functions py))
  (define num-meth (python-info->num-methods py))
  (define num-class (python-info->num-classes py))
  (cons
    (tt (symbol->string bm-name))
    (map ~a (list
      (+ num-fun num-meth num-class)
      (benchmark->sloc src)
      (python-info->num-modules py)
      (hide-zero num-fun)
      (hide-zero num-class)
      (hide-zero num-meth)))))

(define (hide-zero n)
  (if (zero? n) "-" n))

(define (benchmark-name->directory bm-name)
  (define pp (build-path benchmark-dir (symbol->string bm-name) TYPED))
  (if (directory-exists? pp)
    pp
    (raise-argument-error 'benchmark-name->directory "directory-exists?" pp)))

(define (benchmark-name->data-directory bm-name)
  (define pp (build-path data-dir (symbol->string bm-name)))
  (if (directory-exists? pp)
    pp
    (raise-argument-error 'benchmark-name->data-directory "directory-exists?" pp)))

(define (benchmark->sloc bm-dir)
  (for/sum ((py (in-glob (build-path bm-dir "*py"))))
    (python-sloc py)))

(define *python-exe* (make-parameter "python3"))
(define-runtime-path EXPLODE.PY "explode_module.py")

(struct python-info (
  name    ;; Symbol
  module* ;; (Listof module-info)
) #:prefab )

(struct module-info [
  name ;; Symbol
  function* ;; (Listof function-info)
  class* ;; (Listof class-info)
] #:prefab )

(struct function-info [
  name ;; Symbol
  dom* ;; (Listof field-info)
  cod ;; (U String #f)
] #:prefab )

(struct class-info [
  name ;; Symbol
  field* ;; (U #f (Listof field-info))
  method* ;; (Listof function-info)
] #:prefab )

(struct field-info [
  name ;; Symbol
  type ;; (U String #f)
] #:prefab )

(define (python-path? ps)
  (and (path-string? ps)
       (equal? (path-get-extension ps) #".py")))

(define (python-sloc ps)
  (define ps-str (path-string->string ps))
  (define arg* (list "--details" "--wide" ps-str))
  (define all-output (shell "sloccount" arg*))
  (define cmd-str (string-join (cons "sloccount" arg*)))
  (define col* (string-split (last (string-split all-output "\n"))))
  (define-values [loc lang _src sloccount-ps]
    (if (= 4 (length col*))
      (apply values col*)
      (raise-user-error 'python-sloc
        "failed to parse output of 'sloccount ~a'~n  full output: ~a"
        ps
        all-output)))
  (unless (string=? lang "python")
    (raise-user-error 'python-sloc
      "expected SLOCCOUNT to return 'python' language, got '~a' instead.~n  original command: ~a"
      lang cmd-str))
  (unless (string=? sloccount-ps ps-str)
    (raise-user-error 'python-sloc
      "expected SLOCCOUNT to report path string '~a', got '~a' instead.~nSomething is very wrong!"
      ps-str
      sloccount-ps))
  (define n (string->number loc))
  (unless (exact-nonnegative-integer? n)
    (raise-user-error 'python-sloc
      "expected SLOCCOUNT to report a natural number of lines, got '~a'.~nSomething is very wrong."
      loc))
  n)

(define (benchmark-name->python-info bm-name)
  (define ps (benchmark-name->directory bm-name))
  (define m* (glob (build-path ps "*.py")))
  (define filename+md5* (for/list ([m (in-list m*)]) (list (path-string->string (file-name-from-path m)) (md5sum m))))
  (if (null? m*)
    (raise-user-error 'directory->python-info "directory ~a has no Python files" ps)
    (python-info bm-name (map path-string->module-info m*))))

(define (benchmark-name->performance-info bm-name)
  (define data-dir (benchmark-name->data-directory bm-name))
  (make-reticulated-info data-dir))

(define (path-string->module-info ps)
  (define py-json (path-string->exploded-module ps))
  (module-json->module-info py-json))

(define (module-json->module-info j)
  (module-info (json-name j)
               (map function-json->function-info (hash-ref j 'function))
               (map class-json->class-info (hash-ref j 'class))))

(define (class-json->class-info j)
  (class-info (json-name j)
              (let ([f (hash-ref j 'field #f)])
                (if f (map field-json->field-info f) #f))
              (map function-json->function-info (hash-ref j 'method))))

(define (function-json->function-info j)
  (function-info (json-name j)
                 (map field-json->field-info (hash-ref j 'dom))
                 (hash-ref j 'cod)))

(define (field-json->field-info j)
  (field-info (json-name j)
              (hash-ref j 'type)))

(define (json-name j)
  (string->symbol (hash-ref j 'name)))

(define (path-string->exploded-module ps)
  (define py (*python-exe*))
  (check-python-exe! py)
  (check-python-file! EXPLODE.PY)
  (string->jsexpr (shell py (list EXPLODE.PY (path-string->string ps)))))

;; (-> string? void?)
;; Assert that the argument refers to a Python with the right version
(define (check-python-exe! str)
  (define v-str (shell str "--version"))
  (unless (valid-python-version? v-str)
    (raise-argument-error 'check-python-exe! (format "Python 3.4 (given '~a')" v-str) str))
  (void))

(define (check-python-file! fn)
  (unless (file-exists? fn)
    (raise-user-error 'check-python-file! "could not find Python file '~a'" fn))
  (void))

;; TODO can relax to "Python 3" ?
(define (valid-python-version? str)
  (regexp-match? #rx"^Python 3.4" str))

(define (python-info->module* py)
  (for/list ([mi (in-list (python-info-module* py))])
    (string-append (symbol->string (module-info-name mi)) ".py")))

(define (python-info->num-modules py)
  (length (python-info->module* py)))

(define (python-info->function* py)
  (for*/list ([mi (in-list (python-info-module* py))]
              [fi (in-list (module-info-function* mi))])
    (symbol->string (function-info-name fi))))

(define (python-info->num-functions py)
  (length (python-info->function* py)))

(define (python-info->class* py)
  (for*/list ([mi (in-list (python-info-module* py))]
              [fi (in-list (module-info-class* mi))])
    (symbol->string (class-info-name fi))))

(define (python-info->num-classes py)
  (length (python-info->class* py)))

(define (python-info->method* py)
  (for*/list ([mi (in-list (python-info-module* py))]
              [ci (in-list (module-info-class* mi))]
              [fi (in-list (class-info-method* ci))])
    (symbol->string (function-info-name fi))))

(define (python-info->num-methods py)
  (length (python-info->method* py)))

(define (python-info->domain* py)
  (append*
    (for/list ([mi (in-list (python-info-module* py))])
      (append (for/list ([fi (in-list (module-info-function* mi))])
                (function-info-dom* fi))
              (for*/list ([ci (in-list (module-info-class* mi))]
                          [fi (in-list (class-info-method* ci))])
                (function-info-dom* fi))))))

(define (python-info->num-parameters py)
  (for/sum ([d (in-list (python-info->domain* py))])
    (length d)))

(define (python-info->return* py)
  (append*
    (for/list ([mi (in-list (python-info-module* py))])
      (append (for/list ([fi (in-list (module-info-function* mi))])
                (function-info-cod fi))
              (for*/list ([ci (in-list (module-info-class* mi))]
                          [fi (in-list (class-info-method* ci))])
                (function-info-cod fi))))))

(define (python-info->num-returns py)
  (length (python-info->return* py)))

(define (python-info->field* py)
  (define missing-field* (list (field-info 'missing-field #f)))
  (for*/list ([mi (in-list (python-info-module* py))]
              [ci (in-list (module-info-class* mi))]
              [f (in-list (or (class-info-field* ci)
                              (list (field-info (string->symbol (format "missing-fields:~a" (class-info-name ci))) #f))))])
    f))

(define (python-info->num-fields py)
  (length (python-info->field* py)))

(define (python-info->all-types py)
  (set-union
    (list->set (python-info->return* py))
    (for/set ([f (in-list (python-info->field* py))])
      (field-info-type f))
    (for*/set ([d (in-list (python-info->domain* py))]
               [f (in-list d)])
      (field-info-type f))))

(define (python-info->num-types py)
  (+ (python-info->num-functions py)
     (python-info->num-classes py)
     (python-info->num-methods py)))

;; -----------------------------------------------------------------------------

(define DLS-2014-BENCHMARK-NAMES '(
  futen
  http2
  slowSHA
))
(define POPL-2017-BENCHMARK-NAMES '(
  call_method
  call_simple
  chaos
  fannkuch
  float
  go
  meteor
  nbody
  nqueens
  pidigits
  pystone
  spectralnorm
))
(define DLS-2017-BENCHMARK-NAMES '(
  Espionage
  ;; Evolution
  PythonFlow
  take5
  sample_fsm
  aespython
  stats
))

(define MAIN-BENCHMARKS
  (append DLS-2014-BENCHMARK-NAMES POPL-2017-BENCHMARK-NAMES DLS-2017-BENCHMARK-NAMES))
(define NUM-MAIN-BENCHMARKS (length MAIN-BENCHMARKS))

(define-values [EXHAUSTIVE-BENCHMARKS VALIDATE-BENCHMARKS SAMPLE-BENCHMARKS]
  (let ([name* MAIN-BENCHMARKS])
    (define e* (filter ->karst-data name*))
    (define s* (filter (λ (n) (and (not (memq n '(Evolution take5)))
                                   (->sample-data n)
                                   #t))
                       name*))
    (define-values [v* r*] (partition (λ (n) (memq n e*)) s*))
    (values e* v* r*)))

(define NUM-EXHAUSTIVE-BENCHMARKS (length EXHAUSTIVE-BENCHMARKS))


