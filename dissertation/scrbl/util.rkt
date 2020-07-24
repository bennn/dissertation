#lang racket/base

(provide
  thesis-max-page-width
  thesis-max-page-height
  overhead-plots-per-page
  overhead-y-sep
  overhead-plot-y

  SAMPLE-RATE
  NUM-SAMPLE-TRIALS

  glob-first
  make-lang-sloc

  log-bg-thesis-info
  log-bg-thesis-warning
  log-bg-thesis-error)

(require
  file/glob
  gtp-util
  gtp-util/system
  racket/string
  (only-in racket/list last)
  racket/match)

;; -----------------------------------------------------------------------------

(define thesis-max-page-width 440)
(define thesis-max-page-height 580)

(define overhead-plots-per-page 7)
(define overhead-y-sep 10)
(define overhead-plot-y
  (/ (- thesis-max-page-height (* (- overhead-plots-per-page 1) overhead-y-sep))
     overhead-plots-per-page))

(define-logger bg-thesis)

(define SAMPLE-RATE 10)
(define NUM-SAMPLE-TRIALS 10)

(define (glob-first str)
  (match (glob str)
   [(cons r '())
    r]
   ['()
    (raise-user-error 'glob-first "No results for glob '~a'" str)]
   [r*
    (printf "WARNING: ambiguous results for glob '~a'. Returning the first.~n" str)
    (car r*)]))

(define (make-lang-sloc lang-str)
  (define err-name (string->symbol (format "~a-sloc" lang-str)))
  (lambda (ps)
    (define ps-str (path-string->string ps))
    (define arg* (list "--details" "--wide" ps-str))
    (define all-output (shell "sloccount" arg*))
    (define cmd-str (string-join (cons "sloccount" arg*)))
    (define col* (string-split (last (string-split all-output "\n"))))
    (define-values [loc lang _src sloccount-ps]
      (if (= 4 (length col*))
        (apply values col*)
        (raise-user-error err-name
          "failed to parse output of 'sloccount ~a'~n  full output: ~a"
          ps
          all-output)))
    (unless (string=? lang lang-str)
      (raise-user-error err-name
        "expected SLOCCOUNT to return '~a' language, got '~a' instead.~n  original command: ~a"
        lang-str lang cmd-str))
    (unless (string=? sloccount-ps ps-str)
      (raise-user-error err-name
        "expected SLOCCOUNT to report path string '~a', got '~a' instead.~nSomething is very wrong!"
        ps-str
        sloccount-ps))
    (define n (string->number loc))
    (unless (exact-nonnegative-integer? n)
      (raise-user-error err-name
        "expected SLOCCOUNT to report a natural number of lines, got '~a'.~nSomething is very wrong."
        loc))
    n))

