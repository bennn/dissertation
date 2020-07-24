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

  log-bg-thesis-info
  log-bg-thesis-warning
  log-bg-thesis-error)

(require
  file/glob
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

