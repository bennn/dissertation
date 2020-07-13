#lang racket/base

(provide
  thesis-max-page-width
  thesis-max-page-height
  overhead-plots-per-page
  overhead-y-sep
  overhead-plot-y

  SAMPLE-RATE
  NUM-SAMPLE-TRIALS

  log-bg-thesis-info
  log-bg-thesis-warning
  log-bg-thesis-error)

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
