#lang racket/base

(provide
  thesis-max-page-width
  thesis-max-page-height
  log-bg-thesis-info
  log-bg-thesis-warning
  log-bg-thesis-error)

;; -----------------------------------------------------------------------------

(define thesis-max-page-width 440)
(define thesis-max-page-height 580)

(define-logger bg-thesis)
