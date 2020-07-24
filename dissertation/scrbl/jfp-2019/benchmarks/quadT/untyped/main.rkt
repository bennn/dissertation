#lang racket/base

;; AKA quick-test.rkt

;; -----------------------------------------------------------------------------

(require
 (only-in typed/racket/class new send)
)
(require (only-in "world.rkt"
  world:allow-hyphenated-last-word-in-paragraph
  world:quality-default
  world:draft-quality))
(require (only-in "quad-main.rkt"
  typeset))
(require (only-in "quick-sample.rkt"
  quick-sample))
(require (only-in "render.rkt"
  pdf-renderer%))

;; =============================================================================

(parameterize ([world:quality-default world:draft-quality])
  (time
    (begin
      (define to (typeset (quick-sample)))
      (send (new pdf-renderer%) render-to-file to "./output.pdf")
      (void))))
