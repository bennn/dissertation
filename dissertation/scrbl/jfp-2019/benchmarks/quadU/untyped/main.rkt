#lang racket/base

;; -----------------------------------------------------------------------------

(require
 require-typed-check
 "../base/core.rkt"
 (only-in typed/racket/class new send))

(require (only-in "world.rkt"
  world:allow-hyphenated-last-word-in-paragraph ;Boolean)
  world:quality-default ;(Parameterof Index))
  world:draft-quality ;Index)
))
(require (only-in "quad-main.rkt"
  typeset; (-> Quad Quad))
))
(require (only-in "quick-sample.rkt"
  quick-sample ;(-> Quad))
))
(require (only-in "render.rkt"
  pdf-renderer%
  ;  (Class
  ;    [render-to-file (Quad Path-String -> Void)]
  ;    [render-element (Quad -> Any)]
  ;    [render-page ((Listof Quad) -> Void)]
  ;    [render-word (Quad -> Any)]
  ;    [render (-> Quad Any)]
  ;    [finalize (-> Any Any)]
  ;    [setup (-> Quad Quad)]))
))

;; =============================================================================

(parameterize ([world:quality-default world:draft-quality])
  (time
    (begin
      (define to (typeset (quick-sample)))
      (send (new pdf-renderer%) render-to-file to "./output.pdf")
      (void))))
