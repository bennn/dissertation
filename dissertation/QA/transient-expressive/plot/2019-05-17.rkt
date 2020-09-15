#lang racket

(require plot)

;; added by ben g
(require racket/gui)
(define f (new frame% [parent #f] [label "3D Graph"] [min-width 400] [min-height 400]))
(define ec (new editor-canvas% [parent f]))
(define t (new text%))
(send ec set-editor t)

(define ((make-current-value-renderer fn) snip event x y)
  (define overlays
    (and x y (eq? (send event get-event-type) 'motion)
         (list (vrule x #:style 'long-dash)
               (point-label (vector x (fn x)) #:anchor 'auto))))
  (send snip set-overlay-renderers overlays))

(define snip (plot-snip (function sin) #:x-min 0 #:x-max (* 2 pi) #:y-min -1.5 #:y-max 1.5))
(send snip set-mouse-event-callback (make-current-value-renderer sin))

(send t insert snip)
(send f show #t)

