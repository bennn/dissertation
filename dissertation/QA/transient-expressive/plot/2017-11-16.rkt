#lang racket 

(require plot) 
;; (plot-new-window? #true) ;; if #t, no performance trouble

;; added by ben g
(require racket/gui)
(define f (new frame% [parent #f] [label "3D Graph"] [min-width 400] [min-height 400]))
(define ec (new editor-canvas% [parent f]))
(define t (new text%))
(send ec set-editor t)

(require (only-in plot/utils bounds->intervals linear-seq)) 
(define (norm2 x y) (exp (* -1/2 (+ (sqr (- x 5)) (sqr y))))) 
(define x-ivls (bounds->intervals (linear-seq 2 8 16))) 
(define y-ivls (bounds->intervals (linear-seq -5 5 16))) 
(define x-mids (linear-seq 2 8 15 #:start? #f #:end? #f)) 
(define y-mids (linear-seq -5 5 15 #:start? #f #:end? #f)) 
(send t insert
(plot3d (rectangles3d (append* 
(for/list ([y-ivl (in-list y-ivls)] 
[y (in-list y-mids)]) 
(for/list ([x-ivl (in-list x-ivls)] 
[x (in-list x-mids)]) 
(define z (norm2 x y)) 
(vector x-ivl y-ivl (ivl 0 z))))) 
#:alpha 3/4 
#:label "Appx. 2D Normal")))

(send f show #t)
