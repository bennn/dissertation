#lang racket/base

;; License: Apache-2.0

;; By Phil McGrath
;; <https://gist.github.com/LiberalArtist/4d0059f5af23043515a3cc74bd4928c2>

(require racket/draw
         pict
         racket/class
         racket/math
         racket/list
         racket/contract)

(module+ main
  (inset (lightbulb) 10))

(provide (contract-out
          [lightbulb
           (->* []
                [#:color (or/c string? (is-a?/c color%) (is-a?/c brush%))
                 #:base-color (or/c string? (is-a?/c color%))
                 #:border-color (or/c string? (is-a?/c color%))
                 #:tip-color (or/c string? (is-a?/c color%))
                 #:border-width (real-in 0 255)
                 #:bulb-radius (and/c rational? (not/c negative?))
                 #:stem-width-radians (and/c rational? (not/c negative?))
                 #:stem-height (and/c rational? (not/c negative?))
                 #:base-segments natural-number/c
                 #:base-segment-height (and/c rational? (not/c negative?))
                 #:base-segment-corner-radius real?
                 #:tip-ratio (and/c rational? (not/c negative?))]
                pict?)]
          ))

(define (lightbulb #:color [bulb-color "yellow"]
                   #:base-color [base-color (make-color 200 200 200)]
                   #:border-color [border-color (make-color 0 0 0)]
                   #:tip-color [tip-color border-color]
                   #:border-width [border-width 2.5]
                   #:bulb-radius [bulb-radius 50]
                   #:stem-width-radians [stem-width-radians (/ pi 4)]
                   #:stem-height [stem-height 15]
                   #:base-segments [base-segments 3]
                   #:base-segment-height [base-segment-height 9]
                   #:base-segment-corner-radius [base-segment-corner-radius 3]
                   #:tip-ratio [tip-ratio 5/12])
  (define-values [stem-width bulb-part]
    (stem-width+bulb-part-pict #:color bulb-color
                               #:border-color border-color
                               #:border-width border-width
                               #:bulb-radius bulb-radius
                               #:stem-width-radians stem-width-radians
                               #:stem-height stem-height))
  (define base
    (base-pict #:base-color base-color
               #:border-color border-color
               #:tip-color tip-color
               #:border-width border-width
               #:stem-width stem-width 
               #:base-segments base-segments
               #:base-segment-height base-segment-height
               #:base-segment-corner-radius base-segment-corner-radius
               #:tip-ratio tip-ratio))
  (vc-append bulb-part
             base))


(define (stem-width+bulb-part-pict
         #:color [bulb-color "yellow"]
         #:border-color [border-color (make-color 0 0 0)]
         #:border-width [border-width 2.5]
         #:bulb-radius [bulb-radius 50]
         #:stem-width-radians [stem-width-radians (/ pi 4)]
         #:stem-height [stem-height 15])
  (define-syntax-rule (with-methods obj #:methods [m ...] body ...)
    (let ([this obj])
      (with-method ([m (this m)] ...)
        body ... this)))
  (let*-values ([{left-θ right-θ}
                 (let ([6pm (* 3/2 pi)]
                       [half-gap (/ stem-width-radians 2)])
                   (values (- 6pm half-gap)
                           (+ 6pm half-gap)))]
                [{left-x right-x}
                 (let ([θ->x (λ (θ)
                               (+ bulb-radius (* bulb-radius (cos θ))))])
                   (values (θ->x left-θ)
                           (θ->x right-θ)))]
                [{stem-width}
                 (- right-x left-x)]
                [{diameter} (* 2 bulb-radius)]
                [{bottom-y} (+ diameter stem-height)]
                [{pth}
                 (with-methods
                  (new dc-path%) #:methods [arc line-to]
                  (arc 0 0 diameter diameter right-θ left-θ)
                  (line-to left-x bottom-y)
                  (line-to right-x bottom-y)
                  (line-to right-x (+ bulb-radius (- (* bulb-radius (sin right-θ))))))]
                [{lightbulb-pen} (make-pen #:width border-width
                                           #:color border-color)]
                [{lightbulb-brush} (if (is-a? bulb-color brush%)
                                       bulb-color
                                       (make-brush #:color bulb-color
                                                   #:style 'solid))])
    (values
     stem-width
     (dc (λ (dc dx dy)
           (with-methods
            dc #:methods [draw-path
                          get-brush get-pen set-brush set-pen]
            (define old-brush (get-brush))
            (define old-pen (get-pen))
            (set-brush lightbulb-brush)
            (set-pen lightbulb-pen)
            ;;;;;;;;
            (draw-path pth dx dy)
            ;;;;;;;;
            (set-brush old-brush)
            (set-pen old-pen)))
         diameter
         bottom-y))))


(define (base-pict #:base-color [base-color (make-color 200 200 200)]
                   #:border-color [border-color (make-color 0 0 0)]
                   #:tip-color [tip-color border-color]
                   #:border-width [border-width 2.5]
                   #:stem-width [stem-width (let-values ([{w _} (stem-width+bulb-part-pict)])
                                              w)]
                   #:base-segments [base-segments 3]
                   #:base-segment-height [base-segment-height 9]
                   #:base-segment-corner-radius [base-segment-corner-radius 3]
                   #:tip-ratio [tip-ratio 5/12])
  (define base-rect
    (filled-rounded-rectangle
     (+ stem-width
        base-segment-corner-radius)
     base-segment-height
     base-segment-corner-radius
     #:color base-color
     #:border-color border-color
     #:border-width border-width))
  (define tip-diameter
    (* tip-ratio stem-width))
  (define tip
    (disk tip-diameter
          #:color tip-color
          #:border-color border-color
          #:border-width border-width))
  (define base-rect-stack
    (apply vc-append
           (make-list base-segments base-rect)))
  (define tip-radius
    (/ tip-diameter 2))
  (panorama
   (pin-under base-rect-stack
              (- (/ (pict-width base-rect-stack) 2)
                 tip-radius)
              (- (pict-height base-rect-stack)
                 tip-radius)
              tip)))
