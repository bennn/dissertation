#lang typed/racket/base #:locally-defensive

(provide make-timeline)

(require typed/racket/class typed/racket/draw typed/pict)

(require/typed ppict/2
  [#:opaque Coord refpoint-placer?]
  [coord (-> Real Real Symbol Coord)])

(require/typed "ppict-simple.rkt"
  [ppict (-> Pict (Listof (Pairof Coord Pict)) Pict)])

(require/typed pict-abbrevs
  [add-rounded-border
    (->* [Pict]
         [#:radius Real #:y-margin Real #:frame-width Real #:frame-color String]
         Pict)])

(define-type Pict pict)

(: make-timeline-bar (-> Real Real (U #f String) (-> String Pict) Pict))
(define (make-timeline-bar w h label tcodesize)
  (define color (if label "light gray" "white"))
  (define bar (filled-rounded-rectangle w h 1 #:color color #:draw-border? #f))
  (ppict
    bar
    (list (cons (coord 2/100 48/100 'lc) (tcodesize (or label ".")))
          (cons (coord 98/100 48/100 'rc) (tcodesize ".")))))

(: make-timeline-span : (-> Real String (-> String Pict) (Instance Color%) Pict))
(define (make-timeline-span h label ct timeline-span-color)
  (define span-radius 7)
  (define bar-pict (filled-rounded-rectangle 25 h span-radius #:color timeline-span-color #:draw-border? #f))
  (define label-pict (ct label))
  (ht-append 10 bar-pict label-pict))

(: make-timeline (-> Real Real (Instance Color%) (-> String Pict) (-> String Pict) Pict))
(define (make-timeline w h timeline-span-color ct tcodesize)
  (let* ((month*
           '("Nov" "Dec" "Jan'20" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"))
         (bar-h
           (/ h (* 2 (length month*))))
         (make-span-h
           (lambda ((i : Real)) (* i bar-h)))
         (make-span-%
           (lambda ((i : Real)) (/ (make-span-h i) h)))
         (base
           (for/fold : Pict
                     ((acc : Pict (blank)))
                     ((m : String (in-list month*)))
             (vl-append 0 acc
                        (make-timeline-bar w bar-h m tcodesize)
                        (make-timeline-bar w bar-h #f tcodesize))))
         (timeline
           (ppict
             base
             (list
               (cons (coord 14/100 0 'lt) (make-timeline-span (make-span-h 6) "model" ct timeline-span-color))
               (cons (coord 29/100 0 'lt) (make-timeline-span (make-span-h 12) "implementation" ct timeline-span-color))
               (cons (coord 44/100 (make-span-%  7) 'lt) (make-timeline-span (make-span-h 8) "evaluation" ct timeline-span-color))
               (cons (coord 59/100 (make-span-% 11) 'lt) (make-timeline-span (make-span-h 4) "paper" ct timeline-span-color))
               (cons (coord 74/100 (make-span-% 13) 'lt) (make-timeline-span (make-span-h 7) "dissertation" ct timeline-span-color))))))
    (add-rounded-border
      #:radius 5 #:y-margin 6 #:frame-width 3 #:frame-color "slategray"
      timeline)))

