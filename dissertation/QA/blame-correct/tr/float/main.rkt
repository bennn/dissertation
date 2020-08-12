#lang typed/racket/base #:transient
(require typed/racket/class typed/racket/flonum)

(define-type Point%
  (Class
    (field [x Flonum]
           [y Flonum]
           [z Flonum])
    (init [i Flonum])
    (normalize (-> Void))
    (maximize (-> Point Point))))

(define-type Point
  (Instance Point%))

(: point% Point%)
(define point%
  (class object%
    (init [i : Flonum])
    (super-new)
    (field [x : Flonum (flsin i)]
           [y : Flonum (fl* (flcos i) 3.0)]
           [z : Flonum (fl/ (fl* x x) 2.0)])

    (define/public (normalize)
      (define norm : Flonum (flsqrt (fl+ (fl* x x) (fl+ (fl* y y) (fl* z z)))))
      (set-field! x this (fl/ x norm))
      (set-field! y this (fl/ y norm))
      (set-field! z this (fl/ z norm)))

    (define/public (maximize other)
      (set-field! x this (flmax (get-field x this) (get-field x other)))
      (set-field! y this (flmax (get-field y this) (get-field y other)))
      (set-field! z this (flmax (get-field z this) (get-field z other)))
      this)))

(: maximize (-> (Listof Point) Point))
(define (maximize points)
  (for/fold : Point
            ((next : Point (car points)))
            ((p (in-list (cdr points))))
    (send next maximize p)))

(: benchmark (-> Integer Point))
(define (benchmark n)
  (define points
    (for/list : (Listof Point)
              ((i (in-range n)))
      (make-object point% (->fl i))))
  (for ((p (in-list points)))
    (send p normalize))
  (maximize points))

(define POINTS 200000)

(define (main)
  (benchmark POINTS)
  ;;(printf "(~a ~a ~a)~n" (get-field x ppp) (get-field y ppp) (get-field z ppp))
  (void))

(time (main))
;; goal = (0.8944271901453098 1.0 0.4472135954456972)

