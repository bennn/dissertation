#lang typed/racket

(provide
  left
  right
  up
  down
  grid-ref
  grid-height
  grid-width
  show-grid
  array-set!
  build-array
)

(require
  "../base/cell-types.rkt"
  require-typed-check
  ;math/array ;; TODO it'd be nice to use this
)
(require/typed/check "cell.rkt"
  (char->cell% (-> Char Cell%))
  (void-cell% Cell%)
)

;; =============================================================================

(: array-set! (-> Grid Pos (Instance Cell%) Void))
(define (array-set! g p v)
  (vector-set! (vector-ref g (vector-ref p 0)) (vector-ref p 1) v))

(: build-array (-> Pos (-> Pos (Instance Cell%)) Grid))
(define (build-array p f)
  (for/vector : Grid
    ([x (in-range (vector-ref p 0))])
   (for/vector : (Vectorof (Instance Cell%))
                ([y (in-range (vector-ref p 1))])
    (f (vector (assert x index?) (assert y index?))))))
  ;(build-array p f)))

;; a Grid is a math/array Mutable-Array of cell%
;; (mutability is required for dungeon generation)

;; parses a list of strings into a grid, based on the printed representation
;; of each cell
(: parse-grid (-> (Listof String) Grid))
(define (parse-grid los)
  (for/vector : Grid
              ; #:shape (vector (length los)
              ;                (apply max (map string-length los)))
              ;#:fill (new void-cell%)
              ([s (in-list los)])
            (for/vector : (Vectorof (Instance Cell%))
               ([c (in-string s)]) ;: (Instance Cell%)
     (new (char->cell% c)))))

(: show-grid (-> Grid String))
(define (show-grid g)
  (with-output-to-string
    (lambda ()
      (for ([r (in-vector g)])
        (for ([c (in-vector r)])
          (display (send c show)))
        (newline)))))

(: grid-height (-> Grid Index))
(define (grid-height g)
  (vector-length g))
  ;(match-define (vector rows cols) (array-shape g))
  ;rows)

(: grid-width (-> Grid Index))
(define (grid-width g)
  (vector-length (vector-ref g 0)))
  ;(match-define (vector rows cols) (array-shape g))
  ;cols)

(: within-grid? (-> Grid Pos Boolean))
(define (within-grid? g pos)
  (and (<= 0 (vector-ref pos 0) (sub1 (grid-height g)))
       (<= 0 (vector-ref pos 1) (sub1 (grid-width  g)))))

(: grid-ref (-> Grid Pos (U #f (Instance Cell%))))
(define (grid-ref g pos)
  (and (within-grid? g pos)
       (vector-ref (vector-ref g (vector-ref pos 0)) (vector-ref pos 1))))

(: left (->* (Pos) (Index) Pos))
(define (left pos [n 1])
  (vector (vector-ref pos 0)
          (assert (max (- (vector-ref pos 1) n) 0) index?)))

(: right (->* (Pos) (Index) Pos))
(define (right pos [n 1])
  (vector (vector-ref pos 0)
          (assert (max (+ (vector-ref pos 1) n) 0) index?)))

(: up (->* (Pos) (Index) Pos))
(define (up pos [n 1])
  (vector (assert (max (- (vector-ref pos 0) n) 0) index?)
          (vector-ref pos 1)))

(: down (->* (Pos) (Index) Pos))
(define (down pos [n 1])
  (vector (assert (max (+ (vector-ref pos 0) n) 0) index?)
          (vector-ref pos 1)))


;(module+ test
;  (require typed/rackunit)
;
;  (: parse-and-show (-> (Listof String) String))
;  (define (parse-and-show los) (show-grid (parse-grid los)))
;  (: render-grid (-> (Listof String) String))
;  (define (render-grid g) (string-join g "\n" #:after-last "\n"))
;
;  (define g1
;    '(" "))
;  (check-equal? (parse-and-show g1) " \n")
;
;  (define g2
;    '("**********"
;      "*        *"
;      "*        *"
;      "*        *"
;      "**********"))
;  (check-equal? (parse-and-show g2) (render-grid g2))
;
;  (define g3 ; padding should work
;    '("**********"
;      "*        *"
;      "*        *"
;      "*        *"
;      "*****"))
;  (define g3*
;    '("**********"
;      "*        *"
;      "*        *"
;      "*        *"
;      "*****....."))
;  (check-equal? (parse-and-show g3) (render-grid g3*))
;
;  (define g2* (parse-grid g2))
;  (check-true (within-grid? g2* '#(0 0)))
;  (check-true (within-grid? g2* '#(0 1)))
;  (check-true (within-grid? g2* '#(1 0)))
;  (check-true (within-grid? g2* '#(4 4)))
;  (check-false (within-grid? g2* '#(0 10)))
;  (check-false (within-grid? g2* '#(5 0)))
;  (check-false (within-grid? g2* '#(5 10)))
;  )
