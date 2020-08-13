#lang typed/racket/base #:transient

(provide
  *TIMESTAMP
  *MOVES
  square%)

(require typed/racket/class racket/list)
(require "constants.rkt")

(define *TIMESTAMP (box TIMESTAMP))
(define *MOVES (box MOVES))

(: to_pos (-> Integer Integer Integer))
(define (to_pos x y)
  (+ (* y SIZE) x))

(define square%
  (class object%
    (super-new)
    (init [init-board : Board]
          [init-pos : Integer])
    (field [board : Board init-board]
           [pos : Integer init-pos]
           [timestamp : Integer (unbox *TIMESTAMP)]
           [removestamp : Integer (unbox *TIMESTAMP)]
           [zobrist_strings : (Listof Integer) (for/list ((i (in-range 3)))
                                                 (random 9223372)
                                                 #;(error 'random "9223372036854775807"))]
           [color : Integer 0]
           [reference : Square this]
           [ledges : Integer 0]
           [temp_ledges : Integer 0]
           [used : Boolean #false]
           [neighbours : (Listof Square) '()])

    (define/public (set_neighbours) : Void
      (define x (modulo (get-field pos this) SIZE))
      (define y (quotient (get-field pos this) SIZE))
      (set-field! neighbours this '())
      (for ((dx (in-list '(-1 1 0 0)))
            (dy (in-list '(0 0 -1 1))))
        (define newx (+ x dx))
        (define newy (+ y dy))
        (when (and (<= 0 newx)
                   (< newx SIZE)
                   (<= 0 newy)
                   (< newy SIZE))
          (set-field! neighbours this
                      (append
                       (get-field neighbours this)
                       (list
                         (list-ref
                          (get-field squares (get-field board this))
                          (to_pos newx newy))))))))

    (define/public (move (new-color : Integer)) : Void
      (set-box! *TIMESTAMP (+ 1 (unbox *TIMESTAMP)))
      (set-box! *MOVES (+ 1 (unbox *MOVES)))
      (send (get-field zobrist (get-field board this)) update this new-color)
      (set-field! color this new-color)
      (set-field! reference this this)
      (set-field! ledges this 0)
      (set-field! used this #true)
      (for ((neighbour (in-list (get-field neighbours this))))
        (define neighcolor (get-field color neighbour))
        (if (= EMPTY neighcolor)
          (set-field! ledges this (+ 1 (get-field ledges this)))
          (let ((neighbour_ref (send neighbour find #true)))
            (if (= neighcolor new-color)
              (begin
                (unless (= (get-field pos (get-field reference neighbour_ref))
                           (get-field pos this))
                  (set-field! ledges this (+ (get-field ledges this)
                                             (get-field ledges neighbour_ref)))
                  (set-field! reference neighbour_ref this))
                (set-field! ledges this (- (get-field ledges this) 1)))
              (begin
                (set-field! ledges neighbour_ref (- (get-field ledges neighbour_ref) 1))
                (when (= 0 (get-field ledges neighbour_ref))
                  (send neighbour remove neighbour_ref #true)))))))
      (send (get-field zobrist (get-field board this)) add))

    (define/public (remove (reference : Square) (update : Boolean)) : Void
      (send (get-field zobrist (get-field board this)) update this EMPTY)
      (set-field! removestamp this (unbox *TIMESTAMP))
      (when update
        (set-field! color this EMPTY)
        (send (get-field emptyset (get-field board this)) add (get-field pos this)))
      (for ((neighbour (in-list (get-field neighbours this))))
        (when (and (not (= (get-field color neighbour) EMPTY))
                   (not (= (get-field removestamp neighbour) (unbox *TIMESTAMP))))
          (define neighbour_ref (send neighbour find update))
          (if (= (get-field pos neighbour_ref)
                 (get-field pos reference))
            (send neighbour remove reference update)
            (when update
              (set-field! ledges neighbour_ref (+ (get-field ledges neighbour_ref) 1)))))))

    (define/public (find (update : Boolean)) : Square
      (define ref (get-field reference this))
      (unless (= (get-field pos ref)
                 (get-field pos this))
        (set! ref (send ref find update))
        (when update
          (set-field! reference this ref)))
      ref)))

