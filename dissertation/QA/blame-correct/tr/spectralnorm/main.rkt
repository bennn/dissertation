#lang typed/racket/base #:transient
(require racket/match racket/list racket/flonum)

(: eval_A (-> Flonum Flonum Flonum))
(define (eval_A i j)
  (fl/ 1.0
    (fl+ (fltruncate (fl/ (fl* (fl+ i j) (fl+ i (fl+ j 1.0))) 2.0))
         (fl+ i 1.0))))

(: eval_times_u (-> (-> (Pairof Flonum (Listof Flonum)) Flonum) (Listof Flonum) (Listof Flonum)))
(define (eval_times_u func u)
  (for/list ((i (in-range (length u))))
    (func (cons (->fl i) u))))

(: eval_AtA_times_u (-> (Listof Flonum) (Listof Flonum)))
(define (eval_AtA_times_u u)
  (eval_times_u part_At_times_u (eval_times_u part_A_times_u u)))

(: part_A_times_u (-> (Pairof Flonum (Listof Flonum)) Flonum))
(define (part_A_times_u i_u)
  (match-define (cons i u) i_u)
  (define partial_sum : Flonum 0.0)
  (for ((j (in-naturals))
        (u_j (in-list u)))
    (set! partial_sum (fl+ partial_sum (fl* (eval_A i (->fl j)) u_j))))
  partial_sum)

(: part_At_times_u (-> (Pairof Flonum (Listof Flonum)) Flonum))
(define (part_At_times_u i_u)
  (match-define (cons i u) i_u)
  (define partial_sum : Flonum 0.0)
  (for ((j (in-naturals))
        (u_j (in-list u)))
    (set! partial_sum (fl+ partial_sum (fl* (eval_A (->fl j) i) u_j))))
  partial_sum)

(define DEFAULT_N 130)

(define (main)
  (define u : (Listof Flonum) (make-list DEFAULT_N 1.0))
  (define v : (Listof Flonum) (make-list DEFAULT_N 1.0))
  (for ((dummy (in-range 10)))
    (set! v (eval_AtA_times_u u))
    (set! u (eval_AtA_times_u v)))

  (define vBv : Flonum 0.0)
  (define vv : Flonum 0.0)

  (for ((ue (in-list u))
        (ve (in-list v)))
    (set! vBv (fl+ vBv (fl* ue ve)))
    (set! vv (fl+ vv (fl* ve ve))))
  ;;(displayln (list vBv vv))
  (void))

(time (main))

;; goal = 783450668.5017612 482526660.9208582


