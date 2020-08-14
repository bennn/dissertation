#lang typed/racket/base #:transient
(require racket/match)

(define NDIGITS : Natural 5000)

(define-type IIII (List Integer Integer Integer Integer))

(: gen_x (-> Integer IIII))
(define (gen_x k)
  (list k (+ (* 4 k) 2) 0 (+ (* 2 k) 1)))

(: compose (-> IIII IIII IIII))
(define (compose a b)
  (match-define (list aq ar as_ at) a)
  (match-define (list bq br bs bt) b)
  (list (* aq bq)
        (+ (* aq br) (* ar bt))
        (+ (* as_ bq) (* at bs))
        (+ (* as_ br) (* at bt))))

(: extract (-> IIII Integer Integer))
(define (extract z j)
  (match-define (list q r s t) z)
  (quotient (+ (* q j) r) (+ (* s j) t)))

(: pi_digits (-> Integer (Listof Integer)))
(define (pi_digits limit)
   (define z : IIII (list 1 0 0 1))
   (define x 1)
   (define result : (Listof Integer) '())
   (let loop ()
     (when (<= x limit)
       (define y (extract z 3))
       (let loop ()
         (when (not (= y (extract z 4)))
           (set! z (compose z (gen_x x)))
           (set! y (extract z 3))
           (loop)))
       (set! z (compose (list 10 (* -10 y) 0 1) z))
       (set! x (+ x 1))
       (set! result (append result (list y)))
       (loop)))
   result)

(: calc_ndigits (-> Integer (Listof Integer)))
(define (calc_ndigits n)
  (pi_digits n))

(define (main)
  (calc_ndigits NDIGITS))

(time (void (main)))
;; goal = 

