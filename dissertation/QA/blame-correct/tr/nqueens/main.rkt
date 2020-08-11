#lang typed/racket/base #:transient
(require racket/list racket/set)

;;

(define-type (MV T) (Mutable-Vectorof T))

(: permutations (-> (Listof Integer) (Listof (Listof Integer))))
(define (permutations iterable)
  (define pool : (MV Integer) (list->vector iterable))
  (define n : Natural (vector-length pool))
  (define r : Natural n)
  (define indices : (MV Natural) (build-vector n (ann values (-> Natural Natural))))
  (define cycles : (MV Integer)
    (list->vector (reverse (range (+ (- n r) 1) (+ n 1)))))
  (define result : (Listof (Listof Integer))
    (list
      (for/list : (Listof Integer)
                ([i : Natural (in-vector indices)]
                 [_k (in-range 0 (+ 1 r))])
        (vector-ref pool i))))
  (define for-break : (Boxof Boolean) (box #f))
  (let loop : Void ()
    (when (< 0 n)
      (set-box! for-break #false)
      (for ((i : Natural (in-list (reverse (range r)))))
        (unless (unbox for-break)
          (vector-set! cycles i (sub1 (vector-ref cycles i)))
          (cond
            [(= 0 (vector-ref cycles i))
             (let ((final : Natural (vector-ref indices i)))
               (for ((k (in-range i (sub1 (vector-length indices)))))
                 (vector-set! indices k (vector-ref indices (+ k 1))))
               (vector-set! indices (sub1 (vector-length indices)) final))
             (vector-set! cycles i (- n i))]
            [else
             (define j : Integer (vector-ref cycles i))
             (let ([i-j (vector-ref indices (let ((jj (- j)))
                                              (if (< jj 0)
                                                (+ (vector-length indices) jj)
                                                jj)))]
                   [i-i (vector-ref indices i)])
               (vector-set! indices i i-j)
               (vector-set! indices (let ((jj (- j)))
                                      (if (< jj 0)
                                        (+ (vector-length indices) jj)
                                        jj)) i-i))
             (set! result
               (cons
                 (for/list : (Listof Integer)
                           ([i : Natural (in-vector indices)]
                            [_k (in-range 0 (+ 1 r))])
                   (vector-ref pool i))
                 result))
             (set-box! for-break #true)])))
      (when (unbox for-break)
        (loop))))
  result)

(: n_queens (-> Natural (Listof (Listof Integer))))
(define (n_queens queen_count)
  (define cols : (Listof Natural) (range queen_count))
  (define res : (Listof (Listof Integer)) '())
  (for ((vec (in-list (permutations cols))))
    (when (= queen_count
             (set-count
               (list->set
                 (for/list : (Listof Integer)
                          ((i (in-list cols)))
                   (+ (list-ref vec i) i))))
             (set-count
               (list->set
                 (for/list : (Listof Integer)
                          ((i (in-list cols)))
                   (- (list-ref vec i) i)))))
      (set! res (cons vec res))))
  res)

(define ITERATIONS 10)

(define (main)
  (for ((_ (in-range ITERATIONS)))
    (n_queens 8)))

(time (main))

