#lang typed/racket/base #:transient

(require racket/vector)

;; Copied from retic_performance benchmarks
;;
;; from ....
;;# The Computer Language Benchmarks Game
;;# http://shootout.alioth.debian.org/
;;#
;;# contributed by Sokolov Yura
;;# modified by Tupteq

(define-type (MV T) (Mutable-Vectorof T))

(: fannkuch (-> Natural Integer))
(define (fannkuch n)
  (define count : (MV Natural) (build-vector n (lambda ((k : Natural)) (+ k 1))))
  (define max_flips 0)
  (define m (- n 1))
  (define r : (Boxof Natural) (box n))
  (define check 0)
  (define perm1 : (MV Natural) (build-vector n (ann values (-> Natural Natural))))
  (define perm : (MV Natural) (build-vector n (ann values (-> Natural Natural))))
  (: perm1_ins (-> Natural Natural Void))
  (define (perm1_ins index elem)
    (define len : Natural (+ 1 (vector-length perm1)))
    (set! perm1
      (for/vector : (MV Natural)
                  #:length len
                  ((k (in-range len)))
        (cond
          [(< k index)
           (vector-ref perm1 k)]
          [(= k index)
           elem]
          [else
           (vector-ref perm1 (- k 1))]))))
  (: perm1_pop (-> Integer Natural))
  (define (perm1_pop index)
    (: elem Natural)
    (define elem (vector-ref perm1 index))
    (: len Natural)
    (define len (assert (- (vector-length perm1) 1) exact-nonnegative-integer?))
    (set! perm1
      (for/vector : (MV Natural)
                  #:length len
                  ((k : Natural (in-range len)))
        (cond
          [(< k index)
           (vector-ref perm1 k)]
          [else
           (vector-ref perm1 (+ k 1))])))
    elem)
  (let/ec return : Integer
    (let loop : Void ()

      (when (< check 30)
        (set! check (+ check 1)))

      (let loop : Void ()
        (let ((v : Natural (unbox r)))
          (when (< 1 v)
            (vector-set! count (- v 1) v)
            (set-box! r (assert (- v 1) exact-nonnegative-integer?))
            (loop))))

      (when (and (not (= (vector-ref perm1 0) 0))
                 (not (= (vector-ref perm1 m) m)))
        (set! perm (vector-copy perm1))
        (define flips_count 0)
        (let loop : Void ()
          (let ((k : Natural (vector-ref perm 0)))
            (when (< 0 k)
              (define to-flip : (MV Natural) (vector-take perm (+ k 1)))
              (for ((i (in-range (+ k 1))))
                (vector-set! perm i (vector-ref to-flip (- k i))))
              (set! flips_count (+ flips_count 1))
              (loop))))

        (when (> flips_count max_flips)
          (set! max_flips flips_count)))

      (let loop ()
        (let ((v (unbox r)))
          (cond
            [(= v n)
             (return max_flips)]
            [else
              (perm1_ins v (perm1_pop 0))
              (vector-set! count v (assert (- (vector-ref count v) 1) exact-nonnegative-integer?))
              (cond
                [(> (vector-ref count v) 0)
                 (void)]
                [else
                  (set-box! r (+ v 1))
                  (loop)])])))
      (loop))
    (ann -1 Integer)))

(define DEFAULT_ARG 10)

(define (main)
  ;; expected result = 38
  (fannkuch DEFAULT_ARG))

(time (main))

