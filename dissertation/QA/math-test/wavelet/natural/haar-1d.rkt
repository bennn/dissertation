#lang typed/racket

;; this file implements a 1-d linear Haar wavelet transform.
;; Tests live in another file, that compares the result of
;; this with a non-fast transform

(require math/array
         typed/rackunit
         plot
         racket/block)

(provide fast-haar-transform

         fast-haar-inverse-transform
         expand-array
         haar-reconstruct-step

         power-of-2?)

;; the basic idea here is that--for the Haar basis, at least--all you
;; care about is the sum of some subset of values. This means that after
;; you've extracted all of the high-frequency terms, you can compress
;; by a factor of two without affecting your ability to compute
;; the lower-frequency terms.

;; given a 1-d array whose length is a power of two, 
;; use high-freq and squeeze to build a linear-time transform:
(: fast-haar-transform ((Array Float) -> (Array Float)))
(define (fast-haar-transform arr)
  (unless (= 1 (vector-length (array-shape arr)))
    (raise-argument-error 'fast-haar-transform
                          "Array with one dimension"
                          0 arr))
  (unless (power-of-2? (vector-ref (array-shape arr) 0))
    (raise-argument-error 'fast-haar-transform
                          "Array whose length is a power of 2"
                          0 arr))
  (: orig-len Positive-Float)
  (define orig-len
    (match (exact->inexact (vector-ref (array-shape arr) 0))
      [(? positive-float? n) n]
      [else (error "internal error, length of array not positive-Float")]))
  (define sub-arrays
    (let loop : (Listof (Array Float)) ([arr arr])
      (: len Positive-Float)
      (define len
        (match (exact->inexact (vector-ref (array-shape arr) 0))
          [(? positive-float? n) n]
          [else (error "internal error, length of array not positive-Float")]))
      (: k Float)
      (define k (sqrt
                 (match (/ 1.0 (* orig-len (/ 2.0 len)))
                   [(? positive-float? n) n]
                   [else (error "internal error, expected positive-Float")])))
      (define hf (high-freq arr k))
      (cons hf
            (cond [(= len 2.0)
                   ;; must special-case vector zero
                   (list
                    (list->array
                     (list
                      (* k
                         (+ (array-ref arr (vector 0))
                            (array-ref arr (vector 1)))))))]
                  [else
                   (loop (squeeze arr))]))))
  (arrays-join (reverse sub-arrays)))

;; given a one-dimensional array of even length l and a multiplier k,
;; construct a new one-dimensional array of length l/2 where each term
;; is formed my multiplying k by the sum of a pointwise multiple of
;; pair in the array by the Haar Wavelet pair 1,-1
(: high-freq ((Array Float) Float -> (Array Float)))
(define (high-freq arr k)
  (define old-len (vector-ref (array-shape arr) 0))
  (: new-len Natural)
  (define new-len
    (match (/ old-len 2)
      [(? exact-integer? n) n]
      [else (raise-argument-error 'high-freq "vector of size divisible by 2"
                                  0 arr k)]))
  (for/array: #:shape (vector new-len)
    ([i (in-range new-len)])  : Float
    (ann (* k (- (array-ref arr (vector (* 2 i)))
                 (array-ref arr (vector (add1 (* 2 i))))))
         Float)))

;; given a one-dimensional array of even length l, construct a new
;; vector of length l/2 where each term is the sum of two consecutive
;; terms of the old array
(: squeeze ((Array Float) -> (Array Float)))
(define (squeeze arr)
  (define new-len (/ (vector-ref (array-shape arr) 0) 2))
  (for/array ([i (in-range new-len)]) : Float
    (+ (array-ref arr (vector (* 2 i)))
       (array-ref arr (vector (add1 (* 2 i)))))))

;; given a list of 1-d arrays, produce a new array containing
;; the elements from the arrays in order
(: arrays-join (All (T) ((Listof (Array T)) -> (Array T))))
(define (arrays-join as)
  (define elements
    (foldr (ann sequence-append
                ((Sequenceof T) (Sequenceof T) -> (Sequenceof T)))
           '()
           (map (ann in-array ((Array T) -> (Sequenceof T)))
                as)))
  (for/array ([v elements]) : T
    v))

(define-predicate nonnegative-Float? Nonnegative-Float)
(define-predicate positive-float? Positive-Float)



;;;;;;;
;;
;;  INVERSE TRANSFORM
;;
;;;;;;;

;; double the length of an array by duplicating each
;; element
(: expand-array (All (T) ((Array T) -> (Array T))))
(define (expand-array arr)
  (for/array: #:shape (vector (* 2 (array-size arr)))
    ([i (in-range (* 2 (array-size arr)))]) : T
    (array-ref arr (vector (floor (/ i 2))))))



;; use a subset of the elements in a given array of haar coefficients
;; to reconstruct a larger array
(: haar-reconstruct-step ((Array Real) (Array Real) Index Real ->
                                       (Array Real)))
(define (haar-reconstruct-step accum coefficients offset mult)
  (define adjustment-array
    (for/array ([i (in-range (array-size accum))]) : Real
      (define coefficient
        (* mult
           (array-ref coefficients
                      (vector (+ offset
                                 (floor (/ i 2)))))))
      (cond [(= 0 (modulo i 2)) coefficient]
            [else (- coefficient)])))
  (array+ accum
          adjustment-array))



;; given a 1-d array of reals
;; whose length is a power of 2, perform an inverse
;; wavelet transform.
(: fast-haar-inverse-transform ((Array Real) -> (Array Real)))
(define (fast-haar-inverse-transform arr)
  (unless (= 1 (vector-length (array-shape arr)))
    (raise-argument-error 'fast-haar-transform
                          "Array with one dimension"
                          0 arr))
  (unless (power-of-2? (vector-ref (array-shape arr) 0))
    (raise-argument-error 'fast-haar-transform
                          "Array whose length is a power of 2"
                          0 arr))
  (define init-array (make-array #(1)
                                 (* (array-ref arr (vector 0))
                                    (sqrt (/ 1 (array-size arr))))))
  (let loop ([offset : Index 1]
             [accum init-array])
    (cond [(= offset (array-size arr)) accum]
          [else
           (define expanded (expand-array accum))
           (define multiplier (sqrt (/ (array-size accum) (array-size arr))))
           (loop (ensure-index (+ offset (array-size accum)))
                 (haar-reconstruct-step expanded
                                        arr offset
                                        multiplier))])))

(: ensure-index (Nonnegative-Fixnum -> Index))
(define (ensure-index i)
  (cond [(index? i) i]
        [else (raise-argument-error 'ensure-index
                                    "Index"
                                    0 i)]))

(: power-of-2? (Nonnegative-Integer -> Boolean))
(define (power-of-2? p)
  (cond [(= p 1) true]
        [(= (modulo p 2) 0)
         (power-of-2? (floor (/ p 2)))]
        [else false]))