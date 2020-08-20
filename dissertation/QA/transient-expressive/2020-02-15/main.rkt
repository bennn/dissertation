#lang typed/racket/base #:transient

(require/typed racket/vector
  (vector-sort (-> (Vectorof Any) (-> Any Any Boolean) (Vectorof Any))))

(define vs : (Vectorof Real)
  (vector 1 2 3))

(define vvs : (Vectorof Real)
  (vector-sort vs <=))
