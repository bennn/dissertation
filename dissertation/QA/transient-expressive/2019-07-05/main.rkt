#lang typed/racket #:transient
;;(require typed/racket/unsafe)

(module untyped racket
  (provide maybe-car)
  (define (maybe-car x)
    (cond
      [(pair? x) (car x)]
      [else x])))

(require/typed
 'untyped
 [maybe-car (All (a b) (case->
                        (-> (Pairof a b) a)
                        (-> a a)))])

(define s : String
  (maybe-car "hello"))

(define a : String
  (maybe-car (cons "A" 'B)))
