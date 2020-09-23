#lang typed/racket #:transient

(module a racket/base
  (require json)
  (define get-json
    (let ((v (with-input-from-file "buzzbuzz.json" read-json)))
      (lambda () v)))
  (provide get-json))

(define-type T (HashTable Symbol Any #;(U 'null String (Listof String))))

(require/typed 'a
  (get-json (-> (Listof T))))

(define NUM-ITERS 20)

(define (main)
  (let loop : Void ((i : Natural 0))
    (unless (= i NUM-ITERS)
      (map (lambda ((x : T)) (hash-ref x "price_bbbb" #f)) (get-json))
      (loop (add1 i)))))

(time (void (main)))


