#lang typed/racket/base

(require typed/racket/stream
         typed/rackunit
         )

(: s1212 : (Sequenceof Natural))
(define s1212
  (stream-cons 1 (stream-cons 2 s1212)))

(check-equal? (stream-first s1212) 1)

(check-equal? (stream-first (stream-rest s1212)) 2)

(check-equal? (stream-ref s1212 100) 1)
(check-equal? (stream-ref s1212 101) 2)

(define p
  (stream (begin (displayln "evaluating") 5)))

(check-equal? (stream? s1212) #t)
(check-equal? (stream? p) #t)
(check-equal? (stream? (list 1 2 3)) #t)
(check-equal? (stream? (vector 1 2 3)) #f)

(let ([out (open-output-string)])
  (check-equal? (get-output-string out) "")
  (parameterize ([current-output-port out])
    (stream-first p)
    (check-equal? (stream-first p) 5)
    (stream-first p))
  (check-equal? (get-output-string out) "evaluating\n"))

