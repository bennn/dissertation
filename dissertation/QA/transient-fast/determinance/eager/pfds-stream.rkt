#lang typed/racket

(provide empty-stream? empty-stream stream-cons stream-car
         stream-cdr stream-append stream-reverse stream
         stream->list drop take Stream
         #;stream-map #;stream-foldl #;stream-foldr)

(define-type Stream (All (A) (Listof A)))

(define empty-stream null)

(: empty-stream? : (All (A) ((Stream A) -> Boolean)))
(define (empty-stream? stream) (null? stream))


(: stream-car : (All (A) ((Stream A) -> A)))
(define (stream-car stream)
  (if (null? stream)
      (error 'stream-car "given stream is empty")
      (car stream)))

(: stream-cdr : (All (A) ((Stream A) -> (Stream A))))
(define (stream-cdr stream)
  (if (null? stream)
      (error 'stream-cdr "given stream is empty")
      (cdr stream)))

(define-syntax-rule (stream-cons x stream) (cons x stream))

(: stream-append : (All (A) (Stream A) (Stream A) -> (Stream A)))
(define (stream-append stream1 stream2)
  (cond
    [(null? stream1) stream2]
    [(null? stream2) stream1]
    [else (stream-cons (stream-car stream1)
                       (stream-append (stream-cdr stream1) stream2))]))

(: stream-reverse : (All (A) (Stream A) -> (Stream A)))
(define (stream-reverse stream)
  (: loop : (All (A) (Stream A) (Stream A) -> (Stream A)))
  (define (loop stream accum)
    (if (null? stream)
        accum
        (loop (stream-cdr stream) 
              (ann (stream-cons (stream-car stream) accum) (Stream A)))))
  (loop stream empty-stream))

(: stream : (All (A) (A * -> (Stream A))))
(define (stream . xs)
  (: loop : (All (A) ((Listof A) -> (Stream A))))
  (define (loop xs)
    (if (null? xs)
        '()
        (cons (car xs) (loop (cdr xs)))))
  (loop xs))

(: stream->list : (All (A) ((Stream A) -> (Listof A))))
(define (stream->list stream) stream)

(: drop : (All (A) (Integer (Stream A) -> (Stream A))))
(define (drop num stream)
  (cond 
    [(zero? num)    stream] 
    [(null? stream) (error 'drop "not enough elements to drop")]
    [else (drop (sub1 num) (cdr stream))]))


(: take : (All (A) (Integer (Stream A) -> (Stream A))))
(define (take num stream)
  (cond 
    [(zero? num)    empty-stream]
    [(null? stream) (error 'take "not enough elements to take")]
    [else (cons (car stream) (take (sub1 num) (cdr stream)))]))



