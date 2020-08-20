#lang typed/racket #:transient

(require (only-in typed/web-server/http response/xexpr response))

(provide f1 f2 f3 f4)

(: f1 (-> response))
(define (f1)
  (define x '(body (h1 "Try it")))
  (: resp response)
  (define resp (response/xexpr x))
  resp)

(: f2 (-> (U response Any)))
(define (f2)
  (define x '(body (h1 "Try it")))
  (: resp response)
  (define resp (response/xexpr x))
  resp)

(: f3 (-> (U response Number)))
(define (f3)
  (define x '(body (h1 "Try it")))
  (: resp response)
  (define resp (response/xexpr x))
  resp)

(: f4 (-> (U Any response)))
(define (f4)
  (define x '(body (h1 "Try it")))
  (: resp response)
  (define resp (response/xexpr x))
  resp)

