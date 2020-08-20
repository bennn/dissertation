#lang racket

(require "type-test.rkt"
         web-server/servlet
         web-server/servlet-env)

(define (start req)
  ; (f1)
  ; (f2)
  ; (f3)
  (f4)
  )

(serve/servlet start
               #:servlet-regexp #rx""
               #:launch-browser? #false
               #:port 8080)
