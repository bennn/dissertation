#lang racket/base

(provide stream-cons stream empty-stream)

(require racket/function
         syntax/parse/define
         "stream-cons-thunk.rkt"
         (for-syntax racket/base
                     syntax/parse
                     ))

(define-simple-macro (stream-cons a:expr b:expr)
  (stream-cons/thunk (thunk a) (thunk b)))

(define-syntax stream
  (syntax-parser
    [(_)
     #'empty-stream]
    [(_ fst:expr rst:expr ...)
     #'(stream-cons fst (stream rst ...))]))

