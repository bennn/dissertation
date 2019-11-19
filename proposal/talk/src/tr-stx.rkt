#lang racket/base

(module a typed/racket
  (define stx #`#,(vector 0 1))
  (provide stx)
)

(module b racket/base
  (require (submod ".." a))
  stx
)

(require 'b)
