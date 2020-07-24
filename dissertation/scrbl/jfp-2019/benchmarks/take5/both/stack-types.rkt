#lang typed/racket/base

(require "card-adapted.rkt")
(provide Stack)
(define-type Stack
  (Listof Card))
