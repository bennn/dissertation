#lang racket/base

;; Count number of Typed Racket base types

;; TODO some types are not exported from 'base-types'!
;; - Immutable-Vector
;; - Mutable-Vectorof
;; - Sequenceof
;; - Vector

;; so, the total is 199 + 4 = 203

(require typed-racket/base-env/base-types racket/pretty)

(define-values [var* stx*] (module->exports 'typed-racket/base-env/base-types))

(define type-name*
  (for*/list ((phase+export* (in-list stx*))
              (export* (in-list (cdr phase+export*))))
    (car export*)))

(printf "'typed-racket/base-env/base-types' exports ~a syntax identifiers~n" (length type-name*))

#;(pretty-print type-name*)
