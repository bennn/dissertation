#lang racket/base

(provide
  page-break?
  column-break?
  block-break?
  word-break?
  column?
  line?
  run?
  word?
  optical-kern?
  spacer?
  nonnegative-flonum?
)

(require
  racket/math
  "core.rkt")

;; -----------------------------------------------------------------------------

(define-syntax-rule (make-QL-pred QL? sym)
  (lambda (x)
    (and (pair? x)
         (let ((a (car x))
               (b (cdr x)))
           (and (symbol? a)
                (eq? a sym)
                (pair? b)
                (let ((aa (car b))
                      (bb (cdr b)))
                  (and (QuadAttrs? aa)
                       (QL? bb))))))))

(define-syntax-rule (make-quad-list-pred sym)
  (make-QL-pred QuadList? sym))

(define-syntax-rule (make-group-list-pred sym)
  (make-QL-pred GroupQuadList? sym))

(define page-break? (make-quad-list-pred 'page-break))
(define column-break? (make-quad-list-pred 'column-break))
(define block-break? (make-quad-list-pred 'block-break))
(define word-break? (make-quad-list-pred 'word-break))
(define column? (make-group-list-pred 'column))
(define line? (make-group-list-pred 'line))
(define word? (make-quad-list-pred 'word))
(define run? (make-quad-list-pred 'run))
(define spacer? (make-quad-list-pred 'spacer))
(define optical-kern? (make-quad-list-pred 'optical-kern))

(define (nonnegative-flonum? x)
  (and (flonum? x) (<= 0 x)))
