#lang racket/base

(provide
 (all-defined-out))

;; -----------------------------------------------------------------------------

(require
  "untyped.rkt")

;; =============================================================================

(define QuadName? symbol?)

(define QuadAttrKey? symbol?)

(define (QuadAttrValue? x)
  (or (flonum? x) (index? x) (string? x) (symbol? x) (boolean? x) (quad? x) (QuadAttrs? x) (QuadList? x) (exact-integer? x)))

;; QuadAttr could be a list, but that would take twice as many cons cells.
;; try the economical approach.
(define (QuadAttr? x)
  (and (pair? x)
       (QuadAttrKey? (car x))
       (QuadAttrValue? (cdr x))))

(define (QuadAttrs? x)
  (and (list? x)
       (andmap QuadAttr? x)))
(define quad-attrs? QuadAttrs?)

(define (QuadListItem? x)
  (or (string? x)
      (quad? x)))

(define (QuadList? x)
  (and (list? x)
       (andmap QuadListItem? x)))

;; funky implementation
(define (quad? x)
  (and (pair? x)
       (let ((a (car x))
             (b (cdr x)))
         (and (QuadName? a)
              (pair? b)
              (let ((aa (car b))
                    (bb (cdr b)))
                (and (QuadAttrs? aa)
                     (QuadList? bb)))))))

;; quad wants to be generic
;; if it's a function, it must impose a type on its output value
;; whereas if it's syntax, it can avoid demanding or imposing any typing
(define-syntax-rule (quad name attrs items)
  (list* name attrs items))

(define GroupQuadListItem? quad?)

(define (GroupQuadList? x)
  (and (list? x)
       (andmap GroupQuadListItem? x)))

(define Font-Name? string?)

(define (Font-Size? x)
  (and (flonum? x) (< 0 x)))

(define (Font-Weight? x)
  (or (eq? x 'normal)
      (eq? x 'bold)
      (eq? x 'light)))

(define (Font-Style? x)
  (or (eq? x 'normal)
      (eq? x 'italic)
      (eq? x 'slant)))

;; Index is arguably the stricter type for Breakpoint,
;; but in practice it's annoying because doing math with Indexes
;; often leads to non-Index values.
(define (Breakpoint? x)
  (and (exact-integer? x) (<= 0 x)))

(define (listof-quad? x)
  (and (list? x)
       (andmap quad? x)))
