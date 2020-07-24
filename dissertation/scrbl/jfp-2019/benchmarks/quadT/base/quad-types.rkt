#lang typed/racket/base

(provide
  BoxQuad
  RunQuad
  SpacerQuad
  DocQuad
  Optical-KernQuad
  PieceQuad
  WordQuad
  Word-BreakQuad
  PageQuad
  Page-BreakQuad
  ColumnQuad
  Column-BreakQuad
  LineQuad
  BlockQuad
  Block-BreakQuad
  ;; --
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

;; -----------------------------------------------------------------------------

(require "core-types.rkt")

;; =============================================================================

(define-type BoxQuad (List* 'box QuadAttrs QuadList))
(define-type RunQuad (List* 'run QuadAttrs QuadList))
(define-type SpacerQuad (List* 'spacer QuadAttrs QuadList))
(define-type DocQuad (List* 'doc QuadAttrs QuadList))
(define-type Optical-KernQuad (List* 'optical-kern QuadAttrs QuadList))
(define-type PieceQuad (List* 'piece QuadAttrs GroupQuadList))
(define-type WordQuad (List* 'word QuadAttrs QuadList))
(define-type Word-BreakQuad (List* 'word-break QuadAttrs QuadList))
(define-type PageQuad (List* 'page QuadAttrs GroupQuadList))
(define-type Page-BreakQuad (List* 'page-break QuadAttrs QuadList))
(define-type ColumnQuad (List* 'column QuadAttrs GroupQuadList))
(define-type Column-BreakQuad (List* 'column-break QuadAttrs QuadList))
(define-type LineQuad (List* 'line QuadAttrs GroupQuadList))
(define-type BlockQuad (List* 'block QuadAttrs QuadList))
(define-type Block-BreakQuad (List* 'block-break QuadAttrs QuadList))

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

(: page-break? (-> Any Boolean : Page-BreakQuad))
(define page-break? (make-quad-list-pred 'page-break))
(: column-break? (-> Any Boolean : Column-BreakQuad))
(define column-break? (make-quad-list-pred 'column-break))
(: block-break? (-> Any Boolean : Block-BreakQuad))
(define block-break? (make-quad-list-pred 'block-break))
(: word-break? (-> Any Boolean : Word-BreakQuad))
(define word-break? (make-quad-list-pred 'word-break))
(: column? (-> Any Boolean : ColumnQuad))
(define column? (make-group-list-pred 'column))
(: line? (-> Any Boolean : LineQuad))
(define line? (make-group-list-pred 'line))
(: word? (-> Any Boolean : WordQuad))
(define word? (make-quad-list-pred 'word))
(: run? (-> Any Boolean : RunQuad))
(define run? (make-quad-list-pred 'run))
(: spacer? (-> Any Boolean : SpacerQuad))
(define spacer? (make-quad-list-pred 'spacer))
(: optical-kern? (-> Any Boolean : Optical-KernQuad))
(define optical-kern? (make-quad-list-pred 'optical-kern))

(: nonnegative-flonum? (-> Any Boolean : #:+ Nonnegative-Flonum))
(define (nonnegative-flonum? x)
  (and (flonum? x) (<= 0 x)))
