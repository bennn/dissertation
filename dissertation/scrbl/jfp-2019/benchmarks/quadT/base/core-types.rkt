#lang typed/racket/base

(provide
 (except-out (all-defined-out) quad)
 (all-from-out typed/racket/draw))

(require (only-in typed/racket/unsafe unsafe-provide))
(unsafe-provide quad)

;; -----------------------------------------------------------------------------

(require
 (only-in typed/racket/draw Font-Weight Font-Style))

;; =============================================================================

(define-type QuadName Symbol)
(: QuadName? (-> Any Boolean : QuadName))
(define QuadName? symbol?)

(define-type QuadAttrKey Symbol)
(: QuadAttrKey? (-> Any Boolean : QuadAttrKey))
(define QuadAttrKey? symbol?)

(define-type QuadAttrValue (U Float Index String Symbol Boolean Quad QuadAttrs QuadList Integer))
(: QuadAttrValue? (-> Any Boolean : QuadAttrValue))
(define (QuadAttrValue? x)
  (or (flonum? x) (index? x) (string? x) (symbol? x) (boolean? x) (quad? x) (QuadAttrs? x) (QuadList? x) (exact-integer? x)))

;; QuadAttr could be a list, but that would take twice as many cons cells.
;; try the economical approach.
(define-type QuadAttr (Pairof QuadAttrKey QuadAttrValue))
(: QuadAttr? (-> Any Boolean : QuadAttr))
(define (QuadAttr? x)
  (and (pair? x)
       (QuadAttrKey? (car x))
       (QuadAttrValue? (cdr x))))

(define-type QuadAttrs (Listof QuadAttr))
(: QuadAttrs? (-> Any Boolean : QuadAttrs))
(define (QuadAttrs? x)
  (and (list? x)
       (andmap QuadAttr? x)))
(define quad-attrs? QuadAttrs?)

(define-type HashableList  (Rec duo (U Null (List* QuadAttrKey Any duo))))
(define-type JoinableType (U Quad QuadAttrs HashableList))


(define-type QuadListItem (U String Quad))
(: QuadListItem? (-> Any Boolean : QuadListItem))
(define (QuadListItem? x)
  (or (string? x)
      (quad? x)))

(define-type QuadList (Listof QuadListItem))
(: QuadList? (-> Any Boolean : QuadList))
(define (QuadList? x)
  (and (list? x)
       (andmap QuadListItem? x)))

(define-type (Treeof A) (Rec as (U A (Listof as))))


;; funky implementation
(define-type Quad (List* QuadName QuadAttrs QuadList))
(: quad? (-> Any Boolean : Quad))
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

(define-type GroupQuad (List* QuadName QuadAttrs GroupQuadList))

(define-type GroupQuadListItem Quad)
(: GroupQuadListItem? (-> Any Boolean : GroupQuadListItem))
(define GroupQuadListItem? quad?)

(define-type GroupQuadList (Listof GroupQuadListItem))
(: GroupQuadList? (-> Any Boolean : GroupQuadList))
(define (GroupQuadList? x)
  (and (list? x)
       (andmap GroupQuadListItem? x)))


;; quad wants to be generic
;; if it's a function, it must impose a type on its output value
;; whereas if it's syntax, it can avoid demanding or imposing any typing
;;bg worried I can't keep the syntax
(define-syntax-rule (quad name attrs items)
  (list* name attrs items))

(define-type QuadSet (List QuadName QuadAttrs (Listof Quad)))


(define-type Font-Name String)
(: Font-Name? (-> Any Boolean : Font-Name))
(define Font-Name? string?)

(define-type Font-Size Positive-Flonum)
(: Font-Size? (-> Any Boolean : #:+ Font-Size))
(define (Font-Size? x)
  (and (flonum? x) (< 0 x)))

(: Font-Weight? (-> Any Boolean : Font-Weight))
(define (Font-Weight? x)
  (or (eq? x 'normal)
      (eq? x 'bold)
      (eq? x 'light)))

(: Font-Style? (-> Any Boolean : Font-Style))
(define (Font-Style? x)
  (or (eq? x 'normal)
      (eq? x 'italic)
      (eq? x 'slant)))

;; Index is arguably the stricter type for Breakpoint,
;; but in practice it's annoying because doing math with Indexes
;; often leads to non-Index values.
(define-type Breakpoint Nonnegative-Integer)
(: Breakpoint? (-> Any Boolean : Breakpoint))
(define (Breakpoint? x)
  (and (exact-integer? x) (<= 0 x)))

(: listof-quad? (-> Any Boolean : (Listof Quad)))
(define (listof-quad? x)
  (and (list? x)
       (andmap quad? x)))
