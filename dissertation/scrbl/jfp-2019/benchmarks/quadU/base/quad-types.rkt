#lang typed/racket/base

(provide
  quad
  Quad quad?
  quad-attrs? QuadAttrs
  USQ
  Font-Weight?
  Font-Style?)

(require
  (only-in typed/racket/draw Font-Weight Font-Style))

(define-type USQ (U String Quad))

(: quad (-> Symbol QuadAttrs (Listof Any) Quad))
(define (quad name attrs items)
  (list* name attrs items))

(define-type QuadAttrs (Listof (Pairof Symbol Any)))
(: quad-attrs? (-> Any Boolean : QuadAttrs))
(define quad-attrs?
  (let ()
    (: pairof-sym-any (-> Any Boolean : (Pairof Symbol Any)))
    (define (pairof-sym-any x)
      (and (pair? x)
           (symbol? (car x))))
    (lambda (x)
      (and (list? x)
           (andmap pairof-sym-any x)))))

(define-type Quad (List* Symbol QuadAttrs (Listof Any)))
(: quad? (-> Any Boolean : Quad))
(define (quad? x)
  (and (pair? x)
       (let ((a (car x))
             (b (cdr x)))
         (and (symbol? a)
              (pair? b)
              (let ((aa (car b))
                    (bb (cdr b)))
                (and (quad-attrs? aa)
                     (list? bb)))))))

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

