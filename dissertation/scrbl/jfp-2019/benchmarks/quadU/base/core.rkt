#lang racket/base

(provide
  quad
  quad?
  quad-attrs?
  Font-Weight?
  Font-Style?)

;; =============================================================================

(define (quad name attrs items)
  (list* name attrs items))

(define quad-attrs?
  (let ()
    (define (pairof-sym-any x)
      (and (pair? x)
           (symbol? (car x))))
    (lambda (x)
      (and (list? x)
           (andmap pairof-sym-any x)))))

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

(define (Font-Weight? x)
  (or (eq? x 'normal)
      (eq? x 'bold)
      (eq? x 'light)))

(define (Font-Style? x)
  (or (eq? x 'normal)
      (eq? x 'italic)
      (eq? x 'slant)))
