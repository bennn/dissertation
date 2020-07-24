#lang typed/racket/base

(define-type Quad (List* Symbol Quad-Attrs (Listof Any)))
(define-type Quad-Attrs (Listof (Pairof Symbol Any)))

(provide Quad Quad-Attrs)

