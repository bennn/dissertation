#lang at-exp racket/base

(provide
  nname
  cname
  fname
  aname
  tname
  ename

)

(require
  scribble-abbrevs)

;; -----------------------------------------------------------------------------

(define nname (exact "\\nname"))
(define cname (exact "\\cname"))
(define fname (exact "\\fname"))
(define aname (exact "\\aname"))
(define tname (exact "\\tname"))
(define ename (exact "\\ename"))

