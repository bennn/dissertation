#lang racket/base

(require (except-in greenman-thesis #%module-begin)
         ;; re-use Scribble's doclang2 module-begin to sort everything
         ;; into a doc binding
         (only-in scribble/doclang2 #%module-begin))

(provide (all-from-out greenman-thesis)
         #%module-begin)
