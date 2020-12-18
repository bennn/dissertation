#lang racket/base

;; plot samples together

(require file/glob racket/format pict pict-abbrevs)

(define bm* '(take5 tetris synth quadU quadT))

(define (glob1 pat)
  (define m* (glob pat))
  (if (or (null? m*) (not (null? (cdr m*))))
    (raise-argument-error 'glob1 "die" pat)
    (car m*)))

(for ((dir (in-list (list "sample-avg" "sample")))
      (name (in-list (list "avg-abs-dist.png" "all-guesses.png"))))
 (save-pict name
  (add-rounded-border
    #:x-margin 10 #:y-margin 10
    (for/fold ((acc (blank)))
              ((bm (in-list bm*)))
      (vl-append
        acc
        (blank 0 12)
        (vl-append
          4
          (text (~a bm) (list 'bold) 20)
          (bitmap (glob1 (build-path dir (format "~a-*.png" bm))))))))))


