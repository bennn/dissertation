#lang racket

(require math/array)
(module+ test (require rackunit))

(define w 1000)
(define h 1000)

(define (make-grid)
  (array->mutable-array
    (make-array (vector w h) 0)))

(define (count-lit g)
  (for*/sum ((xy (in-array g)))
    xy))

(struct command [f x0 y0 x1 y1] #:transparent)

(define (toggle v)
  (if (= v 1) 0 1))

(define (off v)
  0)

(define (on v)
  1)

(define (add2 v)
  (+ v 2))

(define (nat-sub1 v)
  (if (= v 0) 0 (sub1 v)))

(define (parse-command str [part2? #f])
  (match (regexp-match #rx"^(.*) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)$" str)
    ((list _m cmd x0 y0 x1 y1)
     (command (str->f cmd part2?) (string->number x0) (string->number y0) (string->number x1) (string->number y1)))
    (_
      (raise-user-error 'parse-command "command-string?" str))))

(define (str->f str part2?)
  (match str
    ("toggle" (if part2? add2 toggle))
    ("turn on" (if part2? add1 on))
    ("turn off" (if part2? nat-sub1 off))
    (_ (raise-user-error 'str->f "f-string?" str))))

(define (do-command! g cmd)
  (define f (command-f cmd))
  (for* ((x (in-range (command-x0 cmd) (+ 1 (command-x1 cmd))))
         (y (in-range (command-y0 cmd) (+ 1 (command-y1 cmd)))))
    (define v (vector x y))
    (array-set! g v (f (array-ref g v)))))

(define (part1 input)
  (define g (make-grid))
  (with-input-from-file
    input
    (lambda ()
      (for ((ln (in-lines)))
        (do-command! g (parse-command ln)))))
  (count-lit g))

(define (part2 input)
  (define g (make-grid))
  (with-input-from-file
    input
    (lambda ()
      (for ((ln (in-lines)))
        (do-command! g (parse-command ln #true)))))
  (count-lit g))

(define input (map parse-command (file->lines "input")))

(define (main input)
  (part1 input)
  (part2 input)
  (void))

(time
  ;; 400410
  ;; 15343601
  (main "input"))
