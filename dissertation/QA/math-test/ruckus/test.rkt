#lang racket

(require
  (only-in "../core/math.rkt" vec3)
  "interpolation.rkt")

(define (parse fn)
  (define v (file->value fn))
  (let loop ((v v))
    (cond
      [(pair? v)
       (if (eq? 'struct:vec3 (car v))
         (apply vec3 (cdr v))
         (cons (loop (car v)) (loop (cdr v))))]
      [else
       v])))

(time
  (void (solve-interpolated-surface-system (parse "interpolation-surface.rktd"))))

(time
  (void (solve-interpolated-surface-system (parse "holography.rktd"))))
