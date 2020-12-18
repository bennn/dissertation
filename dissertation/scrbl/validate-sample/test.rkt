#lang racket

;; script testing worst-case / repeat-case sampling

(require
  (prefix-in tr: greenman-thesis/jfp-2019/main)
  (prefix-in rp: greenman-thesis/pepm-2018/main)
  pict pict-abbrevs
  gtp-plot/configuration-info
  gtp-plot/typed-racket-info
  gtp-plot/performance-info)

(define overhead-plots-per-page 7)

(define (sample1)
      (apply
        vl-append
        30
        (append
          (let ((f (tr:make-render-worst-case-validate-plot* "7.7")))
            (for/list ((sym (in-list (map tr:benchmark-name (take-right tr:ALL-BENCHMARKS overhead-plots-per-page)))))
              (apply ht-append 20 (f sym))))
          (for/list ((sym (in-list (take rp:VALIDATE-BENCHMARKS overhead-plots-per-page))))
            (apply ht-append 20 (rp:render-worst-case-validate-plot* sym))))))

(define (sample2)
  (apply
    vl-append
    30
    (let ((f (tr:make-render-validate-plot "7.7")))
      (for/list ((sym (in-list (map tr:benchmark-name (take-right tr:ALL-BENCHMARKS overhead-plots-per-page)))))
        (apply ht-append 20
               (for/list ((i (in-range 10)))
                 (f sym)))))))

(module+ main
  (save-pict
    "sample.png"
    (add-rounded-border
      #:background-color "white"
      #:radius 1
      #:x-margin 40
      #:y-margin 30
      #:frame-width 0
      (sample2)

      ))
  (void))

