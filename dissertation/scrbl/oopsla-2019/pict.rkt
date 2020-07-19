#lang racket/base

(provide
  jungle:example-atom
  jungle:example-atom*
  jungle:example-pair
  jungle:example-pair*
  jungle:tr-api
  jungle:rp-api
)

(require
  racket/string
  racket/format
  racket/math
  (only-in racket/list take-right)

  pict
  pict-abbrevs
  ppict/pict ppict/tag
  (only-in scribble/core content->string)
  scribble-abbrevs/latex)

(define turn revolution)

(define shim-sep 4)
(define output-x 30)
(define output-y 12)

(define untyped-color "white")
(define typed-color "LightGray")
(define success-color "Forest Green")
(define error-color "Firebrick")

(define code-text-style (cons "Inconsolata" 'modern))
(define code-text-size 14)
(define title-text-size 12)
(define title-text-face "Liberation Serif")

(define (title-text str)
  (text str (cons 'bold title-text-face) title-text-size))

(define (code-text str [extra-style #f])
  (define style (if extra-style (cons extra-style code-text-style) code-text-style))
  (text str style code-text-size))

(define (success-text str)
  (colorize (code-text str 'bold) success-color))

(define (error-text str)
  (colorize (code-text str 'bold) error-color))

(define (code-text* str*)
  (if (string? str*)
    (code-text str*)
    (apply vl-append (* 1/4 (pict-height (code-text "x"))) (map code-text str*))))

(define (typed-codeblock str*)
  (add-rounded-border
    #:radius 8
    #:background-color typed-color
    #:frame-width 1
    #:frame-color "black"
    #:x-margin 10
    #:y-margin 8
    (code-text* str*)))

(define (untyped-codeblock str*)
  (add-rounded-border
    #:radius 4
    #:background-color untyped-color
    #:frame-width 3
    #:frame-color "black"
    #:x-margin 20
    #:y-margin 14
    (code-text* str*)))

(define (add-modname str pict)
  (vl-append
    shim-sep
    (code-text str)
    (ht-append shim-sep (blank) pict)))

(define (typed-mod str str*)
  (add-modname str (typed-codeblock str*)))

(define (untyped-mod str str*)
  (add-modname str (untyped-codeblock str*)))

(struct code-arrow [src-tag src-find tgt-tag tgt-find start-angle end-angle start-pull end-pull style] #:transparent)

(define (add-code-arrow pp arrow
                        #:arrow-size [pre-arrow-size #f]
                        #:line-width [pre-line-width #f]
                        #:color [color "black"]
                        #:label [label (blank)]
                        #:hide? [hide? #false])
  (define line-width (or pre-line-width 2))
  (define arrow-size (or pre-arrow-size 6))
  (pin-arrow-line
    arrow-size pp
    (let ((src-tag (code-arrow-src-tag arrow)))
      (if (symbol? src-tag) (find-tag pp src-tag) src-tag))
    (code-arrow-src-find arrow)
    (let ((tgt-tag (code-arrow-tgt-tag arrow)))
      (if (symbol? tgt-tag) (find-tag pp tgt-tag) tgt-tag))
    (code-arrow-tgt-find arrow)
    #:line-width line-width
    #:label label
    #:hide-arrowhead? hide?
    #:style (code-arrow-style arrow)
    #:start-angle (code-arrow-start-angle arrow)
    #:end-angle (code-arrow-end-angle arrow)
    #:start-pull (code-arrow-start-pull arrow)
    #:end-pull (code-arrow-end-pull arrow)
    #:color color))

(define (add-code-arrow* pp . arrow*)
  (for/fold ((acc pp))
            ((arrow (in-list arrow*)))
    (add-code-arrow pp arrow)))

(define (tag-append . x*)
  (string->symbol (string-join (map ~a x*) "-")))

(define (add-hubs pp tag)
  (define io-margin 8)
  (define node-padding 6)
  (define h-blank (blank 0 io-margin))
  (define v-blank (blank io-margin 0))
  (vc-append
    node-padding
    (tag-pict v-blank (tag-append tag 'N))
    (hc-append
      node-padding
      (tag-pict h-blank (tag-append tag 'W)) (tag-pict pp tag) (tag-pict h-blank (tag-append tag 'E)))
    (tag-pict v-blank (tag-append tag 'S))))

(define (make-example-atom-pict t-str* u-str* ok? r-str*)
  (define t-pict (typed-codeblock t-str*))
  (define u-pict (untyped-codeblock u-str*))
  (define r-pict (if r-str* (if ok? (success-text r-str*) (error-text r-str*)) (blank)))
  (define tu-pict
    (let* ((t/sep
             (vl-append t-pict (tag-pict (blank shim-sep shim-sep) 't-shim)))
           (u/sep
             (vl-append (tag-pict (blank shim-sep shim-sep) 'u-shim) u-pict))
           (u/right
             (hc-append shim-sep u/sep (tag-pict (blank output-x 0) 'u-out-0)))
           (u/full
             (vr-append (tag-pict (blank 0 output-y) 'u-out-1) u/right)))
      (add-code-arrow
        (vl-append t/sep (blank 0 shim-sep) u/full)
        (code-arrow 't-shim rb-find 'u-shim rt-find (* 3/4 turn) (* 3/4 turn) 0 0 'solid))))
  (define r-out-arrow (code-arrow 'u-out-0 lb-find 'u-out-0 rb-find 0 0 0 0 'dot))
  (define tu/arrow
    (if ok?
      (add-code-arrow
        (add-code-arrow
          #:arrow-size 0
          tu-pict r-out-arrow)
        (code-arrow 'u-out-0 rb-find 'u-out-1 rt-find (* 1/4 turn) (* 1/4 turn) 0 0 'dot))
      (add-code-arrow tu-pict r-out-arrow)))
  (ppict-do
    tu/arrow
    #:go (at-find-pict 'u-out-0 rc-find 'lc #:abs-x (* 2 shim-sep))
    r-pict))

(define (make-example-pair-pict u-str* t-str* r-str*)
  (define u-pict (untyped-codeblock u-str*))
  (define t-pict (typed-codeblock t-str*))
  (define r-pict (if r-str* (error-text r-str*) (blank)))
  (define tu-pict
    (let* ((t/u
            (vl-append
              t-pict
              (tag-pict (blank shim-sep shim-sep) 't-shim)
              (blank 0 output-y)
              (tag-pict (blank shim-sep shim-sep) 'u-shim)
              u-pict
              (hb-append (blank shim-sep output-y)
                         (tag-pict (blank output-x 0) 'u-out-0)))))
      (add-code-arrow
        t/u
        (code-arrow 't-shim rb-find 'u-shim rt-find (* 3/4 turn) (* 3/4 turn) 0 0 'solid))))
  (define r-out-arrow (code-arrow 'u-out-0 lb-find 'u-out-0 rb-find 0 0 0 0 'dot))
  (ppict-do
    (add-code-arrow tu-pict r-out-arrow)
    #:go (at-find-pict 'u-out-0 rc-find 'lc #:abs-x (* 2 shim-sep))
    r-pict))

(define (interleave a* b*)
  (cond
    [(null? a*)
     b*]
    [(null? b*)
     a*]
    [else
      (list* (car a*) (car b*) (interleave (cdr a*) (cdr b*)))]))

(define (make-example-table . pict*)
  (define title*
    (list (title-text "Flow") (title-text "Reticulated")
          (title-text "Typed Racket") (title-text "Nom")))
  (table 2 (interleave (take-right title* (length pict*)) pict*)
    lt-superimpose lt-superimpose 10 34))

(define jungle:example-atom
  (make-example-atom-pict
    '("f = λ(x:Int) x+1")
    '("f f")
    #true
    #false))

(define jungle:example-atom-flow
  (make-example-atom-pict
    '("function f(x : number): number {"
      "  return x+1;"
      "}")
    '("f(f);")
    #true
    "Ok"))

(define jungle:example-atom-rp
  (make-example-atom-pict
    '("def f(x : Int)->Int:"
      "  return x + 1")
    '("f(f)")
    #false
    "Error"))

(define jungle:example-atom-tr
  (make-example-atom-pict
    '("(: f (-> Integer Integer))"
      "(define (f x)"
      "  (+ x 1))")
    '("(f f)")
    #false
    "Error"))

(define jungle:example-atom-nom
  (make-example-atom-pict
    '("class F {"
      "  constructor () {}"
      "  fun apply(Int x) : Int {"
      "    return x + 1;"
      "  }"
      "}")
    '("dyn f = new F();"
      "f.apply((dyn)f);")
    #false
    "Error"))

(define jungle:example-atom*
  (make-example-table
    jungle:example-atom-flow
    jungle:example-atom-rp
    jungle:example-atom-tr
    jungle:example-atom-nom))

(define jungle:example-pair
  (make-example-atom-pict
    '("g = λ(x:Int×Int) (fst x)+1")
    '("g (\"A\", 2)")
    #true
    #false))

(define jungle:example-pair-rp
  (make-example-pair-pict
    '("x = [\"A\", 2]")
    '("def g(y : Tuple(Int,Int)):"
      "  return y[0] + 1"
      ""
      "g(x)")
    "Error"))

(define jungle:example-pair-tr
  (make-example-pair-pict
    '("(define x (list \"A\" 2))")
    '("(require/typed"
      "  [x (List Integer Integer)])"
      ""
      "(+ (first x) 1)")
    "Error"))

(define jungle:example-pair-nom
  (make-example-pair-pict
    '("class Pair {"
      "  private fst;"
      "  private snd;"
      "  # ...."
      "}"
      ""
      "x = new Pair(\"A\", 2)")
    '("class IntPair {"
      "  private Int fst;"
      "  private Int snd;"
      "  # ...."
      "}"
      ""
      "((IntPair)x).fst + 1")
    "Error"))

(define jungle:example-pair*
  (make-example-table
    jungle:example-pair-rp
    jungle:example-pair-tr
    jungle:example-pair-nom))

(define (make-api-pict u-net t-net client
                       #:callback-x-sep [cx #f]
                       #:callback-y-sep [cy #f]
                       #:callback-start-angle [csa #f]
                       #:callback-start-pull [csp #f]
                       #:callback-end-angle [cea #f]
                       #:callback-end-pull [cep #f]
                       #:extra-x-pad [xxp 0])
  (let* ((v-pict
          (vl-append
            output-y
            u-net
            (ht-append (* 8 shim-sep) (blank) t-net (blank xxp 0))
            client))
         (pp
           (ppict-do v-pict
              #:go (at-find-pict u-net lb-find 'lt #:abs-x shim-sep #:abs-y shim-sep)
              (tag-pict (blank) 'LT)
              #:go (at-find-pict u-net rb-find 'rt #:abs-x shim-sep #:abs-y (* (or cy 1) shim-sep))
              (tag-pict (blank) 'RT)
              #:go (at-find-pict t-net lc-find 'rt #:abs-x (- shim-sep))
              (tag-pict (blank (* 6 shim-sep) (* 4 shim-sep)) 'LC)
              #:go (at-find-pict client lt-find 'lb #:abs-x shim-sep #:abs-y (- shim-sep))
              (tag-pict (blank) 'LB)
              #:go (at-find-pict client rt-find 'rt #:abs-x (* (or cx -1) shim-sep) #:abs-y (* 4 shim-sep))
              (tag-pict (blank) 'RB)))
         (pp/line*
           (for/fold ((acc pp))
                     ((arr (in-list
                            (list
                              (code-arrow 'LT lb-find 'LC lt-find (* 3/4 turn) (* 3/4 turn) 0 0 'solid)
                              (code-arrow 'LC rb-find 'LC lb-find (* 1/2 turn) (* 1/2 turn) 0 0 'solid)))))
             (add-code-arrow acc arr #:arrow-size 0)))
         (pp/arrow*
           (for/fold ((acc pp/line*))
                     ((arr (in-list
                            (list
                              (code-arrow 'LC lt-find 'LC rt-find 0 0 0 0 'solid)
                              (code-arrow 'LC lb-find 'LB lt-find (* 3/4 turn) (* 3/4 turn) 0 0 'solid)
                              (code-arrow 'RB rt-find 'RT rb-find (* (or csa 25/100) turn) (* (or cea 35/100) turn) (or csp 1/2) (or cep 1/2) 'dot)))))
             (add-code-arrow acc arr))))
    pp/arrow*))

(define jungle:tr-api
  (make-api-pict
    (untyped-mod
      "net/url"
      '("#lang racket"
        ";; +600 lines of code ...."
        ""
        "(define (call/input-url url c h)"
        "  ;; connect to the url via c,"
        "  ;; process the data via h"
        "  ....)"))
    (typed-mod
      "typed/net/url"
      '("#lang typed/racket"
        ""
        "(define-type URL ....)"
        ""
        "(require/typed/provide"
        "  ;; from this library"
        "  net/url"
        ""
        "  ;; import the following"
        "  [string->url"
        "   (-> String URL)]"
        ""
        "  [call/input-url"
        "   (∀ (A)"
        "    (-> URL"
        "        (-> String In-Port)"
        "        (-> In-Port A)"
        "        A))])"))
    (untyped-mod
      "client"
      '("#lang racket"
        "(require html typed/net/url)"
        ""
        "(define URL"
        "  (string->url \"https://sr.ht\"))"
        ""
        ";; connect to url, read html"
        "(define (main)"
        "  (call/input-url URL (λ(str) ....) read-html))"))))

(define jungle:rp-api
  (make-api-pict
    #:callback-x-sep 0
    #:callback-y-sep -1
    #:callback-start-angle 3/100
    #:callback-start-pull 125/100
    #:callback-end-angle 47/100
    #:callback-end-pull 115/100
    #:extra-x-pad 30
    (untyped-mod
      "requests"
      '("# 2,000 lines of code ...."
        ""
        "def get(url, *args, **kws):"
        "  # Sends a GET request"
        "  ...."))
    (typed-mod
      "typed_requests"
      '("import requests as r"
        ""
        "def get(url:Str, to:Tuple(Float,Float)):"
        "  return r.get(url, to)"))
    (untyped-mod
      "client"
      '("from typed_requests import get"
        ""
        "wait_times = (2, \"zero\")"
        "get(\"https://sr.ht\", wait_times)"))))

(module+ raco-pict
  (provide raco-pict)
  (define raco-pict
    (add-rectangle-background #:color "white" #:x-margin 40 #:y-margin 40
      (apply vl-append 10
        jungle:rp-api
        '()
    )))
)
