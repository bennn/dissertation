#lang at-exp slideshow

;; Defense slides, Deep and Shallow types.
;;  1 hour presentation? roughly

;; TODO
;; - [X] get colors ... three kingdoms?
;; - [X] pick fonts
;; - [ ] outline, on paper
;; - [ ] jfp example, slides ... take from diss pict
;; - [ ]
;; - [ ]

(require
  file/glob
  pict pict/shadow
  pict-abbrevs pict-abbrevs/slideshow gtp-pict
  ppict/2
  racket/draw
  racket/list
  racket/string
  racket/format
  racket/runtime-path
  slideshow/code
  plot/no-gui (except-in plot/utils min* max*))

(define turn revolution)

;; =============================================================================
;; --- coordinates

(define slide-top 4/100)
(define slide-left 4/100)
(define slide-right (- 1 slide-left))
(define slide-bottom 92/100)
(define text-left (* 3/2 slide-left))
(define text-right (- 1 text-left))
(define text-top (* 4 slide-top))
(define text-bottom slide-bottom)

(define (scale-to-text pp)
  (define w (w%->pixels (- text-right text-left)))
  (define h (h%->pixels (- text-bottom text-top)))
  (scale-to-fit pp w h))

(define heading-coord-left (coord slide-left slide-top 'lt))
(define heading-coord-mid (coord 1/2 slide-top 'ct))
(define heading-coord-right (coord slide-right slide-top 'rt))
(define text-coord-left (coord text-left text-top 'lt))
(define text-coord-mid (coord 1/2 text-top 'ct))
(define text-coord-right (coord text-right text-top 'rt))
(define center-coord (coord 1/2 1/2 'cc))

;; -----------------------------------------------------------------------------
;; --- space

(define pico-x-sep (w%->pixels 1/100))
(define tiny-x-sep (w%->pixels 2/100))
(define small-x-sep (w%->pixels 5/100))
(define med-x-sep (w%->pixels 10/100))
(define big-x-sep (w%->pixels 15/100))

(define pico-y-sep (h%->pixels 1/100))
(define tiny-y-sep (h%->pixels 2/100))
(define small-y-sep (h%->pixels 5/100))
(define med-y-sep (h%->pixels 10/100))
(define big-y-sep (h%->pixels 15/100))

(define code-line-sep (h%->pixels 12/1000))
(define text-line-sep (h%->pixels  4/1000))

(define codeblock-x-sep (w%->pixels 4/100))
(define codeblock-y-sep (h%->pixels 4/100))

;; -----------------------------------------------------------------------------
;; --- color

(define black (string->color% "black"))
(define gray (string->color% "light gray"))
(define white (string->color% "white"))
(define transparent (color%-update-alpha white 0))
(define green0-3k1 (hex-triplet->color% #x71BE8D))
(define green1-3k1 (hex-triplet->color% #x598F61))
(define green2-3k1 (hex-triplet->color% #x4F7459))
(define green3-3k1 (hex-triplet->color% #x3D4A47))
(define red-3k1 (hex-triplet->color% #xC0446C))
(define orange0-3k1 (hex-triplet->color% #xE6BD82))
(define orange1-3k1 (hex-triplet->color% #xFE9D73))
(define orange2-3k1 (hex-triplet->color% #xB87D4A))
(define yellow0-3k1 (hex-triplet->color% #xF8F7B3))
(define yellow1-3k1 (hex-triplet->color% #xDBC847))
(define blue0-3k1 (hex-triplet->color% #x6C78DF))
(define blue1-3k1 (hex-triplet->color% #x5155BF))
(define blue2-3k1 (hex-triplet->color% #x4F4DA1))
(define teal-3k1 (hex-triplet->color% #x8ADCB3))
(define black-3k1 (hex-triplet->color% #x514F52))
(define grey-3k1 (hex-triplet->color% #x6C6685))
(define fog-3k1 (hex-triplet->color% #xDBCAC2))
(define title-3k1 (hex-triplet->color% #xF1E7DE))
(define author-3k1 (hex-triplet->color% #xE5E6E6))

(define untyped-pen-color yellow1-3k1)
(define deep-pen-color green2-3k1)
(define shallow-pen-color green0-3k1)
(define neutral-pen-color grey-3k1)
(define highlight-pen-color orange1-3k1)

(define code-brush-alpha 0.6)

(define untyped-brush-color (color%-update-alpha yellow1-3k1 code-brush-alpha))
(define deep-brush-color (color%-update-alpha green2-3k1 (- code-brush-alpha 0.05)))
(define shallow-brush-color (color%-update-alpha green0-3k1 code-brush-alpha))
(define neutral-brush-color fog-3k1)

(define background-color black-3k1)
(define spotlight-color teal-3k1)
(define title-text-color title-3k1)
(define body-text-color author-3k1)
(define subtitle-text-color highlight-pen-color)
(define code-text-color black)
(define success-color green1-3k1)
(define error-color red-3k1)

;; -----------------------------------------------------------------------------
;; --- text

(define (small-caps-style font)
  (cons 'no-combine (cons 'caps font)))

(define title-text-font (small-caps-style "TeX Gyre Pagella"))
(define title-text-size 70)

(define body-text-font "Lucida Grande")
(define body-text-size 38)
(define sub-body-text-size 30)

(define subtitle-text-font (small-caps-style body-text-font))

(define code-text-font "Inconsolata")
(define code-text-size 26)

(define (txt str*
             #:font [font body-text-font]
             #:style [style #f]
             #:size [size body-text-size]
             #:color [color body-text-color])
  (colorize
    (text (if (string? str*) str* (apply string-append str*))
          (if style (cons style font) font)
          size)
    color))

(define (ht . str*)
  (ht* str*))

(define (ht* str*)
  (txt str* #:font title-text-font #:size title-text-size #:color title-text-color))

(define (rt . str*)
  (rt* str*))

(define (rt* str*)
  (txt str* #:font body-text-font #:size body-text-size #:color body-text-color))

(define (rrt . str*)
  (rrt* str*))

(define (rrt* str*)
  (txt str* #:font body-text-font #:size sub-body-text-size #:color body-text-color))

(define (tt . str*)
  (tt* str*))

(define (tt* str*)
  (txt str* #:font code-text-font #:size code-text-size #:color code-text-color))

(define (code-line-append . pp*)
  (code-line-append* pp*))

(define (code-line-append* pp*)
  (apply vl-append code-line-sep pp*))

(define (text-line-append . pp*)
  (text-line-append* pp*))

(define (text-line-append* pp*)
  (apply vl-append text-line-sep pp*))

(define (result-bubble pp)
  (add-rounded-border
    #:radius 22
    #:x-margin small-x-sep #:y-margin tiny-y-sep
    #:frame-width 1 #:frame-color grey-3k1
    #:background-color fog-3k1
    pp))

(define (success-text str)
  (result-bubble
    (txt str #:font body-text-font #:size body-text-size #:color success-color)))

(define (error-text str)
  (result-bubble
    (txt str #:font body-text-font #:size body-text-size #:color error-color)))

;; -----------------------------------------------------------------------------
;; --- ???

(define default-line-width 4)
(define default-arrow-size 14)
(define default-line-color highlight-pen-color)

(define-runtime-path src "src")

(define (src-path . elem*)
  (src-path* elem*))

(define (src-path* elem*)
  (apply build-path src elem*))

(define (frame-bitmap ps #:w% [w% 9/10])
  (add-rounded-border
    #:radius 2 #:x-margin (w%->pixels 3/100) #:y-margin (h%->pixels 3/100)
    #:frame-width 2 #:frame-color neutral-pen-color
    #:background-color neutral-brush-color
    (bitmap (src-path ps))))

(struct code-arrow (src-tag src-find tgt-tag tgt-find start-angle end-angle start-pull end-pull style) #:transparent)

(define (add-code-arrow pp arrow
                        #:arrow-size [pre-arrow-size #f]
                        #:line-width [pre-line-width #f]
                        #:color [color default-line-color]
                        #:label [label (blank)]
                        #:x-adjust-label [x-label 0]
                        #:y-adjust-label [y-label 0]
                        #:hide? [hide? #false])
  (define line-width (or pre-line-width default-line-width))
  (define arrow-size (or pre-arrow-size default-arrow-size))
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
    #:x-adjust-label x-label
    #:y-adjust-label y-label
    #:hide-arrowhead? hide?
    #:style (code-arrow-style arrow)
    #:start-angle (code-arrow-start-angle arrow)
    #:end-angle (code-arrow-end-angle arrow)
    #:start-pull (code-arrow-start-pull arrow)
    #:end-pull (code-arrow-end-pull arrow)
    #:color color))

(define (add-code-line pp arrow
                       #:line-width [pre-line-width #f]
                       #:color [color default-line-color]
                       #:label [label (blank)]
                       #:x-adjust-label [x-label 0]
                       #:y-adjust-label [y-label 0]
                       #:hide? [hide? #false])
  (add-code-arrow pp arrow #:arrow-size 0
                  #:line-width pre-line-width #:color color #:label label
                  #:x-adjust-label x-label #:y-adjust-label y-label #:hide? hide?))

(define (add-code-arrow* pp . arrow*)
  (for/fold ((pp pp))
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

(define (add-background #:radius r pp)
  (add-rectangle-background
    #:radius r
    #:color white
    pp))

(define (X-codeblock pp* #:title [title #f] #:label [label #f] #:frame-color [frame-color #f] #:background-color [background-color #f])
  (define label-margin (if title (* 50/100 (pict-height title)) 0))
  (define (add-label-margin pp [extra 0]) (vl-append (+ extra label-margin) (blank) pp))
  (define radius 1)
  (define fw 5)
  (let* ((block-pict
          (add-background
            #:radius radius
            (add-rounded-border
              #:frame-color frame-color
              #:frame-width fw
              #:background-color background-color
              #:x-margin tiny-x-sep
              #:y-margin small-y-sep
              #:radius radius
              (add-label-margin (code-line-append* pp*)))))
         (title-pict (and title (rt title))))
    (if label
      (let ((block-pict (add-label-margin block-pict 2)))
        (ppict-do (if title-pict (lt-superimpose block-pict (ht-append 4 (blank) title-pict)) block-pict)
          #:go (coord 1/2 0 'ct) label))
      (if title-pict (vl-append 0 title-pict block-pict) block-pict))))

(define (conslang x y)
  (if x (list* (tt x) (blank) y) y))

(define (untyped-codeblock #:title [title #f] #:lang [lang "#lang racket"] . str*)
  (untyped-codeblock* #:title title (conslang lang (map tt str*))))

(define (untyped-codeblock* pp* #:title [title #f])
  (X-codeblock pp* #:title title #:frame-color untyped-pen-color #:background-color untyped-brush-color))

(define (shallow-codeblock #:title [title #f] #:lang [lang "#lang shallow"] . str*)
  (shallow-codeblock* #:title title (conslang lang (map tt str*))))

(define (shallow-codeblock* pp* #:title [title #f])
  (X-codeblock pp* #:title title #:frame-color shallow-pen-color #:background-color shallow-brush-color))

(define (deep-codeblock #:title [title #f] #:lang [lang "#lang deep"] . str*)
  (deep-codeblock* #:title title (conslang lang (map tt str*))))

(define (deep-codeblock* pp* #:title [title #f])
  (X-codeblock pp* #:title title #:frame-color deep-pen-color #:background-color deep-brush-color))

(define ex:shim-sep tiny-y-sep)
(define ex:output-x (w%->pixels 7/100))
(define ex:output-y small-y-sep)

(define (typed-above-untyped #:typed t-str*
                             #:untyped u-str*
                             #:ok? [ok? #true]
                             #:result [r-str* #f])
  (define t-pict (keyword-apply deep-codeblock '(#:lang) '(#f) t-str*))
  (define u-pict (keyword-apply untyped-codeblock '(#:lang) '(#f) u-str*))
  (define r-pict (if r-str* (if ok? (success-text r-str*) (error-text r-str*)) (blank)))
  (define tu-pict
    (let* ((t/sep
             (vl-append t-pict (tag-pict (blank ex:shim-sep ex:shim-sep) 't-shim)))
           (u/sep
             (vl-append (tag-pict (blank ex:shim-sep ex:shim-sep) 'u-shim) u-pict))
           (u/right
             (hc-append ex:shim-sep u/sep (tag-pict (blank ex:output-x 0) 'u-out-0)))
           (u/full
             (vr-append (tag-pict (blank 0 ex:output-y) 'u-out-1) u/right))
           (t/u (vl-append t/sep u/full)))
      (add-code-arrow
        t/u
        (code-arrow 't-shim rb-find 'u-shim rt-find (* 3/4 turn) (* 3/4 turn) 0 0 'solid))))
  (define r-out-arrow (code-arrow 'u-out-0 lb-find 'u-out-0 rb-find 0 0 0 0 'dot))
  (define tu/arrow
    (if ok?
      (add-code-arrow
        (add-code-line tu-pict r-out-arrow)
        (code-arrow 'u-out-0 rb-find 'u-out-1 rt-find (* 1/4 turn) (* 1/4 turn) 0 0 'dot))
      (add-code-arrow tu-pict r-out-arrow)))
  (ppict-do
    (let ((top-w (pict-width t-pict))
          (bot-w (+ (pict-width u-pict) ex:output-x (if r-pict (pict-width r-pict) 0))))
      (if (< top-w bot-w)
        (lt-superimpose tu/arrow (blank bot-w 0))
        tu/arrow))
    #:go (at-find-pict 'u-out-0 rc-find 'lc #:abs-x (* 2 ex:shim-sep))
    r-pict))

(define (untyped-above-typed #:untyped u-str* #:typed t-str* #:ok? [ok? #true] #:result [r-str* #f])
  (define u-pict (keyword-apply untyped-codeblock '(#:lang) '(#f) u-str*))
  (define t-pict (keyword-apply deep-codeblock    '(#:lang) '(#f) t-str*))
  (define r-pict (and r-str* (error-text r-str*)))
  (define tu-pict
    (let* ((t/u
            (vl-append
              u-pict
              (tag-pict (blank ex:shim-sep ex:shim-sep) 't-shim)
              (blank 0 ex:output-y)
              (tag-pict (blank ex:shim-sep ex:shim-sep) 'u-shim)
              t-pict
              (hb-append (blank ex:shim-sep ex:output-y)
                         (tag-pict (blank ex:output-x 0) 'u-out-0)))))
      (add-code-arrow
        t/u
        (code-arrow 't-shim rb-find 'u-shim rt-find (* 3/4 turn) (* 3/4 turn) 0 0 'solid))))
  (define r-out-arrow (code-arrow 'u-out-0 lb-find 'u-out-0 rb-find 0 0 0 0 'dot))
  (if r-pict
    (ppict-do
      (add-code-arrow tu-pict r-out-arrow)
      #:go (at-find-pict 'u-out-0 rc-find 'lc #:abs-x (* 2 ex:shim-sep))
      r-pict)
    tu-pict))

;; -----------------------------------------------------------------------------

(define (test-margin-slide)
  (define w/10 (w%->pixels 1/10))
  (define h/10 (h%->pixels 1/10))
  (define hl (colorize (hline w/10 1) body-text-color))
  (define vl (colorize (vline 1 h/10) body-text-color))
  (pslide
    #:go (coord slide-left slide-top 'lt)
    (vl-append 0 hl vl)
    #:go (coord slide-left slide-bottom 'lb)
    (vl-append 0 vl hl)
    #:go (coord slide-right slide-top 'rt)
    (vr-append 0 hl vl)
    #:go (coord slide-right slide-bottom 'rb)
    (vr-append 0 vl hl)
    #:go center-coord
    (cc-superimpose vl hl)))

(define (test-screenshot-slide)
  (pslide
    #:go center-coord
    (frame-bitmap "racket-users-ho-any.png" #:w% 5/10)))

(define (test-text-slide)
  (pslide
    #:go (coord text-left text-top 'lt)
    (ht "Three Kingdoms")
    (rt "Deep and Shallow types can coexist in a way that ...")
    (tt "#lang racket/base (define (f x) (add1 x))"))
  (void))

(define (test-code-slide)
  (pslide
    #:go (coord text-left text-top 'lt)
    (ht-append
      tiny-x-sep
      (untyped-codeblock
        ""
        "(define (add1 n)"
        "  (+ n 1))"
        "")
      (shallow-codeblock
        "(: add1 (-> Real Real))"
        "(define (add1 n)"
        "  (+ n 1))"
        "")
      (deep-codeblock
        "(: add1 (-> Real Real))"
        "(define (add1 n)"
        "  (+ n 1))"
        "")))
  (void))

;; =============================================================================

(define (sec:title)
  (define (st str #:size-- [size-- 0] #:color [color body-text-color])
    (txt str #:font subtitle-text-font #:size (- body-text-size size--) #:color color))
  (pslide
    ;; TODO sunset, forest background
    #:go (coord 1/2 4/10 'ct #:sep pico-y-sep)
    @ht{Deep and Shallow Types}
    (st "Thesis Defense" #:size-- 6 #:color subtitle-text-color)
    (blank 0 small-y-sep)
    @st{Ben Greenman    2020-12-19})
  (void))

(define ex:ok "Ok")
(define ex:error "Error")

(define ex:atom-lambda
  (typed-above-untyped
    #:typed '("f = λ(x:Int) x+1")
    #:untyped '("f f")
    #:ok? #true
    #:result #false))

(define ex:atom-flow
  (typed-above-untyped
    #:typed
    '("function f(x : number): number {"
      "  return x+1;"
      "}")
    #:untyped '("f(f);")
    #:ok?  #true
    #:result ex:ok))

(define ex:atom-retic
  (typed-above-untyped
    #:typed
    '("def f(x : Int)->Int:"
      "  return x + 1")
    #:untyped
    '("f(f)")
    #:ok? #false
    #:result ex:error))

(define ex:atom-tr
  (typed-above-untyped
    #:typed
    '("(: f (-> Integer Integer))"
      "(define (f x)"
      "  (+ x 1))")
    #:untyped
    '("(f f)")
    #:ok?  #false
    #:result ex:error))

(define ex:atom-nom
  (typed-above-untyped
    #:typed
    '("class F {"
      "  constructor () {}"
      "  fun apply(Int x) : Int {"
      "    return x + 1;"
      "  }"
      "}")
    #:untyped
    '("dyn f = new F();"
      "f.apply((dyn)f);")
    #:ok? #false
    #:result ex:error))

(define ex:pair-lambda
  (untyped-above-typed
    #:untyped
    '("v = (\"A\", 2)")
    #:typed
    '("((λ(x:Int×Int) (fst x)+1) v)")
    #:ok? #true
    #:result #false))

(define ex:pair-retic
  (untyped-above-typed
    #:untyped
    '("x = [\"A\", 2]")
    #:typed
    '("def g(y : Tuple(Int,Int)):"
      "  return y[0] + 1"
      ""
      "g(x)")
    #:ok? #f
    #:result ex:error))

(define ex:pair-tr
  (untyped-above-typed
    #:untyped
    '("(define x (list \"A\" 2))")
    #:typed
    '("(require/typed"
      "  [x (List Integer Integer)])"
      ""
      "(+ (first x) 1)")
    #:ok? #true
    #:result ex:error))

(define ex:pair-nom
  (untyped-above-typed
    #:untyped
    '("class Pair {"
      "  private fst;"
      "  private snd;"
      "  # ...."
      "}"
      ""
      "x = new Pair(\"A\", 2)")
    #:typed
    '("class IntPair {"
      "  private Int fst;"
      "  private Int snd;"
      "  # ...."
      "}"
      ""
      "((IntPair)x).fst + 1")
    #:ok? #f
    #:result ex:error))

(define ex:atom-title @rt{Example: Enforcing a Base Type})
(define ex:pair-title @rt{Example: Enforcing a Data Structure})

(define (sec:example)
  ;; TODO
  ;; - [ ] still looks very basic ... very primary ... can enhance?
  ;; - [ ] add lang titles
  (pslide
    #:go heading-coord-left
    ex:atom-title
    #:go text-coord-mid
    (text-line-append
      @rrt{What happens when a typed function}
      @rrt{expects an integer}
      @rrt{but receives something else?})
    (blank 0 small-y-sep)
    ex:atom-lambda)
  (pslide
    #:go heading-coord-left
    ex:atom-title
    #:go text-coord-mid
    (scale-to-text
      (make-2table
        #:row-sep small-y-sep
        #:col-sep small-x-sep
        #:row-align lt-superimpose
        (list
          (cons ex:atom-flow ex:atom-retic)
          (cons ex:atom-tr ex:atom-nom)))))
  (pslide
    #:go heading-coord-left
    ex:pair-title
    #:go text-coord-mid
    (text-line-append
      @rrt{What happens when a typed function}
      @rrt{expects a pair of numbers}
      @rrt{but receives a different pair?})
    (blank 0 small-y-sep)
    ex:pair-lambda)
  (void))

(module+ main
  (set-page-numbers-visible! #false)
  (set-spotlight-style! #:size 60 #:color (color%-update-alpha spotlight-color 0.6))
  ;; --
  (parameterize ((current-slide-assembler (slide-assembler/background (current-slide-assembler) #:color background-color)))
    ;(test-margin-slide)
    ;(test-screenshot-slide)
    (sec:title)
    (sec:example)
    ;(sec:big-picture)
    ;(sec:shallow-fast)
    ;(sec:shallow-expressive)
    ;(sec:shallow-simple)
    ;(sec:shallow-bad)
    ;(sec:cometh-soon)
    (pslide)
    (void))
  (void))

;; =============================================================================

(module+ raco-pict (provide raco-pict)
  (define aspect 'fullscreen)
  (define-values [client-w client-h]
    (apply values (for/list ((f (in-list (list get-client-w get-client-h)))) (f #:aspect aspect))))
(define raco-pict
  (ppict-do (filled-rectangle client-w client-h #:draw-border? #f #:color background-color)

    #:go heading-coord-left
    @rt{Example: Enforcing a Data Structure}
    #:go text-coord-mid
    (scale-to-text
      (make-2table
        #:row-sep small-y-sep
        #:col-sep small-x-sep
        #:row-align lt-superimpose
        (list
          (cons (blank) ex:pair-retic)
          (cons ex:pair-tr ex:pair-nom))))


  )))
