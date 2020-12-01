#lang at-exp slideshow

;; Defense slides, Deep and Shallow types.
;;  1 hour presentation? roughly

;; TODO
;; - [X] get colors ... three kingdoms?
;; - [X] pick fonts
;; - [X] jfp example, slides ... take from diss pict
;; - [X] outline, on paper
;; - [ ] outline on slides
;; - [ ] technics in order!
;; - [ ] table pict
;; - [ ] warring states pict, D S E + concrete, pyret
;;   - [ ] similar pict for languages? for, before the design-space analysis?
;;   - [ ] organize along perf + design dimensions
;;   - [ ] deep perf = bumpy, dangerous bend (or R G Y stripes)
;;   - [ ] shallow perf = bumpy, dangerous bend
;;   - [ ] erased perf = "untyped" flag ... for better and worse
;;   - [ ] interop "city" at D S border
;; - [ ] thesis + supports pict
;; - [ ] D S U picts, the safe one and others
;; - [ ] sunrise, green, theme for "published" half
;; - [ ] moonlight, blue, theme for "new" half
;;
;; - [ ] headlines ... gt terrific
;; - [ ] two methods (major contributions)
;;   - [ ] perf ... LHS 
;;   - [ ] design ... RHS 
;; - [ ]
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
(define item-line-sep (h%->pixels 35/1000))

(define codeblock-x-sep (w%->pixels 4/100))
(define codeblock-y-sep (h%->pixels 4/100))

;; =============================================================================
;; --- coordinates

(define slide-top 4/100)
(define slide-left 4/100)
(define slide-right (- 1 slide-left))
(define slide-bottom 92/100)
(define text-left (* 3/2 slide-left))
(define text-right (- 1 text-left))
(define text-top (* 5 slide-top))
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
(define title-coord-mid (coord 1/2 25/100 'ct #:sep pico-y-sep))

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

(define (rt* str* #:color [color body-text-color])
  (txt str* #:font body-text-font #:size body-text-size #:color color))

(define (st . str*)
  (rt* str* #:color subtitle-text-color))

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

(define (item-line-append . pp*)
  (item-line-append* pp*))

(define (item-line-append* pp*)
  (apply vl-append item-line-sep pp*))

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

(define (src-bitmap ps)
  (bitmap (src-path ps)))

(define (frame-bitmap ps #:w% [w% 9/10])
  (add-rounded-border
    #:radius 2 #:x-margin (w%->pixels 3/100) #:y-margin (h%->pixels 3/100)
    #:frame-width 2 #:frame-color neutral-pen-color
    #:background-color neutral-brush-color
    (let ((pp (src-bitmap ps)))
      (scale-to-fit pp (w%->pixels w%) (pict-height pp)))))

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

(define ex-shim-sep tiny-y-sep)
(define ex-output-x (w%->pixels 7/100))
(define ex-output-y small-y-sep)

(define (typed-above-untyped #:typed t-str*
                             #:untyped u-str*
                             #:ok? [ok? #true]
                             #:result [r-str* #f])
  (define t-pict (keyword-apply deep-codeblock '(#:lang) '(#f) t-str*))
  (define u-pict (keyword-apply untyped-codeblock '(#:lang) '(#f) u-str*))
  (define r-pict (if r-str* (if ok? (success-text r-str*) (error-text r-str*)) (blank)))
  (define tu-pict
    (let* ((t/sep
             (vl-append t-pict (tag-pict (blank ex-shim-sep ex-shim-sep) 't-shim)))
           (u/sep
             (vl-append (tag-pict (blank ex-shim-sep ex-shim-sep) 'u-shim) u-pict))
           (u/right
             (hc-append ex-shim-sep u/sep (tag-pict (blank ex-output-x 0) 'u-out-0)))
           (u/full
             (vr-append (tag-pict (blank 0 ex-output-y) 'u-out-1) u/right))
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
          (bot-w (+ (pict-width u-pict) ex-output-x (if r-pict (pict-width r-pict) 0))))
      (if (< top-w bot-w)
        (lt-superimpose tu/arrow (blank bot-w 0))
        tu/arrow))
    #:go (at-find-pict 'u-out-0 rc-find 'lc #:abs-x (* 2 ex-shim-sep))
    r-pict))

(define (untyped-above-typed #:untyped u-str* #:typed t-str* #:ok? [ok? #true] #:result [r-str* #f])
  (define u-pict (keyword-apply untyped-codeblock '(#:lang) '(#f) u-str*))
  (define t-pict (keyword-apply deep-codeblock    '(#:lang) '(#f) t-str*))
  (define r-pict (and r-str* (error-text r-str*)))
  (define tu-pict
    (let* ((t/u
            (vl-append
              u-pict
              (tag-pict (blank ex-shim-sep ex-shim-sep) 't-shim)
              (blank 0 ex-output-y)
              (tag-pict (blank ex-shim-sep ex-shim-sep) 'u-shim)
              t-pict
              (hb-append (blank ex-shim-sep ex-output-y)
                         (tag-pict (blank ex-output-x 0) 'u-out-0)))))
      (add-code-arrow
        t/u
        (code-arrow 't-shim rb-find 'u-shim rt-find (* 3/4 turn) (* 3/4 turn) 0 0 'solid))))
  (define r-out-arrow (code-arrow 'u-out-0 lb-find 'u-out-0 rb-find 0 0 0 0 'dot))
  (if r-pict
    (ppict-do
      (add-code-arrow tu-pict r-out-arrow)
      #:go (at-find-pict 'u-out-0 rc-find 'lc #:abs-x (* 2 ex-shim-sep))
      r-pict)
    tu-pict))

(define ruler-pict
  (frame-bitmap "ruler.jpg" #:w% 2/10))

(define scale-pict
  (frame-bitmap "scale.jpeg" #:w% 2/10))

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
  ;; TODO sunset, forest background
  (define (st str #:size-- [size-- 0] #:color [color subtitle-text-color])
    (txt str #:font subtitle-text-font #:size (- body-text-size size--) #:color color))
  (define t-pict @ht{Deep and Shallow Types})
  (define st-pict (st "Thesis Defense" #:size-- 6 #:color subtitle-text-color))
  (define a-sep (blank 0 small-y-sep))
  (define a-pict @st[#:color body-text-color]{Ben Greenman    2020-12-19})
  (define (title-slide)
    (pslide
      #:go title-coord-mid t-pict
      st-pict a-sep a-pict))
  (title-slide)
  (pslide
    #:go text-coord-mid
    (vl-append
      small-y-sep
      @rrt{Matthias Felleisen} @rrt{Amal Ahmed} @rrt{Jan Vitek}
      @rrt{Shriram Krishnamurthi} @rrt{Fritz Henglein} @rrt{Sam Tobin-Hochstadt}))
  (title-slide)
  (void))

(define ex-ok "Ok")
(define ex-error "Error")

(define ex-atom-lambda
  (typed-above-untyped
    #:typed '("f = λ(x:Int) x+1")
    #:untyped '("f f")
    #:ok? #true
    #:result #false))

(define ex-atom-flow
  (typed-above-untyped
    #:typed
    '("function f(x : number): number {"
      "  return x+1;"
      "}")
    #:untyped '("f(f);")
    #:ok?  #true
    #:result ex-ok))

(define ex-atom-retic
  (typed-above-untyped
    #:typed
    '("def f(x : Int)->Int:"
      "  return x + 1")
    #:untyped
    '("f(f)")
    #:ok? #false
    #:result ex-error))

(define ex-atom-tr
  (typed-above-untyped
    #:typed
    '("(: f (-> Integer Integer))"
      "(define (f x)"
      "  (+ x 1))")
    #:untyped
    '("(f f)")
    #:ok?  #false
    #:result ex-error))

(define ex-atom-nom
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
    #:result ex-error))

(define ex-pair-lambda
  (untyped-above-typed
    #:untyped
    '("v = (\"A\", 2)")
    #:typed
    '("((λ(x:Int×Int) (fst x)+1) v)")
    #:ok? #true
    #:result #false))

(define ex-pair-retic
  (untyped-above-typed
    #:untyped
    '("x = [\"A\", 2]")
    #:typed
    '("def g(y : Tuple(Int,Int)):"
      "  return y[0] + 1"
      ""
      "g(x)")
    #:ok? #f
    #:result ex-error))

(define ex-pair-tr
  (untyped-above-typed
    #:untyped
    '("(define x (list \"A\" 2))")
    #:typed
    '("(require/typed"
      "  [x (List Integer Integer)])"
      ""
      "(+ (first x) 1)")
    #:ok? #true
    #:result ex-error))

(define ex-pair-nom
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
    #:result ex-error))

(define ex-atom-title @rt{Example: Enforcing a Base Type})
(define ex-pair-title @rt{Example: Enforcing a Data Structure})

(define (sec:example)
  ;; TODO
  ;; - [ ] still looks very basic ... very primary ... can enhance?
  ;; - [ ] add lang titles
  (pslide
    #:go heading-coord-left
    ex-atom-title
    #:go text-coord-mid
    (text-line-append
      @rrt{What happens when a typed function}
      @rrt{expects an integer}
      @rrt{but receives something else?})
    (blank 0 small-y-sep)
    ex-atom-lambda)
  (pslide
    #:go heading-coord-left
    ex-atom-title
    #:go text-coord-mid
    (scale-to-text
      (make-2table
        #:row-sep small-y-sep
        #:col-sep small-x-sep
        #:row-align lt-superimpose
        (list
          (cons ex-atom-flow ex-atom-retic)
          (cons ex-atom-tr ex-atom-nom)))))
  (pslide
    #:go heading-coord-left
    ex-pair-title
    #:go text-coord-mid
    (text-line-append
      @rrt{What happens when a typed function}
      @rrt{expects a pair of numbers}
      @rrt{but receives a different pair?})
    (blank 0 small-y-sep)
    ex-pair-lambda)
  (void))

(define (sec:intro)
  (pslide
    #:go center-coord
    ;; Ershov "And I have long since taught myself to think that if I reproduce
    ;; somebody's guess in my work, I should not regret not having been the
    ;; first, but, on the contrary, should always bear it in mind that it is a
    ;; major stimulus: since a similar idea has occured to me living thousands
    ;; of kilometers away, it means that there really is something in it"
    ;; _from hidden places knowledge i obtained_ k. Levitin

    @rrt{if I reproduce somebody's guess in my work .... me living thousands of}
    @rrt{kilometers away ... it means that there really is something in it}
    (blank)
    @rrt{Ershov, 1983}
    #:go heading-coord-right
    (frame-bitmap "ershov.png" #:w% 2/10))
  (pslide
    #:go heading-coord-left
    @rt{By that measure, GT landmark idea}
    #:go text-coord-mid
    @rrt{4x insights}
    @rrt{- Siek Taha   SFP 2006}
    @rrt{- Gronski Knowles Tomb Freund Flanagan   SFP 2006}
    @rrt{- Tobin-Hochstadt Felleisen   DLS 2006}
    @rrt{- Matthews Findler   POPL 2007, TOPLAS 2009})
  (pslide
    #:go heading-coord-left
    @rt{Basic Ideas}
    #:go text-coord-mid
    ;; NOTE focus on the ties between T/U, the boundaries
    @rrt{goal = unite two movements in PL design}
    @rrt{typed, statically typed}
    @rrt{untyped, dynamically typed})
  (pslide
    #:go heading-coord-left
    @rt{Lively Space}
    ;; 0. mccarthy?
    ;; 1. common lisp, strongtalk, grey etal,
    ;; 2. gradual typing (mixed-typed?)
    ;; 3. GT bib
    ;; languages vs papers?
    #:go text-coord-mid
    @rrt{4 originals, directly gave rise to a few languages}
    ;; --> TR retic pycket grift
    @rrt{olde pre-original}
    ;; --> commonlisp strongtalk 
    @rrt{many more, aca + ind labs}
    ;; --> actionscript mypy flow hack pyre pytype rdl typescript typedclojure
    ;;     typedlua gradualtalk tpd pyret grace pallene strongscript thorn c+
    ;;     dart2 nom safets ts*
    @rrt{over 200 papers in gradual-typing-bib}
    #:next ;; not shoulders, rather elephant
    #:go center-coord (frame-bitmap "elephant.jpg" #:w% 6/10) @rrt{Image credit: Hans Moller}
    )
  (pslide
    #:go heading-coord-left
    @rt{Example, disagreement} ;; titles are so difficult
    ;; [[ recall, can mix typed and untyped ]]
    ;; basic example, typed first = static error
    ;;  f(n : number) { n + 1} ... f("A")
    #:go text-coord-mid
    @rrt{number across boundary} ;; get letter
    @rrt{some mixed-typed reject, others allow}
    @rrt{allowance = funny because contradicts the type}
    @rrt{you'd expect all to agree on this basic one})
  (pslide
    #:go heading-coord-left
    @rt{Example 2: it gets worse}
    ;; TODO what are the goals, first of all?
    ;;  what to say about the languages here
    ;; - change "number" to "boxof number"
    ;; - call untyped function with the box
    ;; -  .... result depends on where box accessed (yes thats easy enough)
    #:go text-coord-mid
    ;; f(n : box(number)) { g(n) } ... f(box("A")) ... g(n) { (unbox n) + 1 }
    @rrt{pair across boundary}
    @rrt{some mixed-typed reject early, others late, others never})
  (pslide
    #:go heading-coord-left
    @rt{My Work, understanding the space}
    ;; back to chaos +? elephant
    #:go center-coord
    (ht-append
      10
      (vl-append
        10
        ruler-pict
        @rrt{performance})
      (vl-append
        10
        scale-pict
        @rrt{guarantees}))
    @rrt{implications for (1) perf and for (2) what types mean}
    ;; with improved understanding, compromise ... preview filled-in space
    @rrt{thesis preview: deep and shallow can interoperate})
  ;; now lets pursue the two threads, start with performance
  (void))

(define (sec:perf)
  ;; maybe ... Natural can be rocky, Transient sloped, ... ?
  ;;  be careful, have 2 planes perf@impl design@model
  ;;
  ;; lets begin with perf, because it comes first
  ;;  want to RUN these mixed-typed programs
  ;; and focus on TR ... skull flag? danger?
  (pslide
    ;; performance first
    ;; ... and dive right in to story of typed racket
    ;;  the FIRST big mixed language where types mean something
    ;;  in the beginning implemented and it was good
    ;; [ ] mag glass?
    ;; [ ] pirate flag
    #:go heading-coord-left
    @rt{Performance: Story of Typed Racket}
    #:go text-coord-mid
    ;; black flag, zoom in,
    ;;  but black-flag = exaggeration ... have lots of programmers some good some bad MANY q-mark
    ;;  ... pirate fog, islands?
    @rrt{some programs fine}
    @rrt{others not fine})
  (pslide
    #:go heading-coord-left
    @rt{Trie Example}
    @rrt{JBC q tr-pdfs, 1200x slowdown}
    ;; TODO
    ;; - blur JBC / entire message
    ;; - highlight "burned"
    ;; - highlight "12sec vs 0ms"
    (frame-bitmap "trie-racket-users.png" #:w% 4/10))
  (pslide
    #:go heading-coord-left
    @rt{Story of Typed Racket}
    ;; back to "woods" with some black flags
    @rrt{more questions, more hazards}
    @rrt{evident problem, unclear size and severity})
  (pslide
    #:go heading-coord-left
    @rt{GTP Benchmarks}
    #:go text-coord-mid
    @rrt{developed benchmarks, with Dan Matthias Asumu Max}
    @rrt{adapted from real programs, convert to TR})
  (pslide
    #:go heading-coord-left
    @rt{GTP Benchmarks, table}
    #:go text-coord-mid
    (frame-bitmap "gtp-size.png"))
  (pslide
    #:go heading-coord-left
    @rt{GTP Benchmarks, example}
    #:go text-coord-mid
    @rrt{size shape ... from site}
    (frame-bitmap "jpeg-description.png" #:w% 4/10))
  (pslide
    #:go heading-coord-left
    @rt{What to do with benchmarks?}
    #:go text-coord-mid
    @rrt{central question}
    @rrt{have 21 programs, or 42 if typed and untyped}
    ;; CAN get 21 / 42 numbers out. (That's what others do.) Do we learn much? NO
    @rrt{but --- ignores entire mixed space})
  (pslide
    #:go heading-coord-left
    @rt{Systematic Method}
    #:go text-coord-mid
    ruler-pict)
  (pslide
    #:go heading-coord-left
    @rt{Perf. Method: Study All Configurations}
    #:go text-coord-mid
    @rrt{program N points => 2^N configs}
    ;; recall JPEG picture, its N example, migratable vs contextual
    @rrt{some good some danger, measure systematically})
  (pslide
    ;; gotta work for all programs, more than the tiny N=4 example
    #:go heading-coord-left
    @rt{How to Comprehend the Data?}
    #:go text-coord-mid
    @rrt{small N = unreadable space}
    @rrt{modest N = infeasible}
    @rrt{answer to both comes from a careful consumer-first question})
  (pslide
    #:go heading-coord-left
    @rt{D-deliverable measure}
    #:go text-coord-mid
    @rrt{enduser / decision maker, set Dx}
    @rrt{worst-case tolerate ... may be 2x for ship, may be 10x for dev}
    @rrt{with D every config is yes or no, deliverable or not}
    ;; now have one number that says a lot about the space (relative to param)
    @rrt{compressed to number, easy to interpret a 60% result here}
    @rrt{also can sample})
  (pslide
    #:go heading-coord-left
    @rt{Approximate D-deliverable}
    #:go text-coord-mid
    @rrt{easy to approx. with interval}
    @rrt{knowledge vs truth, pick 1 random then D% good}
    @rrt{well then D% in a few big samples close enough}
    ;; no vis, no need for details
    @rrt{success with linear samples})
  (pslide
    #:go heading-coord-left
    @rt{Method Summary}
    ;; it's a contribution
    #:go text-coord-mid
    @rrt{several programs, fully type}
    @rrt{systematically collect, exhaustive or approx}
    @rrt{study with range of D})
  (pslide
    #:go heading-coord-left
    @rt{D-deliv Example}
    ;; how to read
    #:go text-coord-mid
    @rrt{jpeg plot})
  (pslide
    #:go heading-coord-left
    @rt{Two Evaluations}
    #:go text-coord-mid
    @rrt{story of TR}
    @rrt{story of RP too})
  (sec:perf:tr)
  (sec:perf:rp)
  (void))

(define (sec:perf:tr)
  (pslide
    #:go heading-coord-left
    @rt{TR evaluation}
    #:go text-coord-mid
    @rrt{20 benchmarks, recall table})
  (pslide
    #:go heading-coord-left
    @rt{TR typical result}
    #:go text-coord-mid
    @rrt{or, a few results}
    @rrt{yikes too slow}
    @rrt{few are 10x deliv})
  (pslide
    #:go heading-coord-left
    @rt{TR, map}
    #:go text-coord-mid
    @rrt{decorate, slow})
  (void))

(define (sec:perf:rp)
  (pslide
    #:go heading-coord-left
    @rt{map => RP}
    #:go text-coord-mid
    @rrt{focus, new})
  (pslide
    #:go heading-coord-left
    @rt{RP evaluation ... mirror TR}
    #:go text-coord-mid
    @rrt{benchmarks, size purpose})
  (pslide
    #:go heading-coord-left
    @rt{RP typical result}
    #:go text-coord-mid
    @rrt{lots 10x deliv}
    @rrt{much faster})
  (void))

(define (sec:design)
  ;; Q work in the survey, early, about whether transient is plain "better"?
  (pslide
    #:go heading-coord-left
    @rt{RP vs TR night and day}
    #:go text-coord-mid
    @rrt{map again, side-by-side}
    @rrt{quite a surprise, both sound; types mean something})
  (pslide
    ;; the TR results came first, and our reaction was "sound dead"?
    #:go heading-coord-left
    @rt{Is sound dead?}
    #:go text-coord-mid
    @rrt{NO, thunderous, from research community}
    @rrt{several piped up (across the map)}
    @rrt{papers papers all satisfy all properties})
  (pslide
    #:go heading-coord-left
    @rt{Confusion}
    #:go text-coord-mid
    @rrt{hang on, recall different behaviors})
  (pslide
    #:go heading-coord-left
    @rt{Example 1}
    #:go text-coord-mid
    @rrt{all agree on the number-boundary question})
  (pslide
    #:go heading-coord-left
    @rt{Example 2}
    #:go text-coord-mid
    @rrt{disagree on box-boundary}
    @rrt{concrete cannot even express, every box starts with a type})
  (pslide
    #:go heading-coord-left
    @rt{CM example, functions}
    #:go text-coord-mid
    @rrt{ah missing error})
  (pslide
    #:go heading-coord-left
    @rt{How can they all satisfy all when clearly different?}
    ;; back up to the map
    #:go text-coord-mid
    @rrt{weak properties!}
    @rrt{led to design space analysis}
    @rrt{move UP to design map})
  (pslide
    ;; first step, close review of TS
    #:go heading-coord-left
    @rt{Sound}
    #:go text-coord-mid
    @rrt{sound refers to type sound, classic theorem}
    @rrt{none satisfy, must generalize}
    @rrt{unsound, extreme end, ignoring all types})
  (pslide
    #:go heading-coord-left
    @rt{F-type soundness}
    #:go text-coord-mid
    @rrt{our first step, study sound space}
    @rrt{articulate the parameters}
    ;; transient removal is WEAK but thats ok, don't worry for defense
    @rrt{removes transient}
    @rrt{still others lumped together})
  (pslide
    #:go heading-coord-left
    @rt{Natural vs Forgetful}
    #:go text-coord-mid
    ;; recall CM example
    ;; temporary vs permanent types
    @rrt{example, different behaviors, prim error vs boundary})
  (pslide
    #:go heading-coord-left
    @rt{Derived channel, do types protect?}
    #:go text-coord-mid
    @rrt{CM => yes}
    @rrt{TS =/> yes})
  (pslide
    #:go heading-coord-left
    @rt{Map again, CM helps partition}
    #:go text-coord-mid
    @rrt{that}
    @rrt{X})
  (pslide
    ;; furthermore, CM-analysis-framework helps measure quality of errors
    #:go heading-coord-left
    @rt{Blame, on map}
    ;; finer distinctions even in non-CM space
    #:go text-coord-mid
    @rrt{CM framework, study quality of error outputs}
    @rrt{two more properties (no details?)})
  (pslide
    #:go heading-coord-left
    @rt{Error Preorder}
    ;; hm, gotta predict these properties / rows with a checklist somehow ... map needs to stay low
    #:go text-coord-mid
    @rrt{eager vs lazy example} ;; too many examples? how else to mention the preorder?
    @rrt{now can partition space})
  (pslide
    #:go heading-coord-left
    @rt{Map, color in}
    #:go text-coord-mid
    @rrt{TS; F-TS; CM; BS BC; preorder}
    @rrt{deep vs shallow sound types})
  (pslide
    #:go heading-coord-left
    @rt{Table view}
    #:go text-coord-mid
    @rrt{from jfp})
  (void))

(define (sec:thesis)
  (pslide
    #:go heading-coord-left
    @rt{Map guarantees and perf}
    #:go text-coord-mid
    @rrt{evident 2-way tradeoff}
    @rrt{really its 3 ways, guarantees performance expressiveness}
    @rrt{what to do?}
    @rrt{improve endpoints, several workers feltey, grift, pycket, vitousek}
    ;; can interact, don't lose anything, and really what you want
    @rrt{my thesis, in full})
  (sec:thesis:motivation)
  (pslide
    #:go heading-coord-left
    @rt{Entering unpublished results}
    ;; moonlight, flows left-to-right?
    #:go text-coord-mid
    @rrt{XXX})
  (sec:thesis:model)
  (sec:thesis:implementation)
  (void))

(define (sec:thesis:motivation)
  (pslide
    ;; HM may not need example here, its clear we can toggle ... and too early to discuss transient racket
    #:go heading-coord-left
    @rt{Sieve performance ?}
    #:go text-coord-mid
    @rrt{hm, can try to recall an example}
    @rrt{but real win is when deep cannot budge}
    @rrt{fully-typed back to deep})
  (pslide
    #:go heading-coord-left
    @rt{Shallow removes errors}
    #:go text-coord-mid
    @rrt{lets set-box program run, overall simpler}
    @rrt{very few programs that we can't run --- at end})
  (pslide
    #:go heading-coord-left
    @rt{Shallow does not change behavior}
    #:go text-coord-mid
    @rrt{index-of example, those pesky wrappers})
  (pslide
    #:go heading-coord-left
    @rt{Summary}
    #:go text-coord-mid
    @rrt{wow speed expressiveness correctness lets go})
  (void))

(define (sec:thesis:model)
  ;; HANG ON the contribution is the scaled-up transient, not so much the model
  ;; - more types, subtyping
  ;; - optimizations
  ;; - no dyn = audit check sites
  (pslide
    #:go heading-coord-left
    @rt{Model}
    #:go text-coord-mid
    @rrt{first step, prototype / model}
    @rrt{prove deep deep, shallow shallow}
    @rrt{explore optimizations, show picture (move "wrap" boundary), details at end}
    @rrt{results = cm, f-ts, compilation})
  (pslide
    #:go heading-coord-left
    @rt{Syntax}
    #:go text-coord-mid
    @rrt{show e, but focus on boundaries, 3 module-kinds}
    @rrt{compile to 3 check-kinds})
  (pslide
    #:go heading-coord-left
    @rt{Theorems}
    #:go text-coord-mid
    @rrt{TS}
    @rrt{CM})
  (pslide
    #:go heading-coord-left
    @rt{Complilation}
    #:go text-coord-mid
    @rrt{clear target future work}
    @rrt{thanks fritz})
  (void))

(define (sec:thesis:implementation)
  (pslide
    #:go heading-coord-left
    @rt{Code: Shallow TR}
    #:go text-coord-mid
    @rrt{reuse compiler pipeline}
    @rrt{choice of shapes, rec shapes}
    @rrt{insert checks}
    @rrt{reuse optimizer when possible, another future work})
  ;; anything else to say?
  (pslide
    #:go heading-coord-left
    @rt{Perf data} ;; symmetry wth model results
    #:go text-coord-mid
    @rrt{best of both, overhead plots}
    @rrt{paths story, maybe show the 6 small ones}
    @rrt{any more})
  (void))

(define (sec:conclusion)
  (pslide
    #:go heading-coord-left
    @rt{Summarize thesis support}
    #:go text-coord-mid
    @rrt{review thesis}
    @rrt{proved in model}
    @rrt{validated with implementation}
    @rrt{coming out soon})
  (pslide
    #:go heading-coord-left
    @ht{Contributions}
    #:go title-coord-mid
    ;; icons at bottom?
    (item-line-append
      (hb-append @st{1. } @rt{performance analysis method})
      (hb-append @st{2. } @rt{design analysis method})
      (hb-append @st{3. } @rt{scaled-up Transient})
      (hb-append @st{4. } @rt{Deep + Shallow language}))
    #:go (coord 1/2 75/100 'cc)
    (hc-append small-x-sep scale-pict ruler-pict @rt{?} @rt{?}))
  #;(pslide
    #:go heading-coord-left
    @rt{Contributions}
    #:go text-coord-mid
    @rrt{perf method}
    @rrt{design method}
    @rrt{transient adapted} ;; hmm.. kind of a surprise right?
    @rrt{deep and shallow interop})
  (void))

(define (sec:extra)
  (pslide
    #:go heading-coord-left
    @rt{Blame for transient / shallow}
    #:go text-coord-mid
    @rrt{perf omits blame}
    @rrt{scary results after implemesting}
    @rrt{confirmed in retic}
    (blank)
    (frame-bitmap "transient-blame.png" #:w% 5/10))
  (pslide
    #:go heading-coord-left
    @rt{D S U boundaries}
    #:go text-coord-mid
    @rrt{review pictures}
    @rrt{each in detail}
    @rrt{challenges, dead-ends}
    (frame-bitmap "dsu-optfail.png" #:w% 4/10))
  (pslide
    #:go heading-coord-left
    @rt{Completion Optimize}
    #:go text-coord-mid
    @rrt{transient strategy}
    @rrt{easy redundancy}
    @rrt{occurrence type to remove extra checks})
  (pslide
    #:go heading-coord-left
    @rt{Shallow cannot run 1/2}
    #:go text-coord-mid
    @rrt{all type, unrestricted, oh that inst}
    (frame-bitmap "transient-fail-all.png" #:w% 5/10))
  (pslide
    #:go heading-coord-left
    @rt{Shallow cannot run 2/2}
    #:go text-coord-mid
    @rrt{occurrence type at boundary}
    (frame-bitmap "transient-fail-occurrence.png" #:w% 5/10))
  (pslide
    #:go heading-coord-left
    @rt{Bonus fixes}
    #:go text-coord-mid
    @rrt{shallow for debugging deep}
    (blank)
    (frame-bitmap "transient-bugfix.png" #:w% 5/10))
  (pslide
    #:go heading-coord-right
    @rt{Gradual Guarantee}
    #:go text-coord-mid
    (vr-append
      pico-y-sep
      (frame-bitmap "svcb-snapl-2015.png" #:w% 6/10)
      @rrt{Siek, Vitousek, Cimini, Boyland   SNAPL 2015})
    #:next
    (blank)
    @rrt{- only concerns the Dyn type}
    @rrt{- satisfied by Natural, Optional, and the rest})
  (pslide
    #:go heading-coord-left
    @rt{Gradual Type Theory (GTT)}
    #:go text-coord-mid
    @rrt{Max S. New, 2020 dissertation, Northeastern}
    @rrt{next at Michigan}
    ;; heaven, geometric imagery ... I am the anti-max
    @rrt{forget the landscape, design from first principles}
    @rrt{Natural only satisfactory answer})
  (void))

(module+ main
  (set-page-numbers-visible! #false)
  (set-spotlight-style! #:size 60 #:color (color%-update-alpha spotlight-color 0.6))
  ;; --
  (parameterize ((current-slide-assembler (slide-assembler/background (current-slide-assembler) #:color background-color)))
    ;(test-margin-slide)
    ;(test-screenshot-slide)
    ;(sec:example)
    (sec:title)
;    (sec:intro)
;    (sec:perf)
;    (sec:design)
;    (sec:thesis)
    (sec:conclusion)
    (pslide)
    (sec:extra)
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
    @ht{Contributions}
    #:go title-coord-mid
    (item-line-append
      (hb-append @st{1. } @rt{performance analysis method})
      (hb-append @st{2. } @rt{design analysis method})
      (hb-append @st{3. } @rt{scaled-up Transient})
      (hb-append @st{4. } @rt{Deep + Shallow language}))
    #:go (coord 1/2 75/100 'cc)
    (hc-append small-x-sep scale-pict ruler-pict @rt{?} @rt{?})

;    #:go heading-coord-left
;    @rt{Example: Enforcing a Data Structure}
;    #:go text-coord-mid
;    (scale-to-text
;      (make-2table
;        #:row-sep small-y-sep
;        #:col-sep small-x-sep
;        #:row-align lt-superimpose
;        (list
;          (cons (blank) ex-pair-retic)
;          (cons ex-pair-tr ex-pair-nom))))


  )))
