#lang at-exp slideshow

;; Defense slides, Deep and Shallow types.
;;  1 hour presentation? roughly

;; TODO
;; - [X] get colors ... three kingdoms?
;; - [X] pick fonts
;; - [X] jfp example, slides ... take from diss pict
;; - [X] outline, on paper
;; - [X] outline on slides
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
;; - [ ] NOTE: cannot ask if Java etc. is deep, its an FFI/Interop question
;; - [ ]
;; - [ ]

(require
  file/glob
  pict pict/shadow pict/face
  pict-abbrevs pict-abbrevs/slideshow gtp-pict
  (only-in gtp-util pct rnd)
  ppict/2
  racket/draw
  racket/list
  racket/string
  racket/format
  racket/runtime-path
  slideshow/code
  (only-in racket/math exact-floor)
  (only-in math/statistics mean)
  (only-in greenman-thesis stransient)
  (only-in greenman-thesis/jfp-2019/main
    MAX-OVERHEAD
    transient-rkt-version
    benchmark-name->performance-info)
  (prefix-in rp:
    (only-in greenman-thesis/pepm-2018/main
      benchmark-name->performance-info))
  gtp-plot/configuration-info gtp-plot/plot gtp-plot/typed-racket-info gtp-plot/reticulated-info gtp-plot/performance-info
  plot/no-gui (except-in plot/utils min* max*))

(module+ test
  (require rackunit))

(define turn revolution)

(define-runtime-path cache-dir "with-cache")
(define-runtime-path src "src")

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

(define lattice-x-sep small-x-sep)
(define lattice-y-sep tiny-y-sep)

(define (hsep h)
  (blank 0 h))

(define (wsep w)
  (blank w 0))

(define (hshift n pp)
  (if (< 0 n)
    (vc-append n (blank) pp)
    (vc-append (- n) pp (blank))))

(define (wshift n pp)
  (if (< 0 n)
    (hc-append n (blank) pp)
    (hc-append (- n) pp (blank))))

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
(define contribution-x 49/100)

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
(define title-coord-left (coord text-left 25/100 'ct #:sep pico-y-sep))
(define title-coord-mid (coord 1/2 25/100 'ct #:sep pico-y-sep))
(define title-coord-right (coord text-right 25/100 'ct #:sep pico-y-sep))
(define icon-coord-mid (coord 1/2 75/100 'cc))
(define sky-y 3/10)
(define sky-coord (coord 1/2 (- sky-y 26/1000) 'cb))
(define earth-coord (coord 1/2 1 'cb))
(define answer-y 55/100)
(define answer-coord-left (coord 30/100 answer-y 'ct))
(define answer-coord-mid (coord 1/2 answer-y 'ct))
(define answer-coord-right (coord 70/100 answer-y 'ct))

(define (landscape-w)
  (* 1.2 client-w))

(define (landscape-h)
  (h%->pixels sky-y))

(define (landscape-line-width)
  (* 5/100 (landscape-h)))

(define lattice-small-n 3)
(define lattice-large-n 6)

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
(define red0-3k1 (hex-triplet->color% #xF0749C))
(define red1-3k1 (hex-triplet->color% #xC0446C))
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
(define success-color green0-3k1)
(define error-color red1-3k1)

(define defense-pen-color-converter
  (lambda (n) (case n ((0) deep-pen-color) ((1) shallow-pen-color))))

(define defense-brush-color-converter
  (lambda (n) (case n ((0) deep-brush-color) ((1) shallow-brush-color))))

;; -----------------------------------------------------------------------------
;; --- text

(define (small-caps-style font)
  (cons 'no-combine (cons 'caps font)))

(define (bold-style font)
  (cons 'bold font))

(define title-text-font (small-caps-style "TeX Gyre Pagella"))
(define title-text-size 70)

(define h2-text-size 46)
(define h3-text-size 34)

(define title-rm-font "TeX Gyre Pagella")

(define body-text-font "Lucida Grande")
(define body-bold-font (bold-style body-text-font))
(define body-text-size 38)
(define sub-body-text-size 30)
(define tiny-text-size 22)

(define subtitle-text-font (small-caps-style body-text-font))

(define code-text-font "Inconsolata")
(define code-bold-font (bold-style code-text-font))
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

(define (ht2 . str*)
  (ht2* str*))

(define (ht2* str*)
  (txt str* #:font title-rm-font #:size h2-text-size #:color subtitle-text-color))

(define (ht3 . str*)
  (ht3* str*))

(define (ht3* str*)
  (txt str* #:font title-rm-font #:size h3-text-size #:color subtitle-text-color))

(define (RT . str*)
  (RT* str*))

(define (RT* str* #:color [color body-text-color])
  (txt str* #:font body-text-font #:size title-text-size #:color color))

(define (rt . str*)
  (rt* str*))

(define (rt* str* #:color [color body-text-color] #:font [font body-text-font])
  (txt str* #:font font #:size body-text-size #:color color))

(define (st . str*)
  (rt* str* #:color subtitle-text-color))

(define (bold-rt . str*)
  (rt* str* #:font body-bold-font))

(define (rrt . str*)
  (rrt* str*))

(define (rrt* str*)
  (txt str* #:font body-text-font #:size sub-body-text-size #:color body-text-color))

(define (sst . str*)
  (sst* str*))

(define (sst* str*)
  (txt str* #:font body-text-font #:size sub-body-text-size #:color subtitle-text-color))

(define (rt-deep . str*)
  (rt*-deep str*))

(define (rt*-deep str*)
  (txt str* #:font body-text-font #:size body-text-size #:color deep-pen-color))

(define (rt-deep2 . str*)
  (rt*-deep2 str*))

(define (rt*-deep2 str*)
  (define bg (txt str* #:font body-text-font #:size body-text-size #:color white))
  (define fg (txt str* #:font body-text-font #:size body-text-size #:color deep-brush-color))
  (cc-superimpose bg fg))

(define (rrt-deep . str*)
  (rrt*-deep str*))

(define (rrt*-deep str*)
  (txt str* #:font body-text-font #:size sub-body-text-size #:color deep-pen-color))

(define (rt-shallow . str*)
  (rt*-shallow str*))

(define (rt*-shallow str*)
  (txt str* #:font body-text-font #:size body-text-size #:color shallow-pen-color))

(define (rt-shallow2 . str*)
  (rt*-shallow2 str*))

(define (rt*-shallow2 str*)
  (define bg (txt str* #:font body-text-font #:size body-text-size #:color white))
  (define fg (txt str* #:font body-text-font #:size body-text-size #:color shallow-brush-color))
  (cc-superimpose bg fg))

(define (rrt-shallow . str*)
  (rrt*-shallow str*))

(define (rrt*-shallow str*)
  (txt str* #:font body-text-font #:size sub-body-text-size #:color shallow-pen-color))

(define (rt-untyped . str*)
  (rt*-untyped str*))

(define (rt*-untyped str*)
  (txt str* #:font body-text-font #:size body-text-size #:color untyped-pen-color))

(define (rt-untyped2 . str*)
  (rt*-untyped2 str*))

(define (rt*-untyped2 str*)
  (define bg (txt str* #:font body-text-font #:size body-text-size #:color white))
  (define fg (txt str* #:font body-text-font #:size body-text-size #:color untyped-brush-color))
  (cc-superimpose bg fg))

(define (rrt-untyped . str*)
  (rrt*-untyped str*))

(define (rrt*-untyped str*)
  (txt str* #:font body-text-font #:size sub-body-text-size #:color untyped-pen-color))

(define (make-vdash letter)
  (ppict-do @st{⊢ } #:go (coord 3/10 58/100 'lt) letter))

(define vdash-deep (make-vdash @rrt-deep{D}))
(define vdash-shallow (make-vdash @rrt-shallow{S}))
(define vdash-untyped (make-vdash @rrt-untyped{U}))

(define (tiny-txt str)
  (txt str #:font body-text-font #:size tiny-text-size #:color body-text-color))

(define (tt . str*)
  (tt* str*))

(define (tt* str*)
  (txt str* #:font code-text-font #:size code-text-size #:color code-text-color))

(define (bold-tt . str*)
  (bold-tt* str*))

(define (bold-tt* str*)
  (txt str* #:font code-bold-font #:size code-text-size #:color code-text-color))

(define (code-line-append . pp*)
  (code-line-append* pp*))

(define (code-line-append* pp*)
  (apply vl-append code-line-sep pp*))

(define (text-line-append . pp*)
  (text-line-append* pp*))

(define (text-line-append* pp*)
  (apply vl-append text-line-sep pp*))

(define (text-c-append . pp*)
  (text-c-append* pp*))

(define (text-c-append* pp*)
  (apply vc-append text-line-sep pp*))

(define (item-line-append . pp*)
  (item-line-append* pp*))

(define (item-line-append* pp*)
  (apply vl-append item-line-sep pp*))

(define (item-c-append . pp*)
  (item-c-append* pp*))

(define (item-c-append* pp*)
  (apply vc-append item-line-sep pp*))

(define (bghost pp)
  (blank (pict-width pp) (pict-height pp)))

(define (rt-bullet . str*)
  (rt-bullet* str*))

(define (rt-bullet* str*)
  (bullet* (map rt str*)))

(define (rrt-bullet . str*)
  (rrt-bullet* str*))

(define (rrt-bullet* str*)
  (bullet* (map rrt str*)))

(define (bullet* pp*)
  (item-line-append*
    (for/list ((pp (in-list pp*)))
      (hb-append hyphen-pict pp))))

(define (pipeline-append . pp*)
  (pipeline-append* pp*))

(define (pipeline-append* pp*)
  (define pipeline-sep (h%->pixels 4/100))
  (apply vc-append pipeline-sep
         (for/list ((pp (in-list pp*)))
           (if (pair? pp)
             (ht-append small-x-sep (car pp) (cdr pp))
             pp))))

(define (word-append . pp*)
  (word-append* pp*))

(define (word-append* pp*)
  (apply hc-append pp*))

(define (lattice-label-append . pp*)
  (lattice-label-append* pp*))

(define (lattice-label-append* pp*)
  (apply vc-append small-y-sep pp*))

(define (result-bubble pp)
  (add-rounded-border
    #:radius 22
    #:x-margin small-x-sep #:y-margin tiny-y-sep
    #:frame-width 1 #:frame-color grey-3k1
    #:background-color fog-3k1
    pp))

(define (success-text str)
  (txt str #:font body-text-font #:size body-text-size #:color success-color))

(define success-pict
  (success-text "OK"))

(define (error-text str)
  (txt str #:font body-text-font #:size body-text-size #:color error-color))

(define error-pict
  (error-text "Error"))

(define (bcellophane pp)
  (cellophane pp 0.4))

;; -----------------------------------------------------------------------------
;; --- ???

(define default-line-width 4)
(define default-arrow-size 14)
(define default-line-color highlight-pen-color)

(define (src-path . elem*)
  (src-path* elem*))

(define (src-path* elem*)
  (apply build-path src elem*))

(define (src-lang-path elem)
  (build-path src "lang" elem))

(define (src-bitmap ps)
  (bitmap (src-path ps)))

(define (scale-lang-bitmap pp)
  (scale-to-fit pp 120 80))

(define frame-radius 2)

(define (frame-bitmap ps #:w% [w% 9/10])
  (add-rounded-border
    #:radius frame-radius #:x-margin (w%->pixels 3/100) #:y-margin (h%->pixels 3/100)
    #:frame-width 2 #:frame-color neutral-pen-color
    #:background-color neutral-brush-color
    (scale-src-bitmap ps w%)))

(define (scale-src-bitmap ps w%)
  (let ((pp (src-bitmap ps)))
    (scale-to-width pp w%)))

(define (scale-to-width pp w%)
  (scale-to-fit pp (w%->pixels w%) (pict-height pp)))

(define (frame-person name ps [w% 15/100])
  (define pp (double-frame (scale-src-bitmap ps w%)))
  (if name
    (vl-append pp (ht3 (string-append "  " name)))
    pp))

(define (frame-team title-pict . name*)
  (frame-team* title-pict name*))

(define (frame-team* title-pict name*)
  (define pp* (for/list ((n (in-list name*))) (frame-person #f n 10/100)))
  (define sep 2)
  (define author-grid
    (let loop ((pp* pp*))
      (cond
        [(null? pp*)
         (blank)]
        [(null? (cdr pp*))
         (car pp*)]
        [else
         (vc-append sep (ht-append sep (car pp*) (cadr pp*)) (loop (cddr pp*)))])))
  (vc-append pico-y-sep title-pict author-grid))

(define (double-frame pp)
  (define bg-color subtitle-text-color)
  (define fg-color green3-3k1)
  (define border-gap 12)
  (define fg-width 4)
  (add-rounded-border
    #:radius frame-radius #:x-margin border-gap #:y-margin border-gap
    #:frame-width fg-width #:frame-color fg-color #:background-color bg-color
    (add-rounded-border
      #:radius frame-radius #:x-margin 0 #:y-margin 0
      #:frame-width fg-width #:frame-color fg-color #:background-color bg-color
      pp)))

(define (fancy-table row* #:num-cols [num-cols 3])
  (define title* (map bold-tt (car row*)))
  (define col-align (cons lc-superimpose rc-superimpose))
  (define row-align rc-superimpose)
  (define col-sep small-x-sep)
  (define tbl
    (vc-append
      (table
        num-cols
        title* col-align row-align col-sep 0)
      (table
        num-cols
        (apply append
               (cons
                 (map (lambda (x) (blank (pict-width x) 0)) title*)
                 (map (lambda (x) (map tt x)) (cdr row*))))
        col-align
        row-align
        col-sep
        (h%->pixels 4/100))))
  (define tbl/bg
    (add-neutral-background tbl))
  (double-frame tbl/bg))

(define (add-neutral-background pp)
  (add-rectangle-background
    #:x-margin small-x-sep #:y-margin tiny-y-sep
    #:color neutral-brush-color
    #:radius 1
    pp))

(define (add-dark-background pp)
  (add-rectangle-background
    #:x-margin small-x-sep #:y-margin tiny-y-sep
    #:color background-color
    #:radius 1
    pp))

(define (takeaway-frame pp)
  (add-rounded-border
    #:radius 1
    #:x-margin (/ client-w 2) #:y-margin med-y-sep
    #:frame-width tiny-y-sep #:frame-color green2-3k1 #:background-color background-color
    pp))

(define (type-to-shape lhs rhs)
  (word-append
    lhs @rrt{   --->   } rhs))

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

(define (untyped-code str)
  (untyped-codeblock #:title #f #:lang #f str))

(define (untyped-codeblock #:title [title #f] #:lang [lang "#lang untyped"] . str*)
  (untyped-codeblock* #:title title (conslang lang (map tt str*))))

(define (untyped-codeblock* pp* #:title [title #f])
  (X-codeblock pp* #:title title #:frame-color untyped-pen-color #:background-color untyped-brush-color))

(define (shallow-code str)
  (shallow-codeblock #:title #f #:lang #f str))

(define (shallow-codeblock #:title [title #f] #:lang [lang "#lang shallow"] . str*)
  (shallow-codeblock* #:title title (conslang lang (map tt str*))))

(define (shallow-codeblock* pp* #:title [title #f])
  (X-codeblock pp* #:title title #:frame-color shallow-pen-color #:background-color shallow-brush-color))

(define (deep-code str)
  (deep-codeblock #:title #f #:lang #f str))

(define (deep-codeblock #:title [title #f] #:lang [lang "#lang deep"] . str*)
  (deep-codeblock* #:title title (conslang lang (map tt str*))))

(define (deep-codeblock* pp* #:title [title #f])
  (X-codeblock pp* #:title title #:frame-color deep-pen-color #:background-color deep-brush-color))

(define (typed-codeblock #:title [title #f] #:lang [lang "#lang typed"] . str*)
  (deep-codeblock* #:title title (conslang lang (map tt str*))))

(define (dyn-codeblock sym)
  (case sym
    ((U untyped) untyped-codeblock)
    ((D deep) deep-codeblock)
    ((S shallow) shallow-codeblock)
    ((T typed) typed-codeblock)
    ((#f) (lambda arg* (blank)))
    (else (raise-argument-error 'dyn-codeblock "(or/c D S U)" sym))))

;; TODO stop sign for wrap, bend-sign for scan?
(define wrap-pict (rrt "wrap"))
(define scan-pict (rrt "scan"))
(define noop-pict (rrt "noop"))

(define (DSU-pict [mode 0])
  (ppict-do
    (blank (w%->pixels 6/10) (h%->pixels 4/10))
    #:go (coord 0 10/100 'lt #:abs-x 10)
    (add-hubs (deep-code "Deep") 'D)
    #:go (coord 1 10/100 'rt #:abs-x -10)
    (add-hubs (shallow-code "Shallow") 'S)
    #:go (coord 1/2 1 'cb)
    (add-hubs (untyped-code "Untyped") 'U)
    #:set
    (let* ((pp ppict-do-state)
           (lbl+arr*
             (list
               (list wrap-pict -18 0 (code-arrow 'D-S rb-find 'U-W lt-find (* 75/100 turn) (* 95/100 turn)  40/100 40/100 'solid))
               (list wrap-pict -15 52 (code-arrow 'U-W lb-find 'D-S lb-find (* 54/100 turn) (* 20/100 turn)  60/100 60/100 'solid))
               ;;
               (list wrap-pict 0 -24 (code-arrow 'D-E rt-find 'S-W lt-find (* 11/100 turn) (* 89/100 turn)  1/4 1/4 'solid))
               (list wrap-pict 0  16 (code-arrow 'S-W lb-find 'D-E rb-find (* 61/100 turn) (* 39/100 turn)  1/4 1/4 'solid))
               ;;
               (list noop-pict 13 0 (code-arrow 'S-S lb-find 'U-E rt-find (* 75/100 turn) (* 55/100 turn)  40/100 40/100 'solid))
               (list scan-pict 10 52 (code-arrow 'U-E rb-find 'S-S rb-find (* 96/100 turn) (* 30/100 turn)  60/100 60/100 'solid))
               )))
      (for/fold ((pp pp))
                ((l+a (in-list lbl+arr*)))
        (add-code-arrow pp (fourth l+a) #:line-width 2 #:label (first l+a) #:x-adjust-label (second l+a) #:y-adjust-label (third l+a))))))

(define the-swatch-str "   ")

(define untyped-swatch
  (untyped-codeblock* #:title #f (list (blank tiny-x-sep 0))))

(define shallow-swatch
  (shallow-codeblock* #:title #f (list (blank tiny-x-sep 0))))

(define deep-swatch
  (deep-codeblock* #:title #f (list (blank tiny-x-sep 0))))

(define (dyn-swatch sym)
  (case sym
    ((D T) deep-swatch)
    ((U) untyped-swatch)
    ((S) shallow-swatch)
    (else (raise-argument-error 'dyn-swatch "(or/c 'D 'S 'U)" sym))))

(define (lattice-h-append . pp*)
  (lattice-h-append* pp*))

(define (lattice-h-append* pp*)
  (apply hc-append lattice-x-sep pp*))

(define (lattice-vl-append . pp*)
  (lattice-vl-append* pp*))

(define (lattice-vl-append* pp*)
  (apply vl-append lattice-y-sep pp*))

(define (lattice-vc-append . pp*)
  (lattice-vc-append* pp*))

(define (lattice-vc-append* pp*)
  (apply vc-append lattice-y-sep pp*))

(define (migration-append . pp*)
  (migration-append* pp*))

(define (migration-append* pp*)
  (apply vc-append lattice-y-sep (add-between pp* down-arrow-pict)))

(define (codeblock-append . pp*)
  (codeblock-append* pp*))

(define (codeblock-append* pp*)
  (apply ht-append tiny-x-sep pp*))

(define (boundary-append . pp*)
  (boundary-append* pp*))

(define (boundary-append* pp*)
  (apply ht-append small-x-sep pp*))

(define (path-node sym*)
  (apply hc-append pico-x-sep (map dyn-swatch sym*)))

(define (boundary-node sym*)
  (let* ((pp* 
          (for/list ((sym (in-list sym*))
                     (i (in-naturals)))
            (add-hubs
              (scale (dyn-swatch sym) 1.5)
              (string->symbol (format "N~a" i)))))
         (pp (boundary-append* pp*))
         (gg (bghost pp))
         (pp (vc-append gg pp gg)))
    (add-code-arrow*
      ;; TODO options? gotta standardize everywhere
      pp 
      (code-arrow 'N0-E rc-find 'N1-W lc-find 0 0 0 0 'solid)
      (code-arrow 'N2-W lc-find 'N1-E rc-find (* 1/2 turn) (* 1/2 turn) 0 0 'solid)
      (code-arrow 'N3-E rc-find 'N4-W lc-find 0 0 0 0 'solid)
      (code-arrow 'N0-N rt-find 'N3-N lt-find (* 08/100 turn) (* 92/100 turn) 1/4 1/4 'solid)
      (code-arrow 'N2-S rb-find 'N4-S lb-find (* 90/100 turn) (* 10/100 turn) 1/4 1/4 'solid))))

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

(define (item-table . elem*)
  (unless (= 0 (modulo (length elem*) 2))
    (raise-arguments-error 'item-table "even number of args" "elem*" elem* "num elem*" (length elem*)))
  (item-table* (pair-up elem*)))

(define (pair-up x*)
  (let loop ((prev #f)
             (elem* x*))
    (cond
      [(null? elem*)
       '()]
      [prev
        (cons (cons prev (car elem*))
              (loop #f (cdr elem*)))]
      [else
        (loop (car elem*) (cdr elem*))])))

(define (item-table* pair*)
  (make-2table
    #:col-sep small-x-sep
    #:row-sep item-line-sep
    pair*))

(define blockquote-newline
  (blank 0 tiny-y-sep))

(define (blockquote . pp*)
  (blockquote* pp*))

(define (blockquote* pp*)
  (add-rounded-border
    #:x-margin small-x-sep #:y-margin small-y-sep
    #:radius 4
    #:frame-width 4
    #:background-color blue2-3k1
    #:frame-color blue0-3k1
    (text-line-append* pp*)))

(define optimization-name*
  '(apply box dead-code extflonum fixnum float-complex float list number pair sequence string struct vector))

(define (opt->pict sym)
  (tag-pict
    (double-frame
      (add-dark-background
        (rrt (symbol->string sym))))
    sym))

(define (pad-modulo n x*)
  (define ln (length x*))
  (define diff (- ln (* n (quotient ln n))))
  (append x* (make-list diff (blank))))

(define (bits->path-node b*)
  (path-node
    (for/list ((b (in-list b*)))
      (if b 'T 'U))))

(define (scale-small-lattice pp)
  (scale-to-fit pp (w%->pixels 55/100) (h%->pixels 45/100)))

(define (what-to-measure size)
  (define small-n lattice-small-n)
  (define large-n lattice-large-n)
  (define (make-table . pp*)
    (make-2table
      #:col-align ct-superimpose
      #:row-sep small-y-sep
      pp*))
  (case size
    ((small)
     (define n small-n)
     (define-values [tu-pict pre-lattice-pict]
       (what-to-measure-lattice n))
     (define lattice-pict
       (scale-small-lattice pre-lattice-pict))
     (make-table
       (lattice-label-append tu-pict @rrt{@~a[n] components})
       (lattice-label-append lattice-pict @rrt{@~a[(expt 2 n)] configurations})))
    ((large)
     (define n large-n)
     (define-values [small-tu-pict small-lattice-pict]
       (what-to-measure-lattice small-n))
     (define-values [large-tu-pict large-lattice-pict]
       (what-to-measure-lattice n))
     (define tu-pict
       (scale large-tu-pict 9/10))
     (define lattice-pict
       (bghost (scale-small-lattice small-lattice-pict)))
     (define pre-table
       (make-table
         (lattice-label-append tu-pict @rrt{@~a[n] components})
         (lattice-label-append lattice-pict @rrt{@~a[(expt 2 n)] configurations})))
     (ppict-do
       pre-table
       #:go (at-find-pict lattice-pict lc-find 'lc)
       (scale large-lattice-pict 20/100)))
    (else
      (raise-argument-error 'what-to-measure "(or/c 'small 'large)" size))))

(define (what-to-measure-lattice n)
  (define tu-pict
    (let* ((u-pict (path-node (make-list n 'U)))
           (t-pict (path-node (make-list n 'T))))
      (item-line-append t-pict u-pict)))
  (define x-sep
    (if (< n 5) lattice-x-sep pico-x-sep))
  (define lattice-pict
    (make-lattice n bits->path-node #:x-margin x-sep #:y-margin lattice-y-sep))
  (values tu-pict lattice-pict))

(define (d-lattice n)
  (define num-configs (expt 2 n))
  (define *num-good (box 0))
  (define (make-node bit*)
    (define pp (bits->path-node bit*))
    (if (equal? (first bit*) (last bit*))
      (begin (set-box! *num-good (+ 1 (unbox *num-good))) pp)
      (bcellophane pp)))
  (define lattice-pict
    (scale
      (make-lattice n make-node #:x-margin lattice-x-sep #:y-margin lattice-y-sep)
      6/10))
  (lattice-vc-append
    @rt{@~a[(exact-floor (pct (unbox *num-good) num-configs))]%}
    lattice-pict))

(define (small-face mood)
  (scale (face mood) 45/100))

;;(define thesis-full-pict
;;  ;; TODO use bullet list
;;  (text-line-append
;;    @rt{Deep and Shallow types can coexist in a way
;;    that preserves their formal properties.}
;;    @rt{Programmers can combine these types to strengthen Shallow-type}
;;    @rt{guarantees, avoid unimportant Deep-type runtime errors, and lower the}
;;    @rt{running time of typed/untyped interactions.}))

;; -----------------------------------------------------------------------------
;; --- extra pict

(define ruler-pict
  (frame-bitmap "ruler.jpg" #:w% 2/10))

(define scale-pict
  (frame-bitmap "scale.jpeg" #:w% 2/10))

(define mixed-typed-pict
  (path-node '(D D D U U)))

(define typed-pict
  (path-node '(D D D D D)))

(define untyped-pict
  (path-node '(U U U U U)))

(define (check-pict size)
  (define outer-color green1-3k1)
  (define inner-color green0-3k1)
  (define line-width% 6)
  ;;
  (define size/2 (/ size 2))
  (define size/3 (/ size 3))
  (define line-width (/ size line-width%))
  (define line-width/2 (/ line-width 2))
  ;;
  (define (draw-x dc% dx dy)
    (define old-brush (send dc% get-brush))
    (define old-pen (send dc% get-pen))
    ;;
    (send dc% set-brush (new brush% [color inner-color]))
    (send dc% set-pen (new pen% [width 1] [color outer-color]))
    ;; draw check from mid-left
    (define path% (new dc-path%))
    (send path% move-to line-width/2 (* 60/100 size))
    (send path% line-to (- size/2 (/ line-width 2)) size)
    (send path% line-to (+ size/2 (/ line-width 4)) size)
    (send path% line-to size 0)
    (send path% line-to (- size line-width) 0)
    (send path% line-to (- size/2 (/ line-width 8)) (- size line-width))
    (send path% line-to (- size/2 (/ line-width 5)) (- size line-width))
    (send path% line-to (* 1.6 line-width) (* 60/100 size))
    (send path% close)
    (send dc% draw-path path% dx dy)
    ;;
    (send dc% set-brush old-brush)
    (send dc% set-pen old-pen)
    (void))
  (dc draw-x size size))

(define (x-pict size)
  (define outer-color red1-3k1)
  (define inner-color red0-3k1)
  (define line-width% 6)
  ;;
  (define size/2 (/ size 2))
  (define line-width (/ size line-width%))
  (define line-width/2 (/ line-width 2))
  ;;
  (define (draw-x dc% dx dy)
    (define old-brush (send dc% get-brush))
    (define old-pen (send dc% get-pen))
    ;;
    (send dc% set-brush (new brush% [color inner-color]))
    (send dc% set-pen (new pen% [width 1] [color outer-color]))
    ;; draw X from top-left, counterclockwise
    (define path% (new dc-path%))
    (send path% move-to 0 0)
    (send path% line-to (- size/2 line-width/2) size/2)
    (send path% line-to 0 size)
    (send path% line-to line-width size)
    (send path% line-to size/2 (+ size/2 line-width/2))
    (send path% line-to (- size line-width) size)
    (send path% line-to size size)
    (send path% line-to (+ size/2 line-width/2) size/2)
    (send path% line-to size 0)
    (send path% line-to (- size line-width) 0)
    (send path% line-to size/2 (- size/2 line-width/2))
    (send path% line-to line-width 0)
    (send path% close)
    (send dc% draw-path path% dx dy)
    ;;
    (send dc% set-brush old-brush)
    (send dc% set-pen old-pen)
    (void))
  (dc draw-x size size))

(define (basic-star size)
  (make-compass-pict size #:color fog-3k1))

(define (theory-star size)
  ;; TODO randomize size / rotation? tag with venue + year?
  ;; size 8 ... 30 all reasonable
  ;; bib range from 1970 to 2020 ... 50 years
  (basic-star size))

(define (base-sky-pict)
  ;; TODO curve the sky
  (define outer-color blue1-3k1)
  (define inner-color blue0-3k1)
  (define pen-width (landscape-line-width))
  (define w (landscape-w))
  (define h (landscape-h))
  ;;
  (define (draw-sky dc% dx dy)
    (define old-brush (send dc% get-brush))
    (define old-pen (send dc% get-pen))
    ;;
    (send dc% set-brush (new brush% [color inner-color]))
    (send dc% set-pen (new pen% [width 0] [color inner-color]))
    (send dc% draw-rectangle dx dy (+ dx w) (+ dy h))
    (send dc% set-brush (new brush% [color outer-color]))
    (send dc% set-pen (new pen% [width 0] [color outer-color]))
    (send dc% draw-rectangle dx (+ dy (- h (* 2 pen-width))) w pen-width)

    ;;
    (send dc% set-brush old-brush)
    (send dc% set-pen old-pen)
    (void))
  (dc draw-sky w h))

(define gt-year0 1971)
(define gt-year1 2020)

(define (year->sky-x y)
  (define start gt-year0)
  (define end gt-year1)
  (unless (<= start y end)
    (raise-argument-error 'year->sky-x "(integer-in 1971 2020)" y))
  (/ (- y start) (- end start)))

(define (sky-pict)
  (let* ((pp (base-sky-pict))
         (star-size 16)
         (num-col 7)
         (star-pict (theory-star star-size))
         (pp (ppict-do
               pp
               #:go (coord (year->sky-x 2006) 50/100 'cc)
               (vc-append star-pict (hc-append pico-x-sep star-pict star-pict) star-pict)))
         (pp
           (for/fold ((pp pp))
                     ((yr (in-range 1971 2020))
                      #:unless (= yr 2006))
            (ppict-do pp
              #:go (coord (year->sky-x yr) 10/100 'ct)
              (apply vc-append pico-y-sep
                     (for/list ((i (in-range num-col)))
                       (if (zero? (modulo (+ i yr) 3))
                         (bghost star-pict)
                         star-pict))))))
         )
    pp))

(define (base-earth-pict)
  ;; TODO curve the earth
  (define outer-color green2-3k1)
  (define inner-color green1-3k1)
  (define pen-width (landscape-line-width))
  (define w (landscape-w))
  (define h (landscape-h))
  ;;
  (define (draw-sky dc% dx dy)
    (define old-brush (send dc% get-brush))
    (define old-pen (send dc% get-pen))
    ;;
    (send dc% set-brush (new brush% [color inner-color]))
    (send dc% set-pen (new pen% [width 0] [color inner-color]))
    (send dc% draw-rectangle dx dy (+ dx w) (+ dy h))
    (send dc% set-brush (new brush% [color outer-color]))
    (send dc% set-pen (new pen% [width 0] [color outer-color]))
    (send dc% draw-rectangle dx dy w pen-width)

    ;;
    (send dc% set-brush old-brush)
    (send dc% set-pen old-pen)
    (void))
  (dc draw-sky w h))

(define (earth-pict)
  (let* ((pp (base-earth-pict))
         (lang-pict* (map (compose1 scale-lang-bitmap bitmap)
                          (glob (build-path src "lang" "*.png"))))
         (xy* '(
                (28/100 80/100) (32/100 10/100) (36/100 70/100) (40/100 20/100)
                (44/100 80/100) (48/100 10/100) (52/100 70/100) (56/100 20/100)
                (60/100 80/100) (64/100 10/100) (68/100 70/100) (72/100 20/100)
                (76/100 80/100) (80/100 10/100) (84/100 70/100) (88/100 20/100)))
         (pp (for/fold ((pp pp))
                       ((lang-pict (in-list lang-pict*))
                        (pre-x (in-range (+ gt-year0 2) (- gt-year1 2) 3))
                        (y (in-cycle (in-list '(28 74)))))
               (ppict-do pp #:go (coord (year->sky-x pre-x) (/ y 100) 'cc) lang-pict)))
         )
    pp))

(define pass-pict
  (check-pict 40))

(define fail-pict
  (x-pict 40))

(define ds-model-pict
  @rt{Model})

(define ds-impl-pict
  @rt{Implementation})

(define ershov-pict
  (frame-person "Ershov" "ershov.png" 20/100))

(define tally-pict
  (filled-rectangle 4 small-y-sep #:color neutral-brush-color #:draw-border? #f))

(define trie-code* '(
  "(require pfds/trie)"
  ""
  "(define t (trie ....))"
  "(time (bind t ....))"
))

(define q-pict @ht2{Q. })
(define a-pict @ht2{A. })
(define D-pict @bold-rt{D})

(define down-arrow-pict
  (colorize
    (arrowhead 20 (* 3/4 turn))
    fog-3k1))

(define big-hyphen-pict @ht2{- })
(define hyphen-pict @st{- })
(define hyphen-ghost (bghost hyphen-pict))

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
  (define a-pict @st[#:color body-text-color]{Ben Greenman    2020-12-17})
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

(define (basic-example lhs rhs #:arrow? [arrow? #false] #:text? [text? #true])
  (let* ((pp
          (boundary-append
            (add-hubs
              ((dyn-codeblock lhs)
               "(define (f (n : Num))"
               "  (+ n 1))")
              'BL)
            (add-hubs
              ((dyn-codeblock rhs)
               "(f \"A\")")
              'BR)))
         (arr
           (code-arrow 'BR-S cb-find 'BL-E lc-find (* 3/4 turn) (* 35/100 turn) 90/100 5/100 'solid)))
    (if arrow? (add-code-arrow pp arr) pp)))

(define (question-text pp)
  (word-append q-pict pp))

(define (answer-text pp)
  (word-append a-pict pp))

(define (bad-pair-example lhs mid rhs)
  (codeblock-append
    ((dyn-codeblock lhs)
      "(f '(\"A\" \"B\"))"
      ""
      "")
    ((dyn-codeblock mid)
      "(define (f (x : (List Num)))"
      "  (g x))"
      "")
    ((dyn-codeblock rhs)
      "(define (g y)"
      "  (+ (first y)"
      "     42))")))

(define (bad-fun-example lhs mid rhs)
  (codeblock-append
    ((dyn-codeblock lhs)
      "(f (λ....))"
      )
    ((dyn-codeblock mid)
      "(define (f (x : (-> Num)))"
      "  (g x))")
    ((dyn-codeblock rhs)
      "(define (g y)"
      "  (+ (y) 3))")))

(define (higher-order-any-example lhs rhs)
  (codeblock-append
    ((dyn-codeblock lhs)
     "(: b Any)"
     "(define b (box 42))")
    ((dyn-codeblock rhs)
     ""
     "(set-box! b 0)")))

(define (index-of-example lang)
  (define mk (dyn-codeblock lang))
  (if (eq? mk untyped-codeblock)
    (mk
      "(index-of '(a b) 'a)")
    (mk
      "(: index-of"
      "   (-> (Listof T) T (Maybe Num)))"
      ""
      "(index-of '(a b) 'a)")))

(define (sieve-example lhs rhs)
  (codeblock-append
    ((dyn-codeblock lhs)
     "...."
     "; stream tools")
    ((dyn-codeblock rhs)
     "...."
     "(get-prime 6667)")))

(define (tiny-sieve-example lhs rhs)
  (define fake-line "....         ")
  (define pp
    (codeblock-append
      ((dyn-codeblock lhs)
       fake-line)
      ((dyn-codeblock rhs)
       fake-line)))
  (scale pp 55/100))

(define (big-overhead-plot bm-name deco*)
  (define pi* (map (deco->pi bm-name) deco*))
  (define pp (ss-overhead-plot pi* 8 (w%->pixels 65/100) (h%->pixels 35/100)))
  (vl-append 4
    (rrt (~a bm-name))
    (add-xticks (double-frame pp))))

(define (2col-overhead-plot bm-name deco*)
  (define pi* (map (deco->pi bm-name) deco*))
  (define pp (ss-overhead-plot pi* 3 (w%->pixels 38/100) (h%->pixels 16/100)))
  (vl-append 4
    (tiny-txt (~a bm-name))
    (double-frame pp)))

(define (2col-overhead-plot* deco bm-name*)
  (make-2table
    #:row-sep tiny-y-sep
    (map (lambda (bm-name) (2col-overhead-plot bm-name (list deco))) bm-name*)))

(define (ss-overhead-plot pi* line-width w h)
  (define base-color
    (if (and (not (null? pi*))
             (null? (cdr pi*))
             (reticulated-info? (car pi*)))
      1
      0))
  (define pp
    (parameterize ([*OVERHEAD-MAX* MAX-OVERHEAD]
                   [*OVERHEAD-LEGEND?* #false]
                   [*OVERHEAD-PLOT-WIDTH* w]
                   [*OVERHEAD-PLOT-HEIGHT* h]
                   [*OVERHEAD-LINE-WIDTH* line-width]
                   [*OVERHEAD-LINE-COLOR* base-color]
                   [*PEN-COLOR-CONVERTER* defense-pen-color-converter]
                   [*BRUSH-COLOR-CONVERTER* defense-brush-color-converter]
                   [*INTERVAL-ALPHA* code-brush-alpha]
                   [*MULTI-INTERVAL-ALPHA* code-brush-alpha])
      (overhead-plot pi*)))
  pp)

(define ((deco->pi bm-name) d)
  (case d
    ((D)
     (benchmark-name->performance-info bm-name transient-rkt-version))
    ((S)
     (or
       (with-handlers ((exn:fail? (lambda (ex) #f)))
         (benchmark-name->performance-info bm-name stransient))
       (rp:benchmark-name->performance-info bm-name)))
    ((best)
     (define pi-deep (benchmark-name->performance-info bm-name transient-rkt-version))
     (define pi-shallow (benchmark-name->performance-info bm-name stransient))
     (make-typed-racket-info
       (for/vector ((deep-cfg (in-configurations pi-deep))
                    (shallow-cfg (in-configurations pi-shallow)))
         (define deep-t* (configuration-info->runtime* deep-cfg))
         (define shallow-t* (configuration-info->runtime* shallow-cfg))
         (if (< (mean deep-t*) (mean shallow-t*))
           deep-t*
           shallow-t*))))
    (else (raise-argument-error 'big-overhead-plot "(or/c 'D 'S 'best)" d))))

(define (add-xticks pp)
  (ppict-do
    pp
    #:go (coord 0 1 'lt) @rrt{1x}
    #:go (coord 1/4 1 'lt) @rrt{2x}
    #:go (coord 3/4 1 'ct) @rrt{10x}
    #:go (coord 1 1 'rt) @rrt{20x}))

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

(define full-ershov-quote
  ;; from hidden places knowledge i obtained_ k. Levitin
  "And I have long since taught myself to think that if I reproduce somebody's guess in my work, I should not regret not having been the first, but, on the contrary, should always bear it in mind that it is a major stimulus: since a similar idea has occured to me living thousands of kilometers away, it means that there really is something in it")

(define (sec:intro)
  (pslide
    #:go heading-coord-left
    @ht2{On Great Ideas}
    ;; A.P. Ershov 1983
    ;; TODO check references
    ;; - algebra for PL (cite?)
    ;; - correctness of optimizing compiler BETA (cite?)
    ;; - second literacy
    #:go heading-coord-right
    ershov-pict
    #:go text-coord-left
    (blockquote
      @rrt{If I reproduce somebody's guess}
      @rrt{ in my work ...}
      blockquote-newline
      @rrt{me living far away ...}
      blockquote-newline
      @rrt{it means that}
      @rrt{ there really is something in it}))
  (pslide
    #:go heading-coord-mid
    (takeaway-frame
      (vc-append pico-y-sep
        @ht2{Great Idea:}
        (word-append
          @rt{mixing } @rt-deep2{typed} @rt{ and } @rt-untyped2{untyped} @rt{ code})
        (hsep item-line-sep)
        mixed-typed-pict))
    #:go (coord text-left 43/100 'lt)
    (frame-team @ht3{Gradual Typing} "siek.jpg" "taha.jpeg")
    #:go (coord 24/100 68/100 'ct)
    (frame-team @ht3{Migratory Typing} "tobin-hochstadt.jpg" "felleisen.jpg")
    #:go (coord 53/100 51/100 'ct)
    (frame-team (text-c-append @ht3{Multi-Language} @ht3{Semantics}) "matthews.jpeg" "findler.png")
    #:go (coord text-right 41/100 'rt)
    (frame-team @ht3{Hybrid Typing}
      "unknown.png" "knowles.jpeg" "freund.png" "tomb.jpg" "flanagan.png"))
  (pslide
    #:go heading-coord-left
    @ht2{The Basics}
    #:go text-coord-left
    (make-2table
      #:row-align lt-superimpose
      #:col-align lt-superimpose
      #:row-sep med-y-sep
      #:col-sep small-x-sep
      (list
        (list
          (text-line-append
            @rt-deep2{typed code}
            @rrt{  more constraints, strong guarantees})
          typed-pict)
        (list
          (text-line-append
            @rt-untyped2{untyped code}
            @rrt{  more freedom, for better or worse})
          untyped-pict)
        (list
          (text-line-append
            @rt{mixed-typed code}
            @rrt{  combine both ... somehow})
          (path-node '(U D U D D))))))
  (pslide
    #:go heading-coord-mid
    (boundary-node '(U D U D D))
    (question-text
      @rt{What happens at the boundaries?})
    (hsep (* 1.2 small-y-sep))
    (item-c-append
      (word-append
        @rrt{Types predict a  } (deep-code "Num")
        @rrt{  but get the letter  } (untyped-code "\"A\""))
      (basic-example 'T 'U #:arrow? #true)))
  ;; return to Q in a minute, first more history / context
  ;;  ... now that you know roughly what solving, more about solutions
  (pslide
    #:go (coord text-left 43/100 'lt)
    (frame-team @ht3{Gradual Typing} "siek.jpg" "taha.jpeg")
    #:go (coord 24/100 68/100 'ct)
    (frame-team @ht3{Migratory Typing} "tobin-hochstadt.jpg" "felleisen.jpg")
    #:go (coord 53/100 51/100 'ct)
    (frame-team (text-c-append @ht3{Multi-Language} @ht3{Semantics}) "matthews.jpeg" "findler.png")
    #:go (coord text-right 41/100 'rt)
    (frame-team @ht3{Hybrid Typing}
      "unknown.png" "knowles.jpeg" "freund.png" "tomb.jpg" "flanagan.png")
    #:go sky-coord
    (sky-pict))
  (pslide
    ;; TODO decorate (?)
    #:go sky-coord (sky-pict)
    #:go earth-coord (earth-pict)
    #:next
    #:go center-coord
    @ht2{Mixed-Typed Design Space}
    @rt{Lively, but Disorganized!})
  (pslide
    ;; example disagreement
    #:go earth-coord
    (earth-pict)
    #:go text-coord-mid
    (basic-example 'T 'U)
    (hsep tiny-y-sep)
    (question-text
      (word-append
        @rrt{Does the type  } (deep-code "Num")
        @rrt{  keep out the letter  } (untyped-code "\"A\"")
        @rrt{  ?}))
    #:go answer-coord-left
    (answer-text
      @rt{Yes!})
    (apply ht-append pico-x-sep (make-list 3 tally-pict))
    #:go answer-coord-right
    (answer-text
      @rt{No!})
    (apply ht-append pico-x-sep (make-list 7 tally-pict)))
  (pslide
    #:go earth-coord
    (earth-pict)
    #:go text-coord-mid
    (bad-pair-example 'U 'T 'U)
    (hsep tiny-y-sep)
    (question-text
      (word-append
        @rrt{Does the type  } (deep-code "(List Num)") @rrt{  keep out the list of letters?}))
    #:go answer-coord-left
    (answer-text
      @rt{Yes!})
    (apply ht-append pico-x-sep (make-list 2 tally-pict))
    #:go answer-coord-right
    (answer-text
      @rt{No!})
    (apply ht-append pico-x-sep (make-list 2 tally-pict)))
  (pslide
    #:go earth-coord
    (earth-pict)
    #:go text-coord-mid
    (bad-fun-example 'U 'T 'U)
    (hsep tiny-y-sep)
    (question-text
      (word-append
        @rrt{Does the type  } (deep-code "(-> Num)") @rrt{  keep out bad functions?}))
    #:go answer-coord-left
    (answer-text
      @rt{Yes})
    (apply ht-append pico-x-sep (make-list 2 tally-pict))
    #:go answer-coord-right
    (answer-text
      @rt{No})
    (apply ht-append pico-x-sep (make-list 2 tally-pict)))
  (pslide
    #:go sky-coord
    (sky-pict)
    #:go earth-coord
    (earth-pict)
    #:go center-coord
    #:alt [
      (question-text
        @rt{What happens at the boundaries?})
      #:go answer-coord-mid
      (boundary-append
        (answer-text @rrt{Nothing})
        (answer-text @rrt{Spot-checks})
        (answer-text @rrt{Everything!})
        (answer-text @rrt{...}))
    ]
    (boundary-append
      (question-text
        @rt{Why?})
      (answer-text
        @RT{Performance!}))
    (hsep tiny-y-sep)
    (question-text
      @rt{Where's the data?}))
  (pslide
    #:go sky-coord
    (sky-pict)
    #:go earth-coord
    (earth-pict)
    #:go center-coord
    @ht2{Mixed-Typed Design Space}
    @rt{Lively, but Disorganized!}
    #:go center-coord
    (frame-person #f "elephant.jpg" 7/10))
  (pslide
    #:go sky-coord
    (sky-pict)
    #:go earth-coord
    (earth-pict)
    #:go (coord text-left 34/100 'lt)
    (text-line-append
      @ht2{My Research}
      @rt{ brings order to}
      @rt{ the design space})
    #:go (coord contribution-x 26/100 'lt)
    (ht-append
      @ht2{1. }
      (text-line-append
        @rt{How to assess}
        @rt{ type guarantees}))
    #:go (coord contribution-x 56/100 'lt)
    (ht-append
      @ht2{2. }
      (text-line-append
        @rt{How to measure}
        @rt{ performance})))
  (pslide
    ;; TODO highlight promising points at top and bottom
    #:go sky-coord
    (sky-pict)
    #:go earth-coord
    (earth-pict)
    #:go (coord contribution-x 26/100 'lt)
    (bcellophane (ht-append @ht2{1. } (text-line-append @rt{How to assess} @rt{ type guarantees})))
    #:go (coord contribution-x 56/100 'lt)
    (bcellophane (ht-append @ht2{2. } (text-line-append @rt{How to measure} @rt{ performance})))
    #:go (coord 1/2 38/100 'ct)
    (text-line-append
      @ht2{Thesis Preview:}
      @rt{ Deep and Shallow types can interoperate}))
  (void))

(define (sec:perf)
  (pslide
    #:go earth-coord
    (earth-pict)
    #:go (coord contribution-x 56/100 'lt)
    (text-line-append @rt{How to measure} @rt{ performance})
    #:go center-coord
    @rrt{(highlight TR)})
  (pslide
    #:go earth-coord
    (earth-pict)
    #:go heading-coord-left
    @ht2{Typed Racket}
    #:go text-coord-left
    (rt-bullet
      "Oldest, strongest mixed-typed language"
      "Home of severe performance costs"))
  (pslide
    #:go heading-coord-left
    @ht2{Costs ...}
    #:go title-coord-mid
    (frame-person #f "math-array.png" 8/10))
  (pslide
    #:go heading-coord-left
    @ht2{... More Costs}
    #:go heading-coord-right
    (frame-person #f "trie-small.png" 45/100)
    #:go title-coord-mid
    (hsep tiny-y-sep)
    (item-table
      (apply untyped-codeblock trie-code*)
      @rt{12 seconds}
      (apply typed-codeblock trie-code*)
      @rt{1 ms!}))
  (pslide
    #:go earth-coord
    (earth-pict)
    #:go heading-coord-left
    @ht2{Typed Racket Performance}
    #:go text-coord-left
    (rt-bullet
      "Problems exist ... clearly"
      "Widespead issue?  Language bug?")
    #:go center-coord
    (takeaway-frame
      @ht2{Need a way to measure!}))
  (pslide
    #:go earth-coord
    (earth-pict)
    #:go heading-coord-left
    @ht2{Step 1: Benchmarks}
    #:go heading-coord-right
    @rrt{Ben Asumu Max Dan Matthias}
    #:go text-coord-mid
    @rt{Collected small, useful programs}
    (boundary-append
      (path-node '(U U U U U U))
      (path-node '(T T))
      (path-node '(U U U)))
    (hsep small-y-sep)
    @rt{Added types, if missing}
    (boundary-append
      (path-node '(T T T T T T))
      (path-node '(T T))
      (path-node '(T T T))))
  (pslide
    #:go earth-coord
    (earth-pict)
    #:go heading-coord-left
    @ht2{Step 1: Benchmarks}
    #:go text-coord-left
    (rt-bullet
      "21 benchmark in total"
      "from 2 to 14 modules"
      "games, apps, libraries, ...."))
  (pslide
    #:go earth-coord
    (earth-pict)
    #:go heading-coord-left
    @ht2{Step 1: Benchmarks}
    #:go center-coord
    ;; TOOD migratable vs contextual, circle vs square
    (scale-src-bitmap "jpeg-description.png" 95/100))
  (pslide
    #:go earth-coord
    (earth-pict)
    #:go heading-coord-left
    @ht2{Step 2: How to Measure}
    #:go text-coord-mid
    @rt{What to measure = all configurations}
    ;; NOTE migratable vs contextual
    (hsep small-y-sep)
    #:alt [(what-to-measure 'small)]
    #:alt [(what-to-measure 'large)]
    (ht-append
      med-x-sep
      (question-text @rt{How to study?})
      (question-text @rt{How to scale?}))
    (hsep med-y-sep)
    (answer-text @rt{Focus on the programmer ...}))
  (pslide
    #:go earth-coord
    (earth-pict)
    #:go (coord 30/100 text-top 'ct)
    (migration-append
      (path-node '(U U U U U))
      (path-node '(T U U U U))
      (path-node '(T U T U U))
      (path-node '(T U T T T)))
    #:go (coord 70/100 text-top 'ct)
    #:alt [(hc-append
      (vl-append small-y-sep (small-face 'happy) (small-face 'happier))
      (small-face 'sortof-unhappy))]
    (let* ((bg (bghost (small-face 'happy)))
           (placeholder (lambda (tag) (tag-pict bg tag)))
           (pp (hc-append (vl-append small-y-sep (placeholder 'A) (placeholder 'C))
                          (placeholder 'B))))
      (for/fold ((acc pp))
                ((tag (in-list '(A C B)))
                 (str (in-list '("1x = baseline" "0.9x = improvement" "20x = too slow!"))))
        (ppict-do acc #:go (at-find-pict tag cc-find 'cc) (rt str)))))
  (pslide
    #:go earth-coord
    (earth-pict)
    #:go heading-coord-left
    @ht2{Step 2: How to Measure}
    #:go text-coord-mid
    (answer-text
      (word-append @rt{Count } D-pict @rt{-deliverable configs}))
    (hsep small-y-sep)
    ;; TODO large lattice also?
    (make-2table
      #:row-align ct-superimpose
      #:col-align ct-superimpose
      (list
        (text-line-append
          (word-append
            @rrt{If  } D-pict @rrt{=4, then count})
          @rrt{configs with at most}
          @rrt{4x overhead})
        (d-lattice lattice-small-n))))
  (pslide
    #:go heading-coord-left
    @ht2{Step 2: How to Measure}
    #:go text-coord-mid
    (answer-text
      (word-append @rt{Count } D-pict @rt{-deliverable configs}))
    (text-line-append
      (word-append
        @rrt{For a HUGE space, count  } D-pict @rrt{-deliverable configs})
      @rrt{ in a collection of random samples})
    (hsep small-y-sep)
    (scale (make-lattice 12 bits->path-node #:x-margin lattice-x-sep #:y-margin lattice-y-sep) 5/10))
  (pslide
    #:go earth-coord
    (earth-pict)
    #:go heading-coord-left
    @ht2{Step 3: Picture}
    #:go text-coord-mid
    (tag-pict (big-overhead-plot 'jpeg '(D)) 'plot)
    #:set (let ((pp ppict-do-state))
            (ppict-do pp #:go (at-find-pict 'plot lb-find 'rc)
                      (vl-append (hsep small-y-sep) (word-append D-pict @rt{ = })))))
  (pslide
    #:go earth-coord
    (earth-pict)
    #:go heading-coord-left
    @ht2{Performance Method}
    #:go text-coord-mid
    (item-line-append
      (hb-append @st{1. } @rt{collect benchmarks})
      (hb-append @st{2. }
                 (word-append @rt{count  } D-pict @rt{-deliverable configs (or sample)}))
      (hb-append @st{3. }
                 (word-append @rt{plot results for many  } D-pict))))
  (pslide
    #:go earth-coord
    (earth-pict)
    #:go title-coord-mid
    @rt{Applications: TR and RP})
  (pslide
    #:go earth-coord
    (earth-pict)
    #:go heading-coord-left
    (hb-append
      @ht2{Typed Racket}
      @rrt{  some results from our 21 benchmarks})
    #:go text-coord-mid
    (2col-overhead-plot* 'D '(jpeg suffixtree take5 synth)))
  (pslide
    #:go earth-coord
    (earth-pict)
    #:go title-coord-mid
    @rt{TR = bad})
  (pslide
    #:go earth-coord
    (earth-pict)
    #:go heading-coord-left
    (hb-append
      @ht2{Reticulated Python}
      @rrt{   different benchmarks})
    #:go text-coord-mid
    (2col-overhead-plot* 'S '(spectralnorm go Espionage chaos)))
  (pslide
    #:go earth-coord
    (earth-pict)
    #:go title-coord-mid
    @rt{RP = much better})
  (void))

(define (sec:design)
  (pslide
    #:go earth-coord
    (earth-pict)
    #:go title-coord-mid
    @rt{WOW}
    (hsep small-y-sep)
    @rrt{TR vs RP, night vs day})
  (pslide
    #:go sky-coord
    (sky-pict)
    #:go title-coord-mid
    (bghost @rt{WOW})
    (hsep small-y-sep)
    @rrt{TR vs RP, night vs day}
    (hsep small-y-sep)
    (text-line-append
      @rrt{type sound? yes yes}
      @rrt{gradual guarantee? yes yes}
      @rrt{blame theorem? yes yes})
    @rrt{TR << RP ?})
  (pslide
    #:go text-coord-mid
    (bad-pair-example 'U 'T 'U)
    (hsep tiny-y-sep)
    (question-text
      (word-append
        @rrt{Does the type  } (deep-code "(List Num)") @rrt{  keep out the list of letters?}))
    #:go answer-coord-left
    (answer-text
      @rt{Yes!})
    (hsep small-y-sep)
    @rrt{Typed Racket}
    #:go answer-coord-right
    (answer-text
      @rt{No!})
    (hsep small-y-sep)
    @rrt{Reticulated Python})
  (pslide
    #:go heading-coord-left
    @rt{CM example, functions}
    #:go text-coord-mid
    @rrt{ah missing error})

  (pslide
    ;; NOW these are not the only contenders!
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
  (pslide
    #:go heading-coord-left
    @ht{Thesis}
    #:go title-coord-mid
    @ht2{Deep and Shallow types can interoperate.}
    @rrt{(preserving their formal properties)}
    (blank 0 tiny-y-sep)
    @ht2{Programmers can use these types to:}
    (rt-bullet
      "strengthen Shallow guarantees"
      "avoid unimportant Deep errors"
      "lower runtime costs"))
  (pslide
    #:go center-coord
    ;; moonlight?
    ;; TODO
    ;; - show map (?)
    ;; - Natural + Transient
    @ht{Unpublished Results})
  (pslide
    #:go center-coord
    @rrt{show map, Deep and Shallow}
    @rrt{Natural / TR = clear choice, best in show}
    @rrt{Transient = no wrappers, great}
    @rrt{goal is combine, new model + impl points}
    @rrt{but cannot use Transient from RP, gotta add new types, remove dyn, subtyping})

  (sec:thesis:transient)
  (pslide
    ;; HERE is a bit early to discuss blame problem, but its important signpost here
    #:go center-coord
    @rrt{show Transient, transient+ again}
    @rrt{(gotta add new types, remove dyn, subtyping)}
    @rrt{that gives sense of changes}
    @rrt{but major blame challenges, failed})
  (sec:thesis:model)
  (sec:thesis:implementation)
  (sec:thesis:evaluation)
  (void))

(define (sec:thesis:transient)
  (pslide
    #:go heading-coord-left
    @rt{Transient without Dyn}
    #:go text-coord-mid
    @rrt{(f x) : Num morphs to}
    #:go title-coord-left
    @rrt{Before}
    @rrt{(check Num (check Fun f) (check Num x))}
    @rrt{... because could be Dyn anywhere}
    #:go title-coord-right
    @rrt{After}
    @rrt{(check Num (f x))}
    @rrt{trust shape soundness})
  (pslide
    #:go heading-coord-left
    @ht{Completion}
    #:go text-coord-left
    @rrt{made possible by another change}
    #:go title-coord-left
    @rrt{before, type elaboration}
    #:go title-coord-right
    @rrt{after, surf + tgt types, completion}
    @rrt{theorem, completion correctness}
    @rrt{opens door to optimizations}
    #:go icon-coord-mid
    @rrt{surface types support multi-lang too})
  (void))

(define (sec:thesis:model)
  ;; HANG ON the contribution is the scaled-up transient, not so much the model
  ;; - more types, subtyping
  ;; - optimizations
  ;; - no dyn = audit check sites
  ;; ... okay make a checklist
  (pslide
    ;; map again
    #:go center-coord
    @rrt{natural + mod. transient under one roof}
    )
  (pslide
    ; {first step, prototype / model}
    ; {prove deep deep, shallow shallow}
    ; {explore optimizations, show picture (move "wrap" boundary), details at end}
    ; {results = cm, f-ts, compilation}
    #:go heading-coord-left
    @ht{Model}
    #:go text-coord-mid
    @rt{???})
  (pslide
    #:go heading-coord-left
    @rt{Syntax}
    #:go text-coord-mid
    @rrt{show e, but focus on boundaries, 3 module-kinds}
    @rrt{compile to 3 check-kinds})
  (pslide
    #:go heading-coord-left
    @rt{Complilation}
    #:go text-coord-mid
    @rrt{easy for Deep, Untyped}
    @rrt{Shallow, target future work}
    @rrt{thanks fritz}
    @rrt{boundary strategies})
  (pslide
    #:go heading-coord-left
    @ht{Compilation}
    #:go title-coord-mid
    #:go heading-coord-left
    @ht{Compilation}
    #:go title-coord-mid
    (DSU-pict 0)
    #:go icon-coord-mid
    @rrt{Deep types => wrapper}
    @rrt{Shallow types => check inputs})
  (pslide
    #:go heading-coord-left
    @ht{Properties}
    #:go text-coord-left
    (item-line-append
      (text-line-append
        @st{Type Soundness}
        @rrt{ types predict outcomes})
      (blank 0 small-y-sep)
      (text-line-append
        @st{Complete Monitoring}
        @rrt{ Deep types predict behaviors}))
    #:go text-coord-right
    (item-line-append
      (word-append vdash-deep @st{ e : T})
      (word-append vdash-shallow @st{ e : shape(T)})
      (word-append vdash-untyped @st{ e})))
  (void))

(define (sec:thesis:implementation)
  (pslide
    #:go heading-coord-left
    @ht{Map}
    #:go center-coord
    @rrt{ok model, down to impl now}
    )
  (pslide
    #:go heading-coord-left
    @ht{Typed Racket Compiler}
    #:go text-coord-mid
    (pipeline-append
      (double-frame
        (add-dark-background @rt{Expand}))
      (double-frame
        (add-dark-background
          (text-line-append
            @rt{Typecheck}
            @rt{Kernel})))
      (cons
        (double-frame
          (add-dark-background
            (text-line-append
              @rt{Generate} @rt{Contracts})))
        (double-frame
          (add-dark-background
            @rt{Insert Checks})))
      (double-frame
        (add-dark-background
          @rt{Type-Based Optimizer}))))
  (pslide
    #:go heading-coord-left
    @ht{Types to Shapes}
    #:go text-coord-mid
    ;; type-term color distinction?
    ;; add comment to each?
    ;; in general: check full constructor
    (word-append
      @rrt{(f x) } @sst{: T} @rrt{    --->    (check s (f x))})
    (blank 0 tiny-y-sep)
    (type-to-shape @sst{T} @rrt{s})
    #:go center-coord
    (item-line-append* (flatten
      (list
        (list
          (type-to-shape @sst{Num} @rrt{number?})
          (type-to-shape @sst{(Listof Num)} @rrt{list?}))
        (list
          (type-to-shape @sst{(U Num Sym)}
                         (text-line-append
                           @rrt{(or number?}
                           @rrt{    symbol?)}))
          (type-to-shape @sst{(-> Num Num)}
                         (text-line-append
                           @rrt{(and procedure?}
                           @rrt{     (arity-includes 1))})))))))
  (pslide
    #:go heading-coord-left
    @ht{Optimization}
    #:go title-coord-mid
    @rrt{(fl+ n m)}
    @rrt{(unsafe-fl+ n m)}
    (word-append
      @rrt{when  n } @sst{: Float} @rrt{  and  m } @sst{: Float}))
  (pslide
    #:go heading-coord-left
    @ht{Optimization}
    #:go title-coord-mid
    (table 4
           (pad-modulo 4 (map opt->pict optimization-name*))
           cc-superimpose cc-superimpose pico-x-sep small-y-sep)
    #:go (at-find-pict 'dead-code ct-find 'cc) fail-pict
    #:go (at-find-pict 'pair ct-find 'cc) fail-pict)
  ;; TODO opt examples? or save to end
  ;;  g (-> Str Str) case-lambda (x) x (x y) y
  ;;  ~~> case-lambda (x) x (x y) void
  ;;
  ;;  x (pairof (pairof nat int) str)
  ;;  cdar x -> unsafe-cdr unsafe-car x
  (pslide
    #:go heading-coord-left
    @ht{Map}
    #:go text-coord-mid
    @rrt{ready to evaluate})
  (void))

(define (sec:thesis:evaluation)
  ;; (pslide
  ;;   #:go heading-coord-left
  ;;   @ht{Thesis}
  ;;   #:go title-coord-mid
  ;;   @ht2{Deep and Shallow types can interoperate.}
  ;;   @rrt{(preserving their formal properties)}
  ;;   (blank 0 tiny-y-sep)
  ;;   @ht2{Programmers can use these types to:}
  ;;   (item-line-append
  ;;     (hb-append @st{- } @rt{strengthen Shallow guarantees})
  ;;     (hb-append @st{- } @rt{avoid unimportant Deep errors})
  ;;     (hb-append @st{- } @rt{lower runtime costs})))
  (pslide
    #:go heading-coord-left
    @rt{Shallow to Deep = stronger guarantees}
    #:go text-coord-mid (bad-pair-example 'U 'S 'U)
    #:go text-coord-mid (blank 0 tiny-y-sep) (bad-pair-example #f 'D #f)
    (blank 0 small-y-sep)
    (item-table
      @rt{Shallow: }
      (word-append
        (untyped-code "(\"A\", \"B\")")
        @rt{ is a  }
        (shallow-code "[Num, Num]"))
      @rt{Deep: }
      (word-append
        (untyped-code "(\"A\", \"B\")")
        @rt{ is NOT a  }
        (deep-code "[Num, Num]")))
    (blank 0 small-y-sep)
    #:next
    (takeaway-frame
      @rt{Deep types satisfy complete monitoring}))
  (pslide
    #:go heading-coord-left
    @rt{Deep to Shallow = fewer errors}
    #:go text-coord-mid
    (higher-order-any-example 'D 'U)
    (word-append error-pict
                 @rrt{ attempted to use higher-order})
    (word-append @rrt{value passed as } (deep-code "Any"))
    #:alt [#:go center-coord (frame-person "" "racket-users-ho-any.png" 8/10)]
    (blank 0 small-y-sep)
    (higher-order-any-example 'S 'U)
    success-pict
    #:next
    #:go title-coord-mid
    (takeaway-frame
      @rt{Shallow can run almost all type-correct code}))
  (pslide
    #:go heading-coord-left
    @rt{Deep to Shallow = simpler behavior}
    #:go text-coord-mid
    #:alt [(index-of-example 'U)]
    #:alt [(index-of-example 'D)]
    (index-of-example 'S)
    #:go icon-coord-mid
    (hc-append small-x-sep
      (word-append @rt{Untyped } (untyped-code " 0 "))
      (word-append @rt{Deep } (deep-code " #f "))
      (word-append @rt{Shallow } (shallow-code " 0 "))))
  (pslide
    #:go heading-coord-left
    @rt{Better Performance}
    #:go text-coord-mid
    #:alt [(sieve-example 'U 'U)]
    (make-2table
      #:col-sep med-x-sep
      #:row-sep med-y-sep
      (list
        (cons
          (word-append (tiny-sieve-example 'U 'U) @rrt{ ~ 2 sec.})
          @rt{Untyped baseline})
        (cons
          (lattice-vl-append
            (word-append (tiny-sieve-example 'U 'D) @rrt{ ~ 13 sec.})
            (word-append (tiny-sieve-example 'U 'S) @rrt{ ~ 4 sec.}))
          @rt{Mixed : Shallow wins})
        (cons
          (lattice-vl-append
            (word-append (tiny-sieve-example 'D 'D) @rrt{ < 2 sec.})
            (word-append (tiny-sieve-example 'S 'S) @rrt{ ~ 5 sec.}))
          @rt{Typed : Deep wins}))))
  (pslide
    #:go heading-coord-left
    @rt{Better Performance}
    ;; recall, large area under curve
    #:go text-coord-mid
    (big-overhead-plot 'jpeg '(D S))
    #:go icon-coord-mid
    @rt{Deep + Shallow = maximize D-deliverable cfgs.})
  (pslide
    #:go heading-coord-left
    @rt{Better Performance}
    ;; recall, large area under curve
    #:go text-coord-mid
    (make-2table
      #:row-sep tiny-y-sep
      #:col-sep small-x-sep
      (for/list ((sym* (in-list '((fsmoo dungeon) (suffixtree take5) (synth quadU)))))
        (for/list ((sym (in-list sym*)))
          (2col-overhead-plot sym '(D S))))))
  (pslide
    #:go heading-coord-left
    @rt{Better Performance}
    #:go text-coord-mid
    (fancy-table
      (list
        (list "Benchmark" "Worst Deep" "Worst Shallow")
        (list "sieve" "10x" "2x")
        (list "jpeg" "23x" "2x")
        (list "fsmoo" "451x" "4x")
        (list "dungeon" "14000x" "5x")
        (list "suffixt" "31x" "6x")
        (list "take5" "32x" "3x")
        (list "synth" "49x" "4x")
        (list "quadU" "60x" "8x"))))
  (pslide
    #:go heading-coord-left
    @rt{New Migration Plan}
    #:go text-coord-left
    (item-line-append
      (blank)
      (hb-append @st{- } @rt{begin with Deep types})
      (hb-append @st{- } @rt{use Shallow for speed})
      (hb-append @st{- } @rt{return to Deep at end}))
    #:go text-coord-right
    (lattice-vl-append
      (path-node '(U U U U U))
      (path-node '(D D D U U))
      (path-node '(D S S U U))
      (path-node '(D S S S S))
      (path-node '(D D D D D))))
  (pslide
    #:go heading-coord-left
    @rt{New Migration Plan}
    #:go text-coord-mid
    @rt{Percent of 3-deliverable paths}
    (fancy-table
      (list
        (list "Benchmark" "Deep or Shallow" "Deep and Shallow")
        (list "sieve" "0%" "100%")
        (list "jpeg" "100%" "100%")
        (list "fsmoo" "0%" "50%")
        (list "dungeon" "0%" "67%")
        (list "suffixt" "0%" "12%")
        (list "take5" "100%" "100%"))))
  ;; TODO flash a lattice, to understand the table ... maybe 2 lattices
  ;; TODO mixed-lattice table ((fsm 37.50) (morsecode 25.00) (jpeg 37.50) (kcfa 55.47) (zombie 6.25) (zordoz 46.88))
  ;; TODO mark ALL THESE as not in document, breaking news
  (void))

(define (sec:conclusion)
  (pslide
    #:go title-coord-mid
    @ht2{Deep and Shallow types can interoperate.}
    ;; TODO reword, using sec:thesis table from above?
    (item-table
      (blank) @rt{Natural + Transient}
      pass-pict @rt{preserves formal guarantees}
      pass-pict @rt{enables speedups})
    ;; instead of being stuck with pros/cons, can pick
    ;#:go icon-coord-mid
    ;(hc-append med-x-sep ds-model-pict ds-impl-pict)
    )
  (pslide
    #:go heading-coord-left
    @ht{Contributions}
    #:go title-coord-mid
    (item-line-append
      (hb-append @st{1. } @rt{performance analysis method})
      (hb-append @st{2. } @rt{design analysis method})
      (hb-append @st{3. } @rt{scaled-up Transient}) ;; found several ... issue-points
      (hb-append @st{4. } @rt{Deep + Shallow language}))
    #:go icon-coord-mid
    (hc-append small-x-sep scale-pict ruler-pict @rt{?} @rt{?}))
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
;    (sec:title)
;    (sec:intro)
;    (sec:perf)
;    (sec:design)
;    (sec:thesis)
;    (sec:conclusion)
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

    #:go text-coord-mid
    (bad-pair-example 'U 'T 'U)
    (hsep tiny-y-sep)
    (question-text
      (word-append
        @rrt{Does the type  } (deep-code "(List Num)") @rrt{  keep out the list of letters?}))
    #:go answer-coord-left
    (answer-text
      @rt{Yes!})
    (hsep small-y-sep)
    @rrt{Typed Racket}
    #:go answer-coord-right
    (answer-text
      @rt{No!})
    (hsep small-y-sep)
    @rrt{Reticulated Python}

;    (answer-text
;      (word-append @rt{Count } D-pict @rt{-deliverable configs}))
;    (text-line-append
;      (word-append
;        @rrt{For a HUGE space, count  } D-pict @rrt{-deliverable configs})
;      @rrt{ in a collection of random samples})
;    (hsep small-y-sep)
;    (scale (make-lattice 12 bits->path-node #:x-margin lattice-x-sep #:y-margin lattice-y-sep) 5/10)



  )))
