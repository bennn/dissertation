#lang at-exp slideshow

;; Defense slides, Deep and Shallow types.
;;  1 hour presentation? roughly

;; TODO
;; - [X] get colors ... three kingdoms?
;; - [X] pick fonts
;; - [X] jfp example, slides ... take from diss pict
;; - [X] outline, on paper
;; - [X] outline on slides
;; - [X] technics in order!
;; - [X] table pict
;; - [X] warring states pict, D S E + concrete, pyret
;;   - [X] similar pict for languages? for, before the design-space analysis?
;;   - [X] organize along perf + design dimensions
;;   - [-] deep perf = bumpy, dangerous bend (or R G Y stripes)
;;   - [-] shallow perf = bumpy, dangerous bend
;;   - [-] erased perf = "untyped" flag ... for better and worse
;;   - [-] interop "city" at D S border
;; - [X] thesis + supports pict
;; - [X] D S U picts, the safe one and others
;; - [-] sunrise, green, theme for "published" half
;; - [-] moonlight, blue, theme for "new" half
;; - [-] headlines ... gt terrific
;; - [X] two methods (major contributions)
;;   - [X] perf ... LHS 
;;   - [X] design ... RHS 
;; - [X] more takeaways at end, summarize?
;; - [X] cache huge lattice
;; - [-] NOTE: cannot ask if Java etc. is deep, its an FFI/Interop question

;; MF 2020-12-10
;; NO GIGGLING FRESHMAN remember Max and Asumu and there are senior people here
;;  that are assessing you! Be professional.
;; - [-] only say "I" "my" for thesis statement
;;       ... or, "I took over perf after Asumu had started"
;; - [-] don't confuse properties of model vs implementation
;;       eg "TR satisfies GG"
;; - [X] explain what CM is before jumping to example,
;;      "type system in contral of all checks that went through it, or blame if goes wrong"
;; - [X] cut scaling the model, all but final bullet point
;;       for picture show Natural N+T Transient and draw arrows to unite
;; - [X] thesis preview / prelim thesis , rests on contributions
;; - [X] need GOOD phrase to justify sampling
;;       ... confirmed with ground truth many settings
;; - [X] DON'T measure types,
;;       show: Need to measure the strength of guarantees
;;       say: Need to measure the strength of the guarantees that types offer
;; - [X] use non-green for the earth
;; - [X] move 'simpler behavior' to the end
;; - [X] why overhead line fuzzy? ... oh, thats a general gtp-plot issue
;; - [X] email Amal on Sunday
;; - [ ] (from the top) remind people of contributors, use pictures
;;       Jan, Zeina, Christos, Lukas ... anyone
;; - [ ] type lattice, always go up! make sure words match pictures
;; - [ ] carefully introduce Deep and Shallow,
;;       lots of words for these ... Sam used TR ... Max used Natural ... hm
;; - [ ] sync the benchmarks, for displays ... avoid intro new names

;; words
;; - [ ] migration, faces ... 0.9x

;; visuals
;; - [ ] fancier takeaway frame
;; - [ ] text size ... rt vs rrt

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
    performance-info->sample-info
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

(define xsep wsep)
(define ysep hsep)
(define xshift wshift)
(define yshift hshift)

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
(define contribution-x-left 10/100)
(define contribution-x-right 49/100)
(define contribution-y-top 26/100)
(define contribution-y-bot 56/100)

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
(define landscape-offset 26/1000)
(define sky-coord (coord 1/2 (- sky-y landscape-offset) 'cb))
(define earth-y 7/10)
(define earth-coord (coord 1/2 (+ earth-y landscape-offset) 'ct))
(define answer-y 60/100)
(define answer-coord-left (coord 20/100 answer-y 'ct))
(define answer-coord-mid (coord 1/2 answer-y 'ct))
(define answer-coord-right (coord 80/100 answer-y 'ct))
(define below-sky-y sky-y)
(define above-earth-y (- earth-y 35/1000))
(define low-text-left (coord text-left below-sky-y 'lt))
(define low-text-mid (coord 1/2 below-sky-y 'ct))
(define low-text-right (coord text-right below-sky-y 'rt))
(define bot-text-left (coord text-left above-earth-y 'lt))
(define bot-text-right (coord text-right above-earth-y 'rt))
(define overhead-coord-top (coord 1/2 18/100 'ct))
(define overhead-coord-mid (coord 1/2 36/100 'ct))

(define (landscape-w)
  (* 1.1 client-w))

(define (landscape-h)
  (h%->pixels 29/100 #;sky-y))

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
(define red1-3k1 (hex-triplet->color% #xC3476F))
(define red2-3k1 (hex-triplet->color% #xB0244C))
(define orange0-3k1 (hex-triplet->color% #xE6BD82))
(define orange1-3k1 (hex-triplet->color% #xFE9D73))
(define orange2-3k1 (hex-triplet->color% #xB87D4A))
(define yellow0-3k1 (hex-triplet->color% #xF8F7B3))
(define yellow1-3k1 (hex-triplet->color% #xDBC847))
(define blue0-3k1 (hex-triplet->color% #x6C78DF))
(define blue1-3k1 (hex-triplet->color% #x5155BF))
(define blue2-3k1 (hex-triplet->color% #x4F4DA1))
(define teal-3k1 (hex-triplet->color% #x8ADCB3))
(define black0-3k1 (hex-triplet->color% #x514F52))
(define black1-3k1 (hex-triplet->color% #x312F32))
(define grey-3k1 (hex-triplet->color% #x6C6685))
(define fog-3k1 (hex-triplet->color% #xDBCAC2))
(define darkfog-3k1 (hex-triplet->color% #xAB9A92))
(define title-3k1 (hex-triplet->color% #xF1E7DE))
(define author-3k1 (hex-triplet->color% #xE5E6E6))

(define untyped-pen-color yellow1-3k1)
(define deep-pen-color green2-3k1)
(define shallow-pen-color green0-3k1)
(define validate-pen-color red1-3k1)
(define neutral-pen-color grey-3k1)
(define highlight-pen-color orange1-3k1)

(define code-brush-alpha 0.6)

(define untyped-brush-color (color%-update-alpha yellow1-3k1 code-brush-alpha))
(define deep-brush-color (color%-update-alpha green2-3k1 (- code-brush-alpha 0.05)))
(define shallow-brush-color (color%-update-alpha green0-3k1 code-brush-alpha))
(define validate-brush-color (color%-update-alpha validate-pen-color code-brush-alpha))
(define neutral-brush-color fog-3k1)

(define background-color black0-3k1)
(define dark-background-color black1-3k1)
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

(define (initials f str)
  (word-append* (map (compose1 f string) (string->list str))))

(define (ht . str*)
  (ht* str*))

(define (ht* str*)
  (txt str* #:font title-text-font #:size title-text-size #:color title-text-color))

(define (ht2-initials str)
  (initials ht2 str))

(define (ht2 . str*)
  (ht2* str*))

(define (ht2* str*)
  (txt str* #:font title-rm-font #:size h2-text-size #:color subtitle-text-color))

(define (ht3 . str*)
  (ht3* str*))

(define (ht3* str*)
  (txt str* #:font title-rm-font #:size h3-text-size #:color subtitle-text-color))

(define (ht4 . str*)
  (ht4* str*))

(define (ht4* str*)
  (txt str* #:font title-rm-font #:size h3-text-size #:color body-text-color))

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

(define (item-r-append . pp*)
  (item-r-append* pp*))

(define (item-r-append* pp*)
  (apply vr-append item-line-sep pp*))

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

(define (bullet . pp*)
  (bullet* pp*))

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

;(define (word-append . pp*)
;  (word-append* pp*))
;
;(define (word-append* pp*)
;  (apply hc-append pp*))

(define (word-append . pp*)
  (word-append* pp*))

(define (word-append* pp*)
  (apply hb-append (map clip-descent pp*)))

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

(define (bcellophane* n pp)
  (for/fold ((pp pp))
            ((i (in-range n)))
    (bcellophane pp)))

(define (bcellophane2 pp)
  (bcellophane* 2 pp))

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

(define (frame-person name ps [pre-w% #f])
  (define w% (or pre-w% 15/100))
  (define pp (double-frame (scale-src-bitmap ps w%)))
  (if name
    (vl-append pp (ht3 (string-append "  " name)))
    pp))

(define (frame-picture ps [w% #f])
  (frame-person #f ps w%))

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

(define (frame-credits . name*)
  (frame-credits* name*))

(define (frame-credits* name*)
  (define micro-scale 1/10)
  (define xsep 2)
  (define pp* (for/list ((n (in-list name*))) (frame-person #f n micro-scale)))
  (scale (apply hc-append xsep pp*) 65/100))

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

(define-syntax-rule (with-plot-params w h base-color exp)
  (parameterize ([*OVERHEAD-MAX* MAX-OVERHEAD]
                 [*OVERHEAD-LEGEND?* #false]
                 [*OVERHEAD-PLOT-WIDTH* w]
                 [*OVERHEAD-PLOT-HEIGHT* h]
                 [*OVERHEAD-LINE-WIDTH* 3]
                 [*OVERHEAD-LINE-COLOR* base-color]
                 [*PEN-COLOR-CONVERTER* defense-pen-color-converter]
                 [*BRUSH-COLOR-CONVERTER* defense-brush-color-converter]
                 [*INTERVAL-ALPHA* code-brush-alpha]
                 [*MULTI-INTERVAL-ALPHA* code-brush-alpha])
    exp))

(define (add-neutral-background pp)
  (add-rectangle-background
    #:x-margin 0 #:y-margin 0
    #:color neutral-brush-color
    #:radius 1
    (add-rectangle-background
      #:x-margin small-x-sep #:y-margin tiny-y-sep
      #:color white
      pp)))

(define (add-dark-background pp #:x-margin [xm #f] #:y-margin [ym #f])
  (add-rectangle-background
    #:x-margin (or xm small-x-sep) #:y-margin (or ym tiny-y-sep)
    #:color background-color
    #:radius 1
    pp))

(define (takeaway-frame pp)
  (add-rounded-border
    #:radius 1
    #:x-margin (/ client-w 2) #:y-margin med-y-sep
    #:frame-width tiny-y-sep
    #:frame-color darkfog-3k1
    #:background-color dark-background-color
    pp))

(define (wide-takeaway-frame pp)
  (takeaway-frame (cc-superimpose (xsep client-w) pp)))

(define (wider pp)
  (hc-append (xsep small-x-sep) pp (xsep small-x-sep)))

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

(define (add-code-arrows pp . arrow*)
  (add-code-arrows* pp arrow*))

(define (add-code-arrows* pp arrow*)
  (for/fold ((pp pp))
            ((arrow (in-list arrow*)))
    (add-code-arrow pp arrow)))

(define (tag-append . x*)
  (string->symbol (string-join (map ~a x*) "-")))

(define plot-tag 'plot)
(define cm-client-tag 'cm:client)
(define cm-api-tag 'cm:api)
(define cm-library-tag 'cm:library)
(define cm-arrow-tag 'cm:arrow)
(define (at-cm-arrow #:abs-y [y-sep 0])
  (at-find-pict cm-arrow-tag lc-find 'rt #:abs-y y-sep))
(define cm-arrow-size 20)

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
  (apply vc-append lattice-y-sep (add-between pp* up-arrow-pict)))

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
    (add-code-arrows
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
  (define pp
    (text-line-append* pp*))
  (takeaway-frame
    (ht-append pp (xsep (w%->pixels 25/100)))))

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
     (define pre-pict
       (make-table
         (lattice-label-append tu-pict (tag-pict @rrt{@~a[n] components} 'component))
         (lattice-label-append lattice-pict @rrt{@~a[(expt 2 n)] configurations})))
     (ppict-do
       pre-pict
       #:go (at-find-pict 'component rc-find 'lc #:abs-x small-x-sep)
       right-arrow-pict))
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
         (lattice-label-append tu-pict (tag-pict @rrt{@~a[n] components} 'component))
         (lattice-label-append lattice-pict @rrt{@~a[(expt 2 n)] configurations})))
     (ppict-do
       pre-table
       #:go (at-find-pict 'component rc-find 'lc #:abs-x small-x-sep)
       right-arrow-pict
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
    lattice-pict
    @rt{= @~a[(exact-floor (pct (unbox *num-good) num-configs))]%}))

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

(define (cm-client-pict deco)
  (apply (dyn-codeblock deco) example-client-code*))

(define (cm-api-pict deco)
  (apply (dyn-codeblock deco) example-api-code*))

(define (cm-library-pict deco)
  (apply (dyn-codeblock deco) example-library-code*))

(define (cm-example deco* #:api-comment [api-comment #f])
  (let* ((client-pict
           (add-hubs (cm-client-pict (first deco*)) cm-client-tag))
         (api-pict
           (cm-api-pict (second deco*)))
         (api-pict
           (add-hubs
             (if api-comment
               (cc-superimpose (bcellophane* 2 api-pict) api-comment)
               api-pict)
             cm-api-tag))
         (third?
           (pair? (cddr deco*)))
         (lib-pict
           (add-hubs (if third? (cm-library-pict (third deco*)) (blank)) cm-library-tag))
         (full-pict
           (item-r-append (boundary-append client-pict api-pict) lib-pict))
         (arr
           (code-arrow (tag-append cm-library-tag 'W) lc-find
                       (tag-append cm-client-tag 'S) cb-find
                       (* 1/2 turn) (* 1/4 turn) 5/10 5/10 'solid))
         (full/arr
           (if third? 
             (add-code-arrow full-pict arr
                             #:arrow-size cm-arrow-size
                             #:label (tag-pict (blank) cm-arrow-tag))
             full-pict)))
    full/arr))

(define cm-num-str
  (hc-append @deep-code{Num} @rt{ , } @deep-code{Str}))

(define cm-str-num
  (hc-append @deep-code{Str} @rt{ , } @deep-code{Num}))

(define (cm-gets pp)
  (hc-append @rt{  gets  } pp))

(define (cm-title n)
  (define-values [lhs rhs]
    (case n
      ((0)
       (values happy-face
               @rt{  ...}))
      ((1)
       (values unhappy-face
               (cm-gets error-pict)))
      ((2)
       (values unhappy-face
               (cm-gets cm-str-num)))
      (else
        (raise-argument-error 'cm-title "(integer-in 0 X)" n))))
  (hc-append lhs
             @rt{  expects  } cm-num-str
             rhs))

(define (save-pict+ lbl pp)
  (save-pict (build-path src lbl) pp)
  pp)

(define q-pict @ht2{Q. })
(define a-pict @ht2{A. })
(define D-pict @bold-rt{D})

(define cm-callback-q
  (ht-append
    q-pict
    (text-line-append
      @rt{Do types protect}
      @rt{ the callback?})))

(define (arrowhead-pict rad)
  (colorize
    (arrowhead 20 rad)
    fog-3k1))

(define up-arrow-pict
  (arrowhead-pict (* 1/4 turn)))

(define right-arrow-pict
  (arrowhead-pict 0))

(define left-arrow-pict
  (arrowhead-pict (* 1/2 turn)))

(define down-arrow-pict
  (arrowhead-pict (* 3/4 turn)))

(define big-hyphen-pict @ht2{- })
(define hyphen-pict @st{- })
(define hyphen-ghost (bghost hyphen-pict))

(define (question-text pp)
  (word-append q-pict pp))

(define (answer-text pp)
  (word-append a-pict pp))

;; -----------------------------------------------------------------------------
;; --- extra pict

(define q-num-letter-pict
  (hc-append
    @rrt{Does the type  } (deep-code "Num")
    @rrt{  keep out the letter  } (untyped-code "\"A\"")
    @rrt{  ?}))

(define-values [why-perf-0 why-perf-1]
  (let* ((lhs (question-text @rt{Why?}))
         (rhs (answer-text @RT{Performance!})))
    (values (boundary-append lhs (bghost rhs))
            (boundary-append lhs rhs))))

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

(define (add-mt-originals pp)
  (ppict-do
    pp
    #:go (coord text-left 43/100 'lt)
    (frame-team @ht3{Gradual Typing} "siek.jpg" "taha.jpeg")
    #:go (coord 24/100 68/100 'ct)
    (frame-team @ht3{Migratory Typing} "tobin-hochstadt.jpg" "felleisen.jpg")
    #:go (coord 53/100 51/100 'ct)
    (frame-team (text-c-append @ht3{Multi-Language} @ht3{Semantics}) "matthews.jpeg" "findler.png")
    #:go (coord text-right 41/100 'rt)
    (frame-team @ht3{Hybrid Typing}
      "unknown.png" "knowles.jpeg" "freund.png" "tomb.jpg" "flanagan.png")))

(define (basic-star size #:color [color fog-3k1])
  (make-compass-pict size #:color fog-3k1))

(define (double-star size)
  (define fg (basic-star size))
  (define bg (basic-star (* 9/10 size)))
  (cc-superimpose (rotate bg (* 1/8 turn)) fg))

(define (theory-star size)
  ;; TODO randomize size / rotation? tag with venue + year?
  ;; size 8 ... 30 all reasonable
  ;; bib range from 1970 to 2020 ... 50 years
  (basic-star size))

(define (base-sky-pict)
  (define outer-color blue2-3k1)
  (define mid-color blue1-3k1)
  (define inner-color blue0-3k1)
  (define pen-width (landscape-line-width))
  (define w (landscape-w))
  (define h (landscape-h))
  (vc-append
    (filled-rectangle w h #:draw-border? #f #:color inner-color)
    (filled-rectangle w (* 1/2 pen-width) #:draw-border? #f #:color mid-color)
    (filled-rectangle w (* 1/2 pen-width) #:draw-border? #f #:color outer-color)))

(define gt-year0 1971)
(define gt-year1 2020)

(define (year->sky-x y)
  (define start gt-year0)
  (define end gt-year1)
  (unless (<= start y end)
    (raise-argument-error 'year->sky-x "(integer-in 1971 2020)" y))
  (/ (- y start) (- end start)))

(define sky-pico-star-pict 
  (theory-star 9))

(define sky-tiny-star-pict 
  (theory-star 16))

(define (originals-sky-pict)
  (let* ((pp (base-sky-pict))
         (star-pict sky-tiny-star-pict)
         (pp (ppict-do
               pp
               #:go (coord (year->sky-x 2006) 50/100 'cc)
               (vc-append star-pict (hc-append pico-x-sep star-pict star-pict) star-pict))))
    pp))

(define (research-sky-pict)
  (let* ((pp (base-sky-pict))
         (num-col 9)
         (star-pict sky-tiny-star-pict)
         (star-pict2 (cc-superimpose (bghost star-pict) sky-pico-star-pict))
         (*curr (box 0))
         (pp
           (for/fold ((pp pp))
                     ((yr (in-range 1971 2020)))
            (ppict-do pp
              #:go (coord (year->sky-x yr) 2/100 'ct)
              (apply vc-append pico-y-sep
                     (for/list ((_i (in-range num-col)))
                       (set-box! *curr (+ (unbox *curr) 1))
                       (if (zero? (modulo (unbox *curr) 2))
                         star-pict2
                         star-pict)))))))
    pp))

(define (semantics-sky-pict #:names? [names? #true])
  (let* ((pp (base-sky-pict))
         (pp
           (for/fold ((pp pp))
                     ((txt (in-list (list natural-pict conatural-pict forgetful-pict transient-pict amnesic-pict erasure-pict)))
                      (crd (in-list (list natural-coord-sky conatural-coord-sky forgetful-coord-sky transient-coord-sky amnesic-coord-sky erasure-coord-sky))))
             (ppict-do pp
               #:go crd (hc-append big-theory-star (if names? txt (blank)))))))
    pp))

(define (sky-pict)
  (research-sky-pict))

(define (caption-text pp)
  (vl-append
    (ysep tiny-y-sep)
    (clip-ascent pp)))

(define natural-tag 'natural)
(define conatural-tag 'conatural)
(define forgetful-tag 'forgetful)
(define transient-tag 'transient)
(define amnesic-tag 'amnesic)
(define erasure-tag 'erasure)
(define NT-tag 'NT)

(define natural-coord-sky (coord 10/100 66/100 'lt))
(define conatural-coord-sky (coord 16/100 27/100 'lt))
(define amnesic-coord-sky (coord 36/100 62/100 'lt))
(define forgetful-coord-sky (coord 52/100 21/100 'lt))
(define transient-coord-sky (coord 60/100 54/100 'lt))
(define erasure-coord-sky (coord 80/100 32/100 'lt))

(define natural-pict (add-hubs @rrt{Natural} natural-tag))
(define conatural-pict (add-hubs @rrt{Co-Natural} conatural-tag))
(define forgetful-pict (add-hubs @rrt{Forgetful} forgetful-tag))
(define transient-pict (add-hubs @rrt{Transient} transient-tag))
(define amnesic-pict (add-hubs @rrt{Amnesic} amnesic-tag))
(define erasure-pict (add-hubs @rrt{Erasure} erasure-tag))

(define big-theory-star
  (theory-star 22))

(define huge-theory-star
  (double-star 40))

(define NT-pict
  (add-hubs huge-theory-star NT-tag))

(define (sky-pict-transient)
  (let* ((pp (base-sky-pict))
         (pp
          (ppict-do
            pp
            #:go (coord 20/100 5/10 'ct)
            natural-pict
            #:go (coord 78/100 4/10 'ct)
            transient-pict
            #:go (coord 55/100 6/10 'ct)
            NT-pict))
         (arr*
           (list
             (code-arrow (tag-append natural-tag 'E) rc-find
                         (tag-append NT-tag 'W) lc-find
                         0 0 1/4 1/4 'short-dash)
             (code-arrow (tag-append transient-tag 'W) lc-find
                         (tag-append NT-tag 'E) rc-find
                         (* 1/2 turn) (* 1/2 turn) 1/4 1/4 'short-dash))))
    (add-code-arrows* pp arr*)))

(define (base-earth-pict)
  (define inner-color red0-3k1)
  (define mid-color red1-3k1)
  (define outer-color red2-3k1)
  (define pen-width (landscape-line-width))
  (define w (landscape-w))
  (define h (landscape-h))
  (vc-append
    (filled-rectangle w (* 1/2 pen-width) #:draw-border? #f #:color outer-color)
    (filled-rectangle w (* 1/2 pen-width) #:draw-border? #f #:color mid-color)
    (filled-rectangle w h #:draw-border? #f #:color inner-color)))

(define (symbol->lang-pict sym)
  (path->lang-pict (glob1 (build-path src "lang" (format "~a.png" sym)))))

(define (glob1 pat)
  (define m* (glob pat))
  (cond
    [(or (null? m*) (not (null? (cdr m*))))
     (raise-arguments-error 'glob1 "expected 1 match" "match*" m*)]
    [else
      (car m*)]))

(define (path->lang-pict path)
  (scale-lang-bitmap (bitmap path)))

(define (earth-add-all-lang* pp [pre-render-one #f])
  (define render-one
    (or pre-render-one
        path->lang-pict))
  (define lang-path* (glob (build-path src "lang" "*.png")))
  (for/fold ((pp pp))
              ((lang-path (in-list lang-path*))
               (pre-x (in-range (+ gt-year0 6) (- gt-year1 2) 3))
               (y (in-cycle (in-list '(28 68)))))
    (ppict-do pp #:go (coord (year->sky-x pre-x) (/ y 100) 'cc) (render-one lang-path))))

(define (earth-add-left* pp sym*)
  (earth-add-lang-stack* pp sym* (coord 12/100 28/100 'lc)))

(define (earth-add-right* pp sym*)
  (earth-add-lang-stack* pp sym* (coord 92/100 55/100 'rc)))

(define (earth-add-lang-stack* pp sym* crd)
  (ppict-do
    pp
    #:go crd
    (earth-lang-stack sym*)))

(define (earth-lang-stack sym*)
  (define the-blank (blank 220 0))
  (for/fold ((acc (blank)))
            ((sym (in-list sym*)))
    (hc-append (- small-x-sep) acc (symbol->lang-pict sym))
    #;
    (rc-superimpose acc
                    (cc-superimpose the-blank (symbol->lang-pict sym)))))

(define (earth-pict #:mode [mode #f])
  (let* ((pp (base-earth-pict))
         (pp
           (case mode
             ((erasure)
              (earth-add-left*
                (earth-add-right* pp '(clojure flow javascript hack lua php pyre typescript))
                '(dart pyret python racket thorn)))
             ((post-erasure)
              (earth-add-left*
                pp
                '(dart pyret python racket thorn)))
             ((forgetful)
              (earth-add-left*
                (earth-add-right* pp '(pyret python))
                '(dart thorn racket)))
             ((tr-begin)
              (define (hide-non-tr ps)
                (define fn (path->string (file-name-from-path ps)))
                (if (string-prefix? fn "racket")
                  (path->lang-pict ps)
                  (bcellophane2 (path->lang-pict ps))))
              (earth-add-all-lang* pp hide-non-tr))
             ((rp-begin)
              (define (hide-non-rp ps)
                (define fn (path->string (file-name-from-path ps)))
                (if (string-prefix? fn "python")
                  (path->lang-pict ps)
                  (bcellophane2 (path->lang-pict ps))))
              (earth-add-all-lang* pp hide-non-rp))
             ((tr-rp)
              (define (hide-non-tr/rp ps)
                (define fn (path->string (file-name-from-path ps)))
                (if (or (string-prefix? fn "racket")
                        (string-prefix? fn "python"))
                  (path->lang-pict ps)
                  (bcellophane2 (path->lang-pict ps))))
              (earth-add-all-lang* pp hide-non-tr/rp))
             ((#f) ;; show all
              (earth-add-all-lang* pp))
             (else
               (raise-argument-error 'earth-pict "earth-mode?" mode)))))
    pp))

(define racket-path (build-path src "lang" "racket.png"))

(define (earth-pict-transient)
  (let* ((pp (base-earth-pict)))
    (ppict-do
      pp
      #:go (coord 40/100 2/10 'ct)
      (hc-append
        (scale-lang-bitmap (bitmap racket-path))
        @rrt{  + Transient}))))

(define pass-pict
  (check-pict 40))

(define fail-pict
  (x-pict 40))

(define pass-fail-icon
  (let* ((top pass-pict #;(check-pict 32))
         (bot fail-pict #;(x-pict 32)))
    (hc-append tiny-x-sep top bot)))

(define ds-model-pict
  @rt{Model})

(define ds-impl-pict
  @rt{Implementation})

(define ershov-pict
  (frame-person "Ershov" "ershov.png" 20/100))

(define tally-pict
  (filled-rectangle 4 small-y-sep #:color neutral-brush-color #:draw-border? #f))

(define the-boundary-pict
  (boundary-node '(U D U D D)))

(define the-boundary-q-pict
  (vc-append
    the-boundary-pict
    (question-text
      @rt{What happens at the boundaries?})))

(define trie-code* '(
  "(require pfds/trie)"
  ""
  "(define t (trie ....))"
  "(time (bind t ....))"
))

(define (scale-trie pp)
  (scale pp 7/10))

(define trie-untyped
  (scale-trie (apply untyped-codeblock trie-code*)))

(define trie-typed
  (scale-trie (apply typed-codeblock trie-code*)))

(define (trie-append lhs rhs)
  (hc-append tiny-x-sep lhs rhs))

(define trie-lhs
  (trie-append trie-untyped @rt{12 seconds}))

(define trie-rhs
  (trie-append trie-typed @rt{1 ms!}))

(define perf-tr-team
  '("greenman.png" "takikawa.png" "new.jpg" "feltey.png" "findler.png" "vitek.jpg" "felleisen.jpg"))

(define perf-rp-team
  '("greenman.png" "migeed.png"))

(define (collect-benchmarks-pict sym)
  (case sym
    ((U)
     (boundary-append
       (path-node '(U U U U U U))
       (path-node '(T T))
       (path-node '(U U U))))
    ((T)
     (boundary-append
       (path-node '(T T T T T T))
       (path-node '(T T))
       (path-node '(T T T))))
    (else
      (raise-argument-error 'collect-benchmarks-pict "(or/c 'U 'T)" sym))))

(define example-client-code* '(
  "(t-fold-file \"file.txt\" 0 count)"
  ""
  "(define (count acc str)"
  "  (+ 1 acc))"))

(define example-api-code* '(
  "(: t-fold-file"
  "   (-> Path Num"
  "       (-> Num Str Num)"
  "       Num))"
  ""
  "(define t-fold-file u-fold-file)"))

(define example-library-code* '(
  "(define (u-fold-file path acc f)"
  "  ; read str from path"
  "  ... (f str acc) ...)"))

(define unhappy-face (small-face 'unhappy))
(define unhappier-face (small-face 'sortof-unhappy))
(define happy-face (small-face 'happy))
(define happier-face (small-face 'happier))
(define face-offset-right (- tiny-x-sep))

(define (migration-face i)
  (define f0 happy-face)
  (define f1 unhappier-face)
  (define f2 happier-face)
  (define arrow-sep pico-x-sep)
  (define caption-sep tiny-x-sep)
  (define (mk3 top mid bot)
    (hc-append (vl-append small-y-sep (tag-pict top 'top) (tag-pict bot 'bot)) (tag-pict mid 'mid)))
  (define (arrow1 pp)
    (ppict-do
      pp
      #:go (at-find-pict 'bot rc-find 'lb #:abs-x arrow-sep)
      up-arrow-pict))
  (define (arrow2 pp)
    (ppict-do
      (arrow1 pp)
      #:go (at-find-pict 'top rc-find 'lt #:abs-x arrow-sep)
      up-arrow-pict))
  (let loop ((i i))
    (case i
      ((0)
       (mk3 (bghost f2) (bghost f1) f0))
      ((1)
       (arrow1 (mk3 (bghost f2) f1 f0)))
      ((2)
       (arrow2 (mk3 f2 f1 f0)))
      ((3)
       (ppict-do
         (migration-face 2)
         #:go (at-find-pict 'bot lc-find 'rc #:abs-x (- caption-sep)) @rt{1x}
         #:go (at-find-pict 'mid rc-find 'lc #:abs-x caption-sep) @rt{20x}
         #:go (at-find-pict 'top lc-find 'rc #:abs-x (- caption-sep)) @rt{.9x}))
      (else
        (raise-argument-error 'migration-face "(integer-in 0 3)" i)))))

(define how-to-guarantees-pict
  (ht-append
    @ht2{* }
    (text-line-append
      @rt{How to assess}
      @rt{ type guarantees})))

(define how-to-perf-pict
  (ht-append
    @ht2{* }
    (text-line-append
      @rt{How to measure}
      @rt{ performance})))

(define the-problem-pict
  (text-c-append
    how-to-perf-pict
    (ysep tiny-y-sep)
    @rrt{(the problem)}))

(define benchmark-source-pict
  (let* ((bg (disk 90 #:draw-border? #f #:color fog-3k1))
         (pp*
          (for/list ((name (in-list '("github.png" "neu.png" "planet.png"))))
            (define pp (scale-lang-bitmap (src-bitmap name)))
            (if (string-prefix? name "planet")
              (ppict-do bg #:go (coord 54/100 54/100 'cc) pp)
              (cc-superimpose bg pp))))
         (pp
          (hc-append
            (vc-append (first pp*) (second pp*))
            (third pp*))))
    (scale pp 6/10)))

(define perf-step1-pict
  (word-append @ht2{Step 1: } @rt{Benchmarks}))

(define perf-step2-pict
  (word-append @ht2{Step 2: } @rt{How to Measure}))

(define perf-step3-pict
  (word-append @ht2{Step 3: } @rt{Summarize with a Picture}))

(define (perf-make-sampling-lattice)
  (let* ((pp (make-lattice 11 bits->path-node #:x-margin pico-x-sep #:y-margin lattice-y-sep))
         (pp (scale pp 5/10))
         (pp (inset/clip pp (* -495/1000 (pict-width pp)) 0)))
    (save-pict "src/sampling-lattice.png" pp)
    pp))

(define perf-D-answer-pict
  (answer-text (word-append @rt{Count } D-pict @rt{-deliverable configs})))

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
      "(f (λ \"A\"))"
      )
    ((dyn-codeblock mid)
      "(define (f (x : (-> Num)))"
      "  (g x))")
    ((dyn-codeblock rhs)
      "(define (g y)"
      "  (.... y))")))

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

(define big-plot-ysep 4)

(define (big-sampling-plot bm-name deco)
  (define pi ((deco->pi bm-name) deco))
  (define num-samples 10)
  (define sample-rate 10)
  (define num-units (performance-info->num-units pi))
  (define pp (ss-validate-plot pi (w%->pixels 65/100) (h%->pixels 35/100)))
  (vl-append big-plot-ysep
    (hc-append
      (add-dark-background (rrt (~a bm-name)) #:x-margin 0 #:y-margin 0)
      (xsep (* 1.5 med-x-sep))
      @rrt{@~a[num-samples] samples of @~a[(* sample-rate num-units)] configs})
    (add-xticks (double-frame pp))))

(define (big-overhead-plot bm-name deco*)
  (define pi* (map (deco->pi bm-name) deco*))
  (define pp (ss-overhead-plot pi* (w%->pixels 65/100) (h%->pixels 35/100)))
  (vl-append big-plot-ysep
    (rrt (~a bm-name))
    (add-xticks (double-frame pp))))

(define (2col-overhead-plot bm-name deco*)
  (define pi* (map (deco->pi bm-name) deco*))
  (define pp (ss-overhead-plot pi* (w%->pixels 38/100) (h%->pixels 16/100)))
  (vl-append 4
    (tiny-txt (~a bm-name))
    (add-tiny-xticks (double-frame pp))))

(define (tiny-overhead-plot bm-name deco*)
  (define pi* (map (deco->pi bm-name) deco*))
  (define pp (ss-overhead-plot pi* (w%->pixels 38/100) (h%->pixels 16/100)))
  (double-frame pp))

(define (2col-overhead-plot* deco bm-name*)
  (make-2table
    #:row-sep small-y-sep
    (map (lambda (bm-name) (2col-overhead-plot bm-name (list deco))) bm-name*)))

(define (ss-overhead-plot pi* w h)
  (define base-color
    (if (and (not (null? pi*))
             (null? (cdr pi*))
             (reticulated-info? (car pi*)))
      1
      0))
  (define pp
    (with-plot-params w h base-color
      (overhead-plot pi*)))
  pp)

(define (ss-validate-plot pi w h)
  (define sample-pi (performance-info->sample-info pi #:replacement? #f))
  (define base-color 0)
  (define pp
    (with-plot-params w h base-color
      (validate-samples-plot pi sample-pi)))
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

(define (deco->sampling-pi bm-name d)
  (case d
    ((D)
     (performance-info->sample-info ((deco->pi bm-name) d)))
    (else (raise-argument-error 'deco->sampling-pi "(or/c 'D)" d))))

(define (add-xticks pp)
  (ppict-do
    pp
    #:go (coord 0 1 'lt) @rrt{1x}
    #:go (coord 1/4 1 'lt) @rrt{2x}
    #:go (coord 3/4 1 'ct) @rrt{10x}
    #:go (coord 1 1 'rt) @rrt{20x}))

(define (add-tiny-xticks pp)
  (ppict-do
    pp
    #:go (coord 1/4 1 'lt) @tiny-txt{2x}
    #:go (coord 1 1 'rt) @tiny-txt{20x}))

(define (add-x-label pp)
  (ppict-do
    pp
    #:go (at-find-pict plot-tag lb-find 'rc)
    (vl-append
      (ysep (* 1.7 tiny-y-sep))
      (word-append D-pict @rt{ = }))))

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
  "And I have long since taught myself to think that if I reproduce somebody's
  guess in my work, I should not regret not having been the first, but, on the
  contrary, should always bear it in mind that it is a major stimulus: since a
  similar idea has occured to me living thousands of kilometers away, it means
  that there really is something in it")

(define (sec:intro)
  (pslide
    #:go heading-coord-left
    @ht2{On Great Ideas}
    ;; A.P. Ershov 1983
    ;; - correctness of optimizing compiler
    ;; - second literacy
    ;; ... one day at conference, idea to colleague, memo of same idea,
    ;;     this ershovs response, compliment
    #:go text-coord-mid
    (blockquote
      @ht4{If I reproduce somebody's guess}
      @ht4{ in my work ...}
      blockquote-newline
      @ht4{ me living far away ...}
      blockquote-newline
      @ht4{ it means that}
      @ht4{ there really is something in it.})
    #:go heading-coord-right ershov-pict)
  (pslide
    #:go heading-coord-mid
    (takeaway-frame
      (vc-append pico-y-sep
        @ht2{Great Idea:}
        (word-append
          @rt{mixing } @rt-deep2{typed} @rt{ and } @rt-untyped2{untyped} @rt{ code})
        (hsep item-line-sep)
        mixed-typed-pict))
    #:set (add-mt-originals ppict-do-state))
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
    the-boundary-q-pict
    #:next
    (ysep (h%->pixels 14/100))
    (item-c-append
      q-num-letter-pict
      (basic-example 'T 'U #:arrow? #true)))
  ;; return to Q in a minute, first more history / context
  ;;  ... now that you know roughly what solving, more about solutions
  (pslide
    #:alt [
     #:set (add-mt-originals ppict-do-state)
     #:next
     #:go sky-coord (originals-sky-pict)
     #:go low-text-left (caption-text @rrt{research landscape})]
    #:alt [
     #:go sky-coord (research-sky-pict)
     #:go low-text-left (caption-text @rrt{research landscape ... over 200 publications})
     #:next
     #:go (coord 1/2 46/100 'ct) (scale the-boundary-q-pict 8/10) ]
    #:go sky-coord (semantics-sky-pict #:names? #f)
    #:go (coord 1/2 46/100 'ct) (scale the-boundary-q-pict 8/10)
    #:go low-text-left
    (item-line-append
      (caption-text @rrt{research landscape ... over 200 publications})
      @rrt{ 6+ ideas for boundaries}))
  (pslide
    #:go sky-coord (semantics-sky-pict #:names? #f)
    #:alt [
     #:go low-text-left (caption-text @rrt{research landscape})
     #:next
     #:go earth-coord (earth-pict)
     #:go bot-text-left (word-append @rrt{language landscape ... many implementations}) ]
    #:go earth-coord (earth-pict)
    #:go center-coord
    @ht2{Mixed-Typed Design Space}
    @rt{Lively, but Disorganized!} )
  (pslide
    ;; example disagreement
    #:go text-coord-mid
    (item-c-append
      (basic-example 'T 'U)
      (hc-append q-pict q-num-letter-pict))
    #:alt [#:go earth-coord (earth-pict)]
    #:go earth-coord (earth-pict #:mode 'erasure)
    #:go answer-coord-left (answer-text @rt{Yes!})
    #:go answer-coord-right (answer-text @rt{No}))
  (pslide
    #:go text-coord-mid
    (item-c-append
      (bad-fun-example 'U 'T 'U)
      (hc-append
        q-pict
        @rrt{Can the type  } (deep-code "(-> Num)") @rrt{  detect bad functions?}))
    #:alt [ #:go earth-coord (earth-pict #:mode 'post-erasure) ]
    #:go answer-coord-left (answer-text @rt{Yes})
    #:go answer-coord-right (answer-text @rt{No})
    #:go earth-coord (earth-pict #:mode 'forgetful))
  (pslide
    #:go sky-coord (semantics-sky-pict #:names? #false)
    #:go earth-coord (earth-pict)
    #:go (coord 1/2 42/100 'ct)
    #:alt [
      (question-text @rt{What happens at the boundaries?})
      #:go answer-coord-mid
      (boundary-append
        (answer-text @rrt{Nothing})
        (answer-text @rrt{Spot-checks})
        (answer-text @rrt{Everything!})
        (answer-text @rrt{...}))
    ]
    #:alt [
     #:next
     #:alt [why-perf-0]
     why-perf-1
     #:next
     #:go answer-coord-mid (question-text @rt{Where's the data?})
    ]
    #:go center-coord (frame-picture "elephant.jpg" 78/100))
  (pslide
    #:go sky-coord (semantics-sky-pict #:names? #false)
    #:go earth-coord (earth-pict)
    #:go (coord text-left 34/100 'lt)
    (text-line-append
      @ht2{My Research}
      @rt{ brings order to}
      @rt{ the design space})
    #:go (coord contribution-x-right below-sky-y 'lt)
    how-to-guarantees-pict
    #:go (coord contribution-x-right contribution-y-bot 'lt)
    how-to-perf-pict)
  (pslide
    #:next
    #:go (coord contribution-x-left below-sky-y 'lt)
    the-problem-pict
    #:go (coord contribution-x-right below-sky-y 'lt)
    (text-c-append
      how-to-guarantees-pict
      (ysep tiny-y-sep)
      @rrt{(solution space)})
    #:next
    #:go (coord 1/2 56/100 'ct)
    ;; TODO ? highlight promising points, to illustrate Deep and Shallow?
    (text-line-append
      @ht2{Thesis Preview:}
      @rt{ Deep and Shallow types can interoperate}))
  (void))

(define (sec:perf)
  (pslide
    #:go earth-coord (earth-pict #:mode 'tr-begin)
    #:go (coord contribution-x-left below-sky-y 'lt) the-problem-pict)
  (pslide
    #:go earth-coord (earth-pict #:mode 'tr-begin)
    #:go heading-coord-left
    @ht2{Typed Racket}
    #:go text-coord-left
    (rrt-bullet
      "Oldest, strongest mixed-typed language"
      "Home of severe performance costs"))
  (pslide
    #:go earth-coord (earth-pict #:mode 'tr-begin)
    #:go heading-coord-left @ht2{Costs ...}
    #:go text-coord-mid
    @rt{25x to 50x}
    (ysep tiny-y-sep)
    (frame-picture "math-array.png" 8/10))
  (pslide
    #:go earth-coord (earth-pict #:mode 'tr-begin)
    #:go heading-coord-left @ht2{... More Costs}
    #:go text-coord-mid
    (frame-picture "trie-small.png" 45/100)
    (hsep small-y-sep)
    #:alt [(boundary-append trie-lhs (bghost trie-rhs))]
    (boundary-append trie-lhs trie-rhs))
  (pslide
    #:go earth-coord (earth-pict #:mode 'tr-begin)
    #:go heading-coord-left
    @ht2{Typed Racket, Performance}
    #:go text-coord-left (rrt-bullet "Clearly, problems exist")
    #:next
    #:go center-coord
    (takeaway-frame (wider @ht2{Need a way to measure!})))
  (pslide
    #:go earth-coord (earth-pict #:mode 'tr-begin)
    ;; 21 total, variety size purpose
    #:go heading-coord-left perf-step1-pict
    #:go heading-coord-right
    (frame-credits* perf-tr-team)
    #:next
    #:go (coord 10/100 35/100 'cc) benchmark-source-pict
    #:go title-coord-mid
    @rt{Collected small, useful programs}
    (ysep tiny-y-sep)
    (collect-benchmarks-pict 'U)
    (hsep med-y-sep)
    #:next
    @rt{Added types, if missing}
    (ysep tiny-y-sep)
    (collect-benchmarks-pict 'T))
  #;(pslide
    #:go earth-coord (earth-pict #:mode 'tr-begin)
    #:go heading-coord-left @ht2{Step 1: Benchmarks}
    #:go text-coord-left (rt-bullet "21 benchmark in total" "from 2 to 14 modules" "games, apps, libraries, ...."))
  (pslide
    #:go earth-coord (earth-pict #:mode 'tr-begin)
    #:go heading-coord-left perf-step1-pict
    #:go heading-coord-mid
    (ysep (h%->pixels 14/100))
    (hc-append (path-node '(U U U U U)) @rt{  +  } (path-node '(T U)))
    (ysep pico-y-sep)
    (scale-src-bitmap "jpeg-description.png" 95/100))
  (pslide
    #:go earth-coord (earth-pict #:mode 'tr-begin)
    #:go heading-coord-left perf-step2-pict
    #:go text-coord-mid
    @rt{What to measure = all configurations}
    #:next
    (hsep small-y-sep)
    #:alt [(what-to-measure 'small)]
    #:alt [(what-to-measure 'large)]
    (hsep small-y-sep)
    (ht-append
      med-x-sep
      (question-text @rt{How to study?})
      (question-text @rt{How to scale?}))
    #:next
    (hsep med-y-sep)
    (answer-text @rt{Focus on the programmer ...}))
  (pslide
    #:go earth-coord (earth-pict #:mode 'tr-begin)
    #:go (coord 30/100 text-top 'ct)
    (migration-append
      (path-node '(T U T T T))
      (path-node '(T U T U U))
      (path-node '(T U U U U))
      (path-node '(U U U U U)))
    #:go (coord 70/100 text-top 'ct)
    #:alt [(migration-face 0)]
    #:alt [(migration-face 1)]
    #:alt [(migration-face 2)]
    (migration-face 3))
  (pslide
    #:go earth-coord (earth-pict #:mode 'tr-begin)
    #:go heading-coord-left perf-step2-pict
    #:go text-coord-mid perf-D-answer-pict
    (hsep (h%->pixels 8/100))
    #:next
    (make-2table
      #:row-align ct-superimpose
      #:col-align ct-superimpose
      (list
        (text-line-append
          (word-append
            @rrt{If  } D-pict @rrt{=4, then count})
          (blank)
          @rrt{configs with at most}
          (blank)
          @rrt{4x overhead})
        (d-lattice lattice-small-n))))
  (pslide
    #:go heading-coord-left perf-step2-pict
    #:go (coord 1/2 55/100 'ct) (src-bitmap "sampling-lattice.png") ;; (perf-make-sampling-lattice)
    #:go text-coord-mid perf-D-answer-pict
    #:next
    (hsep small-y-sep)
    (word-append D-pict @rrt{-deliverable  ~  Bernoulli random variable})
    (hsep small-y-sep)
    (word-append @rrt{linear-size sampling works}))
  (pslide
    #:go earth-coord (earth-pict #:mode 'tr-begin)
    #:go heading-coord-left perf-step3-pict
    #:next
    #:go text-coord-mid (tag-pict (big-overhead-plot 'quadU '(D)) plot-tag)
    #:set (add-x-label ppict-do-state)
    #:next
    #:go text-coord-mid (big-sampling-plot 'quadU 'D))
  (pslide
    #:go earth-coord (earth-pict #:mode 'tr-begin)
    #:go heading-coord-left @ht2{Performance Method}
    #:next
    #:go text-coord-mid
    (table
      2
      (list
        (hc-append @st{1. } @rrt{collect mixed-typed benchmarks})
        (scale (make-lattice 2 bits->path-node #:x-margin lattice-x-sep #:y-margin lattice-y-sep) 5/10)
        (hc-append @st{2. } (vl-append tiny-y-sep (word-append @rrt{count  } D-pict @rrt{-deliverable configs}) @rrt{ (or sample)}))
        (vc-append (ysep small-y-sep) pass-fail-icon)
        (hc-append @st{3. } (word-append @rrt{plot results}))
        (scale (tiny-overhead-plot 'quadU '(D)) 6/10))
      (cons lt-superimpose ct-superimpose)
      lt-superimpose
      tiny-x-sep
      small-y-sep))
  (pslide
    #:go earth-coord (earth-pict #:mode 'tr-rp)
    #:go text-coord-mid
    @ht2{Applications:}
    (ysep tiny-y-sep)
    (make-2table
      #:row-sep tiny-y-sep
      (list
        (symbol->lang-pict 'racket) @rt{Typed Racket}
        (symbol->lang-pict 'python) @rt{Reticulated Python})))
  (pslide
    #:go earth-coord (earth-pict #:mode 'tr-begin)
    #:go heading-coord-left (word-append @ht2{Typed Racket} @rrt{  some results from our 21 benchmarks})
    #:go overhead-coord-mid (2col-overhead-plot* 'D '(jpeg suffixtree take5 synth))
    #:next
    #:go overhead-coord-mid (wide-takeaway-frame @ht2{Bad}))
  (pslide
    #:go earth-coord (earth-pict #:mode 'rp-begin)
    #:go heading-coord-left (word-append @ht2{Reticulated Python} @rrt{   different benchmarks})
    #:go overhead-coord-top (src-bitmap "retic-overhead.png") #;(let ((pp (2col-overhead-plot* 'S '(spectralnorm pystone chaos go)))) (save-pict+ "retic-overhead.png" (ct-superimpose (blank (pict-width pp) (+ (pict-height pp) med-x-sep)) pp)))
    #:next
    #:go overhead-coord-mid (wide-takeaway-frame @ht2{Not so bad}))
  (void))

(define (sec:design)
  (pslide
    #:go earth-coord (earth-pict #:mode 'tr-rp)
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
    #:go title-coord-mid
    (cm-example '(U T))
    #:go heading-coord-left
    #:alt [(cm-title 0)]
    (cm-title 1))
  (pslide
    #:go heading-coord-left
    (cm-title 2)
    #:go title-coord-mid
    #:alt [(cm-example '(U T U))]
    (cm-example '(U T U)
                #:api-comment
                (vc-append
                  cm-callback-q
                  (hsep tiny-y-sep)
                  (answer-text
                    @rt{TR = Yes})
                  (answer-text
                    @rt{RP = No}))))
  (pslide
    #:go earth-coord
    (earth-pict)
    #:go sky-coord
    (sky-pict)
    #:next
    #:go center-coord
    ;; TODO show boundary pict here?
    (takeaway-frame
      @ht2{Need to measure type guarantees}))
  (pslide
    #:go sky-coord
    (sky-pict)
    #:go (coord contribution-x-right contribution-y-top 'lt)
    ;; .... need a way to measure the strength of guarantees that types provide
    (text-line-append @rt{How to assess} @rt{ type guarantees}))
  (pslide
    #:go sky-coord
    (sky-pict)
    #:go center-coord
    (item-line-append
      @rrt{before =  Sound vs Unsound}
      @rrt{after = Complete Monitoring, Type S., Tag S., Unsound}
      @rrt{        + Blame, Error Preorder}))
  (pslide
    #:go sky-coord
    (sky-pict)
    #:go center-coord
    ;; ... syntactic proporty
    (item-line-append
      (hc-append
        med-x-sep
        @rrt{star =  Complete Monitoring}
        (scale the-boundary-pict 60/100))
      @rrt{Do types supervise all boundaries?}))
  (pslide
    #:go heading-coord-left
    ;; TODO use the sky-colors for each
    (word-append
      @ht2{Complete Monitoring}
      @rt{  vs. }
      @ht2{Type Soundness})
    #:next
    #:go title-coord-mid
    (cm-example '(U T U)
                #:api-comment
                (vc-append
                  cm-callback-q
                  (hsep tiny-y-sep)
                  (make-2table
                    #:row-align lb-superimpose
                    #:col-sep tiny-x-sep
                    #:row-sep pico-y-sep
                    (list
                      (clip-descent @ht2-initials{TS}) @rt{=/> Yes}
                      (clip-descent @ht2-initials{CM}) @rt{=> Yes}))))
    #:set (let* ((pp ppict-do-state))
            (ppict-do
              pp
              #:go (at-cm-arrow #:abs-y (- small-y-sep))
              (hc-append
                (make-2table
                  #:row-align lt-superimpose
                  #:col-sep tiny-x-sep
                  #:row-sep tiny-y-sep
                  (list @ht2-initials{TS} @untyped-code{nothing}
                        @ht2-initials{CM} cm-num-str))
                (xsep small-x-sep)))))
  (pslide
    #:go sky-coord
    (sky-pict)
    #:go center-coord
    (item-line-append
      @rrt{Complete Monitoring, Type S., Tag S., Unsound}
      @rrt{useful partition Deep vs Shallow}))
  (pslide
    #:go sky-coord
    (sky-pict)
    #:go center-coord
    (item-line-append
      @rrt{further distinctions, new properties}
      @rrt{- do errors find all responsible? B. Completeness}
      @rrt{- do errors find only responsible? B. Soundness}
      @rrt{- which programs run without error? E. Preorder}))
  (pslide
    #:go center-coord
    (frame-person #f "jfp-table.png" 8/10))
  (void))

(define (sec:thesis)
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
    #:go (coord contribution-x-right contribution-y-top 'lt)
    (ht-append
      @ht2{1. }
      (text-line-append
        @rt{How to assess}
        @rt{ type guarantees}))
    #:go (coord contribution-x-right contribution-y-bot 'lt)
    (ht-append
      @ht2{2. }
      (text-line-append
        @rt{How to measure}
        @rt{ performance})))
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
    #:go sky-coord
    (sky-pict-transient)
    #:go earth-coord
    (earth-pict-transient)
    #:go low-text-left
    (item-line-append
      @rrt{Natural = strongest types}
      @rrt{Transient = no wrappers}
      @rrt{combine in TR})
    #:go (coord 56/100 sky-y 'ct)
    (let* ((ycolor yellow1-3k1)
           (ray
            (filled-rectangle
              tiny-x-sep
              (h%->pixels 1/2)
              #:draw-border? #f
              #:color ycolor))
           (star
             (make-compass-pict 18 #:color ycolor))
           (ray
            (hc-append pico-x-sep star ray star))
           (ray
             (ppict-do
               ray
               #:go (coord 1 1/10 'lc) @rrt{1. Model}
               #:go (coord 1 9/10 'lc) @rrt{2. Implementation})))
      ray))
  (sec:thesis:model)
  (sec:thesis:implementation)
  (sec:thesis:evaluation)
  (void))

(define (sec:thesis:model)
  (pslide
    #:go heading-coord-left
    (word-append
      @ht2{Model}
      @rt{   Deep + Shallow + Untyped})
    #:go text-coord-left
    (frame-person #f "model-0.png" 43/100)
    (hsep med-y-sep)
    (frame-person #f "model-1.png" 50/100)
    #:next
    #:go text-coord-right
    (hsep (* 2 med-y-sep))
    (frame-person #f "model-2.png" 60/100))
  (pslide
    #:go heading-coord-left
    (word-append
      @ht2{Model}
      @rt{   boundaries})
    #:go title-coord-mid
    (DSU-pict 0)
    #:next
    #:go icon-coord-mid
    @rrt{Deep types => wrapper}
    @rrt{Shallow types => check inputs})
  (pslide
    #:go heading-coord-left
    (word-append
      @ht2{Model}
      @rt{  Properties})
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
    #:go sky-coord (sky-pict-transient) #:go earth-coord (earth-pict-transient)
    #:go low-text-mid
    @rrt{model OK}
    (hsep (* 3 med-y-sep))
    @rrt{implementation})
  (pslide
    #:go heading-coord-left
    @ht2{Typed Racket Compiler}
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
    @ht2{Types to Shapes}
    #:go text-coord-mid
    ;; type-term color distinction?
    ;; add comment to each?
    ;; in general: check full constructor
    (word-append
      @rrt{(f x) } @sst{: T} @rrt{    --->    (check s (f x))})
    (hsep tiny-y-sep)
    (type-to-shape @sst{T} @rrt{s})
    (hsep small-y-sep)
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
    @ht2{Optimization}
    #:go title-coord-mid
    @rrt{(fl+ n m)}
    @rrt{(unsafe-fl+ n m)}
    (word-append
      @rrt{when  n } @sst{: Float} @rrt{  and  m } @sst{: Float}))
  (pslide
    #:go heading-coord-left
    @ht2{Optimization}
    #:go title-coord-mid
    (table 4
           (pad-modulo 4 (map opt->pict optimization-name*))
           cc-superimpose cc-superimpose pico-x-sep small-y-sep)
    #:next
    #:go (at-find-pict 'dead-code ct-find 'cc) fail-pict
    #:go (at-find-pict 'pair ct-find 'cc) fail-pict)
  ;; TODO opt examples? or save to end
  ;;  g (-> Str Str) case-lambda (x) x (x y) y
  ;;  ~~> case-lambda (x) x (x y) void
  ;;
  ;;  x (pairof (pairof nat int) str)
  ;;  cdar x -> unsafe-cdr unsafe-car x
  (pslide
    #:go sky-coord (sky-pict-transient) #:go earth-coord (earth-pict-transient)
    #:go low-text-mid
    @rrt{model OK}
    (hsep (* 3 med-y-sep))
    @rrt{implementation OK})
  (void))

(define (sec:thesis:evaluation)
  (pslide
    #:go heading-coord-left
    (word-append
      @ht2{Shallow to Deep}
      @rt{ = stronger guarantees})
    #:go title-coord-mid
    (cm-example '(U S U))
    #:go (at-find-pict cm-api-tag rt-find 'rc #:abs-x face-offset-right) unhappy-face
    #:next
    #:go (at-find-pict cm-api-tag rb-find 'rb #:abs-x (- pico-x-sep) #:abs-y (- tiny-y-sep))
    (let* ((pp (cm-api-pict 'D))
           (pp (ppict-do pp #:go (coord 1 0 'rc #:abs-x face-offset-right) happy-face)))
      pp)
    #:go (at-find-pict cm-arrow-tag lc-find 'rc)
    (hc-append
      cm-num-str
      (xsep small-x-sep))
    #:go title-coord-mid
    (takeaway-frame
      ;; TODO clearer takeaway?
      @rt{Deep satisfies complete monitoring}))
  (pslide
    #:go heading-coord-left
    (word-append
      @ht2{Deep to Shallow}
      @rt{ = fewer errors})
    #:go text-coord-mid
    (higher-order-any-example 'D 'U)
    (word-append error-pict
                 @rrt{ attempted to use higher-order})
    (word-append @rrt{value passed as } (deep-code "Any"))
    #:next
    #:alt [#:go center-coord (frame-person #f "racket-users-ho-any.png" 8/10)]
    (blank 0 small-y-sep)
    (higher-order-any-example 'S 'U)
    success-pict
    #:next
    #:go title-coord-mid
    (takeaway-frame
      @rt{Shallow can run almost all type-correct code}))
  (pslide
    #:go heading-coord-left
    @ht2{Better Performance}
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
    @ht2{Better Performance}
    ;; recall, large area under curve
    #:go text-coord-mid
    (big-overhead-plot 'jpeg '(D S))
    #:go icon-coord-mid
    @rt{Deep + Shallow = maximize D-deliverable cfgs.})
  (pslide
    #:go heading-coord-left
    @ht2{Better Performance}
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
    @ht2{Better Performance}
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
    @ht2{New Migration Plan}
    #:go text-coord-left
    (item-line-append
      (blank)
      (hb-append @st{1. } @rt{Deep, until slow})
      (hb-append @st{2. } @rt{Shallow, to fix boundaries})
      (hb-append @st{3. } @rt{Deep, or mix, at end}))
    #:go text-coord-right
    (migration-append
      (path-node '(D S D D D))
      (path-node '(D S S S S))
      (path-node '(D S S U U))
      (path-node '(D D D U U))
      (path-node '(U U U U U))))
  (pslide
    #:go heading-coord-left
    @ht2{New Migration Plan}
    #:go text-coord-mid
    #:alt [
     (text-c-append
       (word-append @rt{What % of paths are  } D-pict @rt{-deliverable})
       @rt{at each step?})
     (ysep med-y-sep)
     (scale (make-lattice 4 bits->path-node #:x-margin pico-x-sep #:y-margin lattice-y-sep) 7/10)
    ]
    @rt{% of 3-deliverable paths}
    (ysep tiny-y-sep)
    (fancy-table
      (list
        (list "Benchmark" "Deep or Shallow" "Deep and Shallow")
        (list "sieve" "0%" "100%")
        (list "jpeg" "100%" "100%")
        (list "fsmoo" "0%" "50%")
        (list "dungeon" "0%" "67%")
        (list "suffixt" "0%" "12%")
        (list "take5" "100%" "100%"))))
  (pslide
    #:go heading-coord-left
    @ht2{Better Together}
    #:go text-coord-mid
    @rt{How many configs do best with a mix?}
    (ysep tiny-y-sep)
    (fancy-table
      (list
        (list "Benchmark" "" "D+S ≥ D|S")
        (list "fsm" "" "37%")
        (list "morsecode" "" "25%")
        (list "jpeg" "" "37%")
        (list "kcfa" "" "55%")
        (list "zombie" ""  "6%")
        (list "zordoz" "" "46%"))))
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
    ;#:go icon-coord-mid
    ;(hc-append small-x-sep scale-pict ruler-pict @rt{?} @rt{?})
    )
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
  (let* ((u-result (word-append @rt{Untyped } (untyped-code " 0 ")))
         (d-result (word-append @rt{Deep } (deep-code " #f ")))
         (s-result (word-append @rt{Shallow } (shallow-code " 0 ")))
         (r* (list u-result d-result s-result))
         (make-icon (lambda (i)
                      (define pp*
                        (for/list ((r (in-list r*))
                                   (k (in-naturals)))
                          (if (<= k i) r (bcellophane2 r))))
                      (apply hc-append small-x-sep pp*))))
    (pslide
      #:go heading-coord-left
      (word-append
        @ht2{Deep to Shallow}
        @rt{ = simpler behavior})
      #:alt [#:go icon-coord-mid (make-icon 0)
             #:go text-coord-mid (index-of-example 'U) ]
      #:alt [#:go icon-coord-mid (make-icon 1)
             #:go text-coord-mid (index-of-example 'D) ]
      #:go icon-coord-mid (make-icon 2)
      #:go text-coord-mid (index-of-example 'S)
      #:next
      #:go center-coord
      (takeaway-frame
        @rt{No wrappers = fewer surprises})))
  (void))

(define (sec:thesis:transient)
  ;; TODO move to 'extra'
  (pslide
    #:go heading-coord-left
    (word-append
      @ht2{Closing the Gap:}
      @rt{  Transient without Dyn})
    #:go text-coord-mid
    @rrt{(f x) : Num morphs to}
    #:go low-text-left
    @rrt{Before}
    @rrt{(check Num (check Fun f) (check Num x))}
    @rrt{... because could be Dyn anywhere}
    (hsep med-y-sep)
    @rrt{After}
    @rrt{(check Num (f x))}
    @rrt{no Dyn, fewer checks})
  #;(pslide
    #:go heading-coord-left
    @ht2{Completion}
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
;    (pslide)
;    (sec:extra)

(pslide


)

    (void))
  (void))

;; =============================================================================


(module+ raco-pict (provide raco-pict)
  (define aspect 'fullscreen)
  (define-values [client-w client-h]
    (apply values (for/list ((f (in-list (list get-client-w get-client-h)))) (f #:aspect aspect))))
(define raco-pict
  (ppict-do (filled-rectangle client-w client-h #:draw-border? #f #:color background-color)

    #:go earth-coord (earth-pict #:mode 'rp-begin)
    #:go heading-coord-left (word-append @ht2{Reticulated Python} @rrt{   different benchmarks})
    #:go overhead-coord-top (src-bitmap "retic-overhead.png") #;(let ((pp (2col-overhead-plot* 'S '(spectralnorm pystone chaos go)))) (save-pict+ "retic-overhead.png" (ct-superimpose (blank (pict-width pp) (+ (pict-height pp) med-x-sep)) pp)))
    #:next
    #:go overhead-coord-mid (wide-takeaway-frame @ht2{Not so bad})


  )))
