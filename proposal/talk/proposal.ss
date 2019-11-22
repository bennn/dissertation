#lang at-exp slideshow

(define PREVIEW #f)

;; image-color.com
;; /Users/ben/code/racket/gtp/shallow/gf-icfp-2018/talk/simple.ss
;; /Users/ben/code/racket/gtp/rrc/oopsla-2019/talk/splash.ss

;; TODO ...
;; - bridge pict
;; - RUN honest types and lying types to build slides
;;    (its in parallel of course, but should run --- use to make picture)

;; (DONT tho) Meeting of the Waters =
;; - Manaus
;; - dark Rio Negro
;; - sandy Amazon River
;; - for 6km = 4mi, run together
;; - b/c temperature, speed, water density

;; From MF: "... want real expressiveness as opposed to type system features
;;  that we throw in and maybe someone needs ..."

(require
  file/glob
  pict pict/convert pict/balloon pict/face
  (prefix-in pict: pict/shadow)
  pict-abbrevs pict-abbrevs/slideshow gtp-pict
  ppict/2
  "lightbulb.rkt"
  racket/draw
  racket/list
  racket/string
  racket/format
  scribble-abbrevs/pict
  slideshow/code
  plot/no-gui (except-in plot/utils min* max*)
  (only-in scribble-abbrevs/scribble authors*)
  (only-in images/icons/symbol check-icon x-icon)
  (only-in images/icons/misc clock-icon)
  (only-in 2htdp/image star)
  (only-in 2htdp/planetcute gem-blue)
  images/icons/style
)

(define slide-top 4/100)
(define slide-left 2/100)
(define slide-right (- 1 slide-left))
(define slide-bottom 86/100)
(define slide-text-left (* 3 slide-left))
(define slide-text-right (- 1 slide-text-left))
(define slide-text-top (* 4 slide-top))
(define slide-text-bottom slide-bottom)
(define slide-text-coord (coord slide-text-left slide-text-top 'lt))
(define slide-text-coord-right (coord slide-text-right slide-text-top 'rt))
(define center-coord (coord 1/2 1/2 'cc))
(define heading-text-coord (coord slide-left slide-top 'lt))
(define big-landscape-coord (coord 1/2 slide-text-top 'ct))

(define turn revolution)

(define pico-x-sep (w%->pixels 1/100))
(define tiny-x-sep (w%->pixels 2/100))
(define small-x-sep (w%->pixels 5/100))
(define med-x-sep (w%->pixels 10/100))

(define pico-y-sep (h%->pixels 1/100))
(define tiny-y-sep (h%->pixels 2/100))
(define small-y-sep (h%->pixels 5/100))
(define med-y-sep (h%->pixels 10/100))

(define PLOT-FN-ALPHA 0.6)

;; COLOR
(define black (string->color% "black"))
(define gray (string->color% "light gray"))
(define white (string->color% "white"))
(define goal-color (string->color% "lime green"))
(define transparent (color%-update-alpha white 0))
(define program-color (hex-triplet->color% #xABC9CF)) ;; gray/blue
(define racket-red  (hex-triplet->color% #x9F1D20))
(define racket-blue (hex-triplet->color% #x3E5BA9))
(define ice-color (hex-triplet->color% #xF3F1F2))
(define sand-color (hex-triplet->color% #xFFF7C2))
(define stamp-color (hex-triplet->color% #xDCCC90))
(define cliff-color (hex-triplet->color% #x3A3B27))
(define sea-color (hex-triplet->color% #x84CEB3))
(define storm-color (hex-triplet->color% #xBBBBBB) #;(string->color% "Dark Gray"))
(define water-color (hex-triplet->color% #x7CCCB1))
(define LIGHT-RED (string->color% "Tomato"))
(define GREEN (string->color% "mediumseagreen"))
(define BLUE (string->color% "cornflowerblue"))
(define timeline-span-color (color%-update-alpha (string->color% "cadetblue") 0.8))
(define bg-accent-color (color%-update-alpha timeline-span-color 5/10))
(define topbar-accent-color (color%-update-alpha (string->color% "medium slate blue") 5/10))
(define pipe-color (hex-triplet->color% #xA6AF60))
(define city-color (hex-triplet->color% #x302E22))

(define typed-color   (hex-triplet->color% #xF19C4D)) ;; orange
;; #xE59650 #xEF9036
(define untyped-color (hex-triplet->color% #x697F4D)) ;; dark green
;; #x72875C #x708E6D

(define uni-sound-color (hex-triplet->color% #x666666))
(define tag-sound-color (hex-triplet->color% #x888888))
(define type-sound-color (hex-triplet->color% #xBBBBBB))
(define complete-monitoring-color (hex-triplet->color% #xFFFFFF))
(define honest-color LIGHT-RED)
(define lying-color BLUE)
(define vacuous-color (hex-triplet->color% #x777777))

(define no-box-color (hex-triplet->color% #xCCCCCC))
(define yes-box-color white #;(hex-triplet->color% #x69f547) #;"lime green")

(define (tagof str) (string-append "⌊" str "⌋"))
(define tag-T (tagof "T"))

(define uni-sound-str "Dyn Soundness")
(define tag-sound-str "Tag Soundness")
(define type-sound-str "Type Soundness")
(define complete-monitoring-str "Complete Monitoring")

(define uni-sound-tag 'uni-sound)
(define tag-sound-tag 'tag-sound)
(define type-sound-tag 'type-sound)
(define complete-monitoring-tag 'complete-mon)

(define (clip* pp)
  (clip-ascent (clip-descent pp)))

(define (make-region-label txt-pict)
  (define w (* 110/100 (pict-width txt-pict)))
  (define h (* 120/100 (pict-height (t tag-T))))
  (define bg (make-landscape-background w h #:color stamp-color))
  (ppict-do bg #:go (coord 1/2 48/100 'cc) txt-pict))

(define (add-landscape-background pp #:x-margin [pre-x-margin #f] #:y-margin [pre-y-margin #f])
  (define x-margin (or pre-x-margin pico-x-sep))
  (define y-margin (or pre-y-margin pico-y-sep))
  (define w (+ (* 2 x-margin) (pict-width pp)))
  (define h (+ (* 2 y-margin) (pict-height pp)))
  (cc-superimpose
    (make-landscape-background w h #:color stamp-color)
    pp))

(define region-border-width 5)
(define region-border-color black)

(define title-font
  #;"ArtNoveauDecadente" #;(fantasy)
  #;"Treasure Map Deadhand" #;(+10 pt ... no too small, only for decorations not for reading)
  #;"Kentucky Fireplace" #;(cute ... very handwriting)
  #;"Copperplate"
  #;"Primitive" #;(damn capitals ... maybe good for T / U)
  #;"Weibei SC" #;ok
  "FHA Modernized Ideal ClassicNC"
  #;"FHA Condensed French NC"
  #;"Plantagenet Cherokee" #;ok
  #;"Lao MN")

(define body-font "Tsukushi A Round Gothic")

(define subtitle-font body-font)

(define subsubtitle-font
  #;"Apple Chancery"
  "Libian SC"
  #;"Tsukushi A Round Gothic"
  #;"Plantagenet Cherokee"
  #;"Roboto Condensed"
  #;"Treasure Map Deadhand")

(define code-font "Inconsolata")
(define lang-font code-font)
(define tu-font title-font)
(define decoration-font title-font)

(define title-size 60)
(define subtitle-size 44) ;;?
(define subsubtitle-size 48)
(define body-size 40)
(define caption-size 40)
(define code-size 28)
(define code-line-sep 4)
(define big-body-size 52)
(define tu-size 72)
(define big-node-size 120)

(define ((make-string->text #:font font #:size size #:color color) str)
  (colorize (text str font size) color))

(define (make-string->title #:size [size title-size] #:color [color black])
  (make-string->body #:size size #:color color))

(define (make-string->subtitle #:size [size subtitle-size] #:color [color black])
  (make-string->text #:font subtitle-font #:size size #:color color))

(define (make-string->subsubtitle #:size [size subsubtitle-size] #:color [color black])
  (make-string->text #:font subsubtitle-font #:size size #:color color))

(define (make-string->body #:size [size body-size] #:color [color black])
  (make-string->text #:font body-font #:size size #:color color))

(define (make-string->code #:size [size code-size] #:color [color black])
  (make-string->text #:font code-font #:size size #:color color))

(define titlet (make-string->title))
(define small-titlet (make-string->title #:size 42))
(define st (make-string->subtitle))
(define st-blue (make-string->subtitle #:color racket-blue))
(define sbt (make-string->text #:font (cons 'bold subtitle-font) #:size subtitle-size #:color black))
(define sst (make-string->subtitle))
(define sst2 (make-string->subsubtitle #:size (- subsubtitle-size 10)))
(define t (make-string->body))
(define it (make-string->text #:font (cons 'italic body-font) #:size body-size #:color black))
(define bt (make-string->text #:font (cons 'bold body-font) #:size body-size #:color black))
(define btcodesize (make-string->text #:font (cons 'bold body-font) #:size code-size #:color black))
(define itcodesize (make-string->text #:font (cons 'italic body-font) #:size code-size #:color black))
(define ct (make-string->code))
(define bigct (make-string->code #:size (- body-size 4)))
(define cbt (make-string->text #:font (cons 'bold code-font) #:size code-size #:color black))
(define ckwdt cbt)
(define ctypet ct)
(define tcodesize (make-string->body #:size code-size))
(define tsmaller (make-string->body #:size (- body-size 4)))
(define lang-text (make-string->code #:size 32))
(define blang-text (make-string->text #:font (cons 'bold code-font) #:size 32 #:color black))
(define captiont (make-string->body #:size caption-size #:color black))
(define (ownership-text str #:color [color black]) ((make-string->text #:font "PilGi" #:size 40 #:color color) str))
(define huge-t (make-string->text #:font tu-font #:size 80 #:color black))
(define big-t (make-string->body #:size big-body-size))
(define big-tb (make-string->text #:font (cons 'bold body-font) #:size big-body-size #:color black))

(define unknown-sound-str "???")

(define Natural-str "Natural")
(define Transient-str "Transient")
(define Amnesic-str "Amnesic")
(define Natural-pict (t Natural-str))
(define Transient-pict (t Transient-str))
(define Amnesic-pict (t Amnesic-str))

(define N-str "N")
(define T-str "T")
(define A-str "A")
(define N-pict (t N-str))
(define T-pict (t T-str))
(define A-pict (t A-str))


;;(define t-v-sep (* 3/4 (pict-height @t{ })))
;;(define t-h-sep (pict-width @t{ }))
;;(define t-v-sep-pict (blank 0 t-v-sep))
;;(define t-h-sep-pict (blank t-h-sep 0))
;;
;;(define erasure-tag 'erasure)
;;(define erasure-missing-tag 'erasure-blanklabel)
;;(define natural-tag 'natural)
;;(define transient-tag 'transient)
;;(define amnesic-tag 'amnesic)
;;(define ts-tag 'TS)
;;
;;(define table-row-tag 'table-row)
;;
;;(define (blur pp h-rad [v-rad #f])
;;  (define old-tag (pict-tag pp))
;;  (define pp/b (pict:blur pp h-rad (or h-rad v-rad)))
;;  (if old-tag (tag-pict pp/b old-tag) pp/b))

;;(define (add-landscape-line pp arrow)
;;  (add-program-arrow pp arrow #:hide? #true #:style 'solid #:line-width 4))

(struct program-arrow [src-tag src-find tgt-tag tgt-find start-angle end-angle start-pull end-pull color] #:transparent)

(define (add-program-arrow pp arrow #:arrow-size [arrow-size 12] #:line-width [pre-line-width #f] #:style [style 'short-dash] #:label [label (blank)] #:hide? [hide? #false])
  (define line-width (or pre-line-width 3))
  (pin-arrow-line
    arrow-size pp
    (find-tag pp (program-arrow-src-tag arrow))
    (program-arrow-src-find arrow)
    (find-tag pp (program-arrow-tgt-tag arrow))
    (program-arrow-tgt-find arrow)
    #:line-width line-width
    #:label label
    #:hide-arrowhead? hide?
    #:style style
    #:start-angle (program-arrow-start-angle arrow)
    #:end-angle (program-arrow-end-angle arrow)
    #:start-pull (program-arrow-start-pull arrow)
    #:end-pull (program-arrow-end-pull arrow)
    #:color (program-arrow-color arrow)))

(define (add-shadow-background pp #:x-margin [pre-x-margin #f] #:y-margin [pre-y-margin #f] #:color [shadow-color black] #:alpha [shadow-alpha 0.1])
  (define w (pict-width pp))
  (define h (pict-height pp))
  (define default-margin (* 5/100 (min w h)))
  (define x-margin (or pre-x-margin default-margin))
  (define y-margin (or pre-x-margin default-margin))
  (define shadow-pict (cellophane (filled-rectangle w h #:draw-border? #f #:color shadow-color) shadow-alpha))
  (lt-superimpose (ht-append (blank x-margin 0) (vl-append (blank 0 y-margin) shadow-pict)) pp))

(define (pict->square pp)
  (define w (pict-width pp))
  (define h (pict-height pp))
  (cc-superimpose (blank (max w h)) pp))

(define (hide pp)
  (blank (pict-width pp) (pict-height pp)))

(define (tag-append . x*)
  (string->symbol (string-join (map ~a x*) "-")))

(define caption-margin small-y-sep)

(define (add-caption str pp)
  (vc-append caption-margin pp (captiont str)))

(define big-program-w (w%->pixels 45/100))
(define big-program-h (h%->pixels 55/100))

(define big-program-x 4/100)
(define big-program-y 15/100)
(define big-program-coord (coord 1/2 big-program-y 'ct))
(define big-program-coord-left (coord big-program-x big-program-y 'lt))
(define big-program-coord-right (coord (- 1 big-program-x) big-program-y 'rt))

(define untyped-program-code* '(#f #f #f #f #f))
(define mixed-program-code* '(#f #t #f #f #t))

(define program-w (w%->pixels 30/100))
(define program-h (h%->pixels 40/100))

(define big-landscape-w (* 7/10 client-w))
(define big-landscape-h (* 55/100 client-h))

(define (make-landscape-background w h #:color [pre-color #f])
  (define c (or pre-color sand-color))
  (define bc cliff-color)
  (define (draw-box dc dx dy)
    (define old-brush (send dc get-brush))
    (define old-pen (send dc get-pen))
    ;; --- white background
    (let ((path (new dc-path%)))
      (send dc set-brush (new brush% [color white] [style 'solid]))
      (send dc set-pen (new pen% [width 1] [color white]))
      (send path rectangle 0 0 w h)
      (send dc draw-path path dx dy))
    ;; --- foreground
    (send dc set-brush (new brush% [color c] [style 'crossdiag-hatch]))
    (for ((pw (in-list '(3 2)))
          (offset (in-list '(0 4))))
      (send dc set-pen (new pen% [width pw] [color bc]))
      (define path (new dc-path%))
      (send path rectangle (+ 0 offset) (+ 0 offset) (- w (* 2 offset)) (- h (* 2 offset)))
      (send dc draw-path path dx dy))
    (send dc set-brush old-brush)
    (send dc set-pen old-pen))
  (dc draw-box w h))
;;  (filled-rectangle w h #:color sand-color #:draw-border? #f)

(define (make-big-landscape-background)
  (make-landscape-background big-landscape-w big-landscape-h))

(define hyrule-landscape
  (cc-superimpose (make-big-landscape-background) (if PREVIEW @st{???} (inset/clip (bitmap "src/hyrule.png") -1 -6))))

(define (make-performance-landscape)
  hyrule-landscape)

(define (make-uni-sound-region w h)
  (filled-rectangle w h #:draw-border? #f #:color uni-sound-color))

(define (make-tag-sound-region pre-w pre-h)
  (define w (* 89/100 pre-w))
  (define h (* 76/100 pre-h))
  (draw-boring-curve w h tag-sound-color))

(define (make-type-sound-region pre-w pre-h)
  (define w (* 78/100 pre-w))
  (define h (* 51/100 pre-h))
  (draw-boring-curve w h type-sound-color))

(define (make-complete-monitoring-region pre-w pre-h)
  (define w (* 67/100 pre-w))
  (define h (* 26/100 pre-h))
  (draw-boring-curve w h complete-monitoring-color))

(define (draw-boring-curve w h color)
  (define bc region-border-color)
  (define c color)
  (define (draw-region dc dx dy)
    (define old-brush (send dc get-brush))
    (define old-pen (send dc get-pen))
    (send dc set-pen (new pen% [width 0] [color c]))
    (for ((brush (in-list
                   (list
                     (new brush% [style 'solid] [color white])
                     (new brush% [style 'solid] [color c])))))
      (send dc set-brush brush)
      (define path (new dc-path%))
      (send path move-to w 0)
      (send path line-to 0 0)
      (send path line-to 0 h)
      (send path line-to (* 9/10 w) h)
      (send path curve-to (* 95/100 w) h w h w (* 7/10 h))
      (send path line-to w 0)
      (send path close)
      (send dc draw-path path dx dy))
    (send dc set-pen (new pen% [width region-border-width] [color bc]))
    (send dc set-brush (new brush% [style 'solid] [color c]))
    (define path (new dc-path%))
    (send path move-to 1 h)
    (send path line-to (* 9/10 w) h)
    (send path curve-to (* 95/100 w) h w h w (* 7/10 h))
    (send path line-to w 2)
    (send dc draw-path path dx dy)
    ;; --
    (send dc set-brush old-brush)
    (send dc set-pen old-pen))
  (dc draw-region w h))

(define (make-implementation-landscape)
  (define the-flag-base (blank 18 4))
  (define num-colors 7)
  (for/fold ((acc (make-big-landscape-background)))
            ((xy (in-list '((89/100 07/100) (73/100 11/100)
                            (83/100 18/100 "clojure.png") (94/100 26/100) (62/100 12/100)
                            (52/100 22/100 "python.png") (42/100 13/100) (32/100 20/100)
                            (22/100 11/100) (15/100 30/100 "javascript.png") (09/100 12/100)
                            (80/100 31/100) (65/100 38/100 "typescript.png") (43/100 38/100)
                            (75/100 66/100 "racket.png")
                            (27/100 40/100) (08/100 46/100) (87/100 51/100)
                            (71/100 60/100) (49/100 52/100) (24/100 56/100)
                            (10/100 62/100 "lua.png") (92/100 60/100 "php.png") (75/100 75/100)
                            (46/100 71/100) (17/100 77/100) (30/100 73/100)
                            (87/100 76/100) (65/100 91/100 "hack.png") (49/100 83/100 "pyre.png")
                            (36/100 53/100 "dart.png")
                            (35/100 85/100) (93/100 89/100 "flow.png")
                            (59/100 67/100) (11/100 92/100 "thorn.png"))))
             (i (in-naturals)))
    (ppict-do acc
      #:go (coord (car xy) (cadr xy) 'cc)
      (if (null? (cddr xy))
        (blank) #;(make-simple-flag the-flag-base
          #:flag-background-color (rgb-triplet->color% (->pen-color (modulo i num-colors)))
          #:flag-brush-style 'horizontal-hatch
          #:flag-border-color (rgb-triplet->color% (->pen-color (modulo i num-colors)))
          #:pole-height 70)
        (scale-to-fit (bitmap (build-path "src" "lang" (caddr xy))) 90 90)))))

(define (make-theorem-landscape)
  (let* ((pp (make-big-landscape-background))
         (w (- (pict-width pp) 12))
         (h (- (pict-height pp) 12))
         (top-left-coord (coord 0 0 #:abs-x +6.5 #:abs-y 5.5 'lt))
         (pp (ppict-do pp
               #:go top-left-coord (make-uni-sound-region w h)
               #:go top-left-coord (make-tag-sound-region w h)
               #:go top-left-coord (make-type-sound-region w h)
               #:go top-left-coord (make-complete-monitoring-region w h)))
         (label-x 2/100)
         (pp (ppict-do pp
               #:go (coord label-x 80/100 'lt)
               (make-region-label (t uni-sound-str))
               #:go (coord label-x 55/100 'lt)
               (make-region-label (t tag-sound-str))
               #:go (coord label-x 31/100 'lt)
               (make-region-label (t type-sound-str))
               #:go (coord label-x 06/100 'lt)
               (make-region-label (t complete-monitoring-str))))
         )
    pp))

(define small-PLT-pict
  (vl-append 2 (bitmap "src/racket-small.png") (blank)))

(define med-PLT-pict
  (bitmap "src/racket-med.png"))

(define neu-pict
  (bitmap "src/neu-small.png"))

(define meeting-of-the-waters
  (if PREVIEW (blank) (bitmap "src/meeting-of-the-waters.jpg")))

(define the-Q (hb-append 0 (huge-t "Q") (big-t ". ")))

(define the-mt @big-t{migratory typing})
(define the-h @big-t{honest})
(define the-l @big-t{lying})

(define the-mt-b @big-tb{migratory typing})
(define the-h-b @big-tb{honest})
(define the-l-b @big-tb{lying})

(define (line-append . pp*)
  (line-append* pp*))

(define (line-append* pp*)
  (apply hb-append 0 pp*))

(define (make-thesis-question bold?)
  (define txt-pict
    (vl-append
      tiny-y-sep
      (line-append (tag-pict @big-t{Does  } 'qstart) (if bold? the-mt-b the-mt) @big-t{  benefit})
      (line-append @big-t{from a combination of  } (if bold? the-h-b the-h))
      (line-append @big-t{and  } (if bold? the-l-b the-l) @big-t{  types?})))
  (ht-append the-Q txt-pict))

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

(define (make-program-pict pp #:radius [pre-radius #f] #:bg-color [bg-color #f] #:frame-color [frame-color #f] #:x-margin [pre-x #f] #:y-margin [pre-y #f])
  (define radius (or pre-radius 12))
  (define x-m (or pre-x (w%->pixels 5/100)))
  (define y-m (or pre-y med-y-sep))
  (add-rectangle-background
    #:radius radius #:color (if (eq? bg-color transparent) transparent white)
    (add-rounded-border
      pp
      #:radius radius
      #:background-color (or bg-color (color%-update-alpha program-color 0.85))
      #:frame-width 3
      #:frame-color (or frame-color program-color)
      #:x-margin x-m
      #:y-margin y-m)))

(define (make-module-pict pp #:radius radius #:bg-color [bg-color #false] #:x-margin [pre-x #f] #:y-margin [pre-y #f])
  (add-rounded-border
    pp
    #:radius radius
    #:background-color bg-color
    #:frame-width 2
    #:x-margin (or pre-x pico-x-sep)
    #:y-margin (or pre-y pico-y-sep)))

(define (make-typed-pict pp #:x-margin [x #f] #:y-margin [y #f])
  (make-module-pict pp #:radius 37 #:bg-color typed-color #:x-margin x #:y-margin y))

(define (make-untyped-pict pp #:x-margin [x #f] #:y-margin [y #f])
  (make-module-pict pp #:radius 4 #:bg-color untyped-color #:x-margin x #:y-margin y))

(define (make-typed-icon #:font-size [font-size #f] #:width [w #f] #:height [h #f])
  (make-tu-icon "T" #:font-size font-size #:width w #:height h))

(define (make-untyped-icon  #:font-size [font-size #f] #:width [w #f] #:height [h #f])
  (make-tu-icon "U" #:font-size font-size #:width w #:height h))

(define (make-lang-pict str)
  (define pp (make-tu-icon str))
  (make-module-pict pp #:radius 60 #:bg-color "peachpuff" #:x-margin small-x-sep #:y-margin tiny-x-sep))

(define (make-tu-icon str #:font-size [pre-font-size #f] #:width [pre-w #f] #:height [pre-h #f])
  (define font-size (or pre-font-size tu-size))
  (define w (or pre-w 70))
  (define h (or pre-h 70))
  (define str-pict (clip-descent (text str `(bold . ,tu-font) font-size)))
  (cc-superimpose (blank w h) str-pict))

(define U-node (make-untyped-pict (make-untyped-icon)))
(define T-node (make-typed-pict (make-typed-icon)))
(define Lib-node (make-typed-pict (make-tu-icon "Lib") #:x-margin 40 #:y-margin 10))

(define (make-tree w h tu-code* #:arrows? [draw-arrows? #false] #:owners? [draw-owners? #false] #:labels? [labels? #true])
  (define node-sym* '(A B C D E))
  (define node*
    (for/fold ((acc (blank w h)))
              ((pos (in-list (list (coord 0 0 'lt)
                                   (coord 52/100 25/100 'ct)
                                   (coord 1 1/100 'rt)
                                   (coord 12/100 1 'lb)
                                   (coord 95/100 95/100 'rb))))
               (bool (in-list tu-code*))
               (tag (in-list node-sym*))
               (i (in-naturals)))
      (define node (add-hubs (if bool T-node U-node) tag))
      (ppict-do acc #:go pos (if draw-owners? (add-ownership node i #:draw-label? labels?) node))))
  (define node*/arrows
    (cond
      [draw-arrows?
       (define arr*
         (list
           (program-arrow 'D-E rt-find 'E-W lt-find (* 1/10 turn) (* 9/10 turn) 60/100 1/4 black)
           (program-arrow 'E-N rt-find 'C-S rb-find (* 18/100 turn) (* 29/100 turn) 1/4 1/4 black)
           ;; --
           (program-arrow 'C-W lt-find 'A-E lt-find (* 36/100 turn) (* 64/100 turn) 1/4 1/4 black)
           (program-arrow 'A-E rb-find 'B-W lt-find 0 0 1/2 1/2 black)
           (program-arrow 'B-W lb-find 'D-N rt-find (* 1/2 turn) (* 3/4 turn) 1/4 1/4 black)
           (program-arrow 'B-S rb-find 'E-N lt-find (* 3/4 turn) (* 3/4 turn) 1/8 1/8 black)
           (program-arrow 'C-W lb-find 'B-N rt-find (* 1/2 turn) (* 3/4 turn) 1/2 1/3 black)
           (program-arrow 'D-N lt-find 'A-S lb-find (* 1/4 turn) (* 1/4 turn) 1/4 1/4 black)
           (program-arrow 'E-W lb-find 'D-E rb-find (* 60/100 turn) (* 45/100 turn) 1/4 1/4 black)
           (program-arrow 'E-S lb-find 'B-S lb-find (* 70/100 turn) (* 20/100 turn) 5/10 1/4 black)))
       (cond
         [(eq? draw-arrows? 'path)
          (define path-idx 2)
          (define short-arrow* (take arr* path-idx))
          (define val-tag 'end-val)
          (define node/path
            (for/fold ((acc node*))
                      ((a (in-list short-arrow*)))
              (add-program-arrow acc a)))
          (define node/path+
            (add-program-arrow node/path (list-ref arr* path-idx) #:style 'transparent #:hide? #true #:label (tag-pict (blank) val-tag)))
          (ppict-do
            node/path+
            #:go (at-find-pict val-tag cc-find 'cc #:abs-y (* -3/2 (pict-height ownership-v-pict)))
            (add-hubs (add-ownership* ownership-v-pict (decode-lbl* short-arrow*) #:draw-label? #f) 'V)
            #:set (add-program-arrow ppict-do-state (program-arrow 'C-W lt-find 'V-E rb-find (* 36/100 turn) (* 44/100 turn) 1/4 1/4 black)))]
         [else
          (for/fold ((acc node*))
                    ((a (in-list arr*)))
            (add-program-arrow acc a))])]
      [else
       node*]))
  node*/arrows)

(define (decode-lbl* pa*)
  ;; HACK get integers from the tags in pa*
  (remove-duplicates
    (apply append (for/list ((a (in-list pa*))) (list (decode-lbl (program-arrow-src-tag a)) (decode-lbl (program-arrow-tgt-tag a)))))))

(define (decode-lbl sym)
  ;; HACK
  (define str (symbol->string sym))
  (unless (< 0 (string-length str))
    (raise-argument-error 'decode-lbl "symbol? with +0 characters"))
  (- (char->integer (string-ref str 0)) (char->integer #\A)))

(define ownership-v-pict
  (let ((v-pict (bigct "v")))
    (cc-superimpose (blank (* 2 (pict-width v-pict)) 0) v-pict)))

(define (natural->owner-color i)
  (color%-update-alpha (rgb-triplet->color% (->pen-color i)) 0.3))

(define ell-pict
  (ownership-text "l"))

(define (natural->owner-label i)
  (define i-pict (ownership-text (number->string i)))
  (ht-append ell-pict (vl-append (* 36/100 (pict-height i-pict)) (blank) i-pict)))

(define (add-ownership pp i #:draw-label? [draw-label? #true] #:x-margin [pre-x-margin #f] #:y-margin [pre-y-margin #f])
  (define color (natural->owner-color i))
  (define x-margin (or pre-x-margin (* 1/2 (pict-width pp))))
  (define y-margin (or pre-y-margin (pict-height pp)))
  (define base-pict
    (add-spotlight-background
      #:border-color color #:x-margin x-margin #:y-margin y-margin
      pp))
  (if draw-label?
    (ppict-do
      base-pict
      #:go (coord 1 0 'rc)
      (natural->owner-label i))
    base-pict))

(define (add-ownership* pp oid* #:draw-label? [draw-label? #true])
  (define x% 30/100)
  (define y% 30/100)
  (define last-i (- (length oid*) 1))
  (unless (< 0 last-i)
    (raise-argument-error 'add-ownership* "list of 2 or more integers" 1 pp oid* '#:draw-label? draw-label?))
  (for/fold ((acc (add-ownership pp (car oid*) #:draw-label? #f)))
            ((lbl (in-list (cdr oid*)))
             (i (in-naturals)))
    (add-ownership acc lbl
                   #:draw-label? (and draw-label? (= i last-i))
                   #:x-margin (* (- x% (* 4/100 i)) (pict-width acc))
                   #:y-margin (* (- y% (* 4/100 i)) (pict-height acc)))))

(define (make-migration-arrow)
  (colorize (arrowhead (w%->pixels 5/100) 0) (hex-triplet->color% #x333333)))

(define (make-illustration-text . pp*)
  (make-illustration-text* pp*))

(define (make-illustration-text* pp*)
  (apply vc-append tiny-y-sep pp*))

(define (placer->signs sym)
  (case sym
    ((lt) (values -1 -1))
    ((rt) (values 1 -1))
    ((lb) (values -1 1))
    ((rb) (values 1 1))
    (else (error 'placer->signs))))

(define big-node-blank (blank 160))

(define (make-big-typed-node pp)
  (make-typed-pict (cc-superimpose big-node-blank pp)))

(define (make-big-untyped-node pp)
  (make-untyped-pict (cc-superimpose big-node-blank pp)))

(define (make-big-tu-pict tp up)
  (combine-big-nodes (make-big-untyped-node up) 'lt (make-big-typed-node tp) 'rb))

(define (make-big-ut-pict up tp)
  (combine-big-nodes (make-big-typed-node tp) 'lt (make-big-untyped-node up) 'rb))

(define (combine-big-nodes bottom-pict bottom-placer top-pict top-placer)
  (define w (+ (pict-width bottom-pict) (pict-width top-pict)))
  (define h (+ (pict-height bottom-pict) (pict-height top-pict)))
  (define x-offset (* 1/10 w))
  (define y-offset (* 1/10 h))
  (define-values [x-b-sign y-b-sign] (placer->signs bottom-placer))
  (define-values [x-t-sign y-t-sign] (placer->signs top-placer))
  (ppict-do (blank (- w (* 2 x-offset)) (- h (* 2 y-offset)))
    #:go (coord 1/2 1/2 bottom-placer #:abs-x (* x-b-sign x-offset) #:abs-y (* y-b-sign y-offset)) bottom-pict
    #:go (coord 1/2 1/2 top-placer #:abs-x (* x-t-sign x-offset) #:abs-y (* y-t-sign y-offset)) top-pict))

(define scripting-pict
  (make-big-ut-pict
    (lightbulb
      #:border-width 1
      #:bulb-radius 45
      #:stem-width-radians (* 1/10 turn)
      #:stem-height 12)
    (bitmap "src/parthenon-logo.png")))

(define ocaml-tag 'ocaml)

(define ocaml-pict (bitmap "src/ocaml-logo-small.png"))
(define opengl-pict (bitmap "src/opengl-logo-small.png"))
(define reasonml-pict (bitmap "src/reasonml-logo-small.png"))
(define react-pict (bitmap "src/react-logo-small.png"))

(define reuse-pict
  (make-big-tu-pict
    reasonml-pict
    react-pict))

(define (make-sample-program untyped? arrow? [scale-by 8/10])
  (make-program-pict
    #:bg-color white #:frame-color black
    (scale
      (make-tree program-w program-h (if untyped? untyped-program-code* mixed-program-code*) #:arrows? arrow?)
      scale-by)))

(define (add-cloud-background pp #:color [pre-color #f] #:x-margin [pre-x-margin #f] #:y-margin [pre-y-margin #f] #:style [pre-style #f])
  (define x-margin (or pre-x-margin pico-x-sep))
  (define y-margin (or pre-y-margin pico-y-sep))
  (cc-superimpose
    (cloud (+ x-margin (pict-width pp)) (+ y-margin (pict-height pp)) (or pre-color "gray") #:style (or pre-style '(square wide)))
    pp))

(define (st-cloud str #:style [pre-style #f])
  (add-cloud-background
    #:style pre-style #:x-margin (w%->pixels 10/100) #:y-margin (h%->pixels 18/100) #:color storm-color (sbt str)))

(define (make-uni-sound-bubble)
  (make-property-bubble uni-sound-str uni-sound-color))

(define (make-tag-sound-bubble)
  (make-property-bubble tag-sound-str tag-sound-color))

(define (make-type-sound-bubble)
  (make-property-bubble type-sound-str type-sound-color))

(define (make-complete-monitoring-bubble)
  (make-property-bubble complete-monitoring-str complete-monitoring-color))

(define widest-region-label
  (make-region-label
    (t
      (for/fold ((acc #f))
                ((str (in-list (list uni-sound-str tag-sound-str type-sound-str complete-monitoring-str))))
        (if (or (not acc) (< (string-length acc) (string-length str))) str acc)))))

(define (make-property-bubble lbl-str color)
  (define lbl-pict (make-region-label (t lbl-str)))
  (add-rounded-border
    #:radius 4 #:x-margin tiny-x-sep #:y-margin (h%->pixels 8/100)
    #:background-color color
    #:frame-color black #:frame-width region-border-width
    (lc-superimpose (ghost widest-region-label) lbl-pict)))

(define (descr-append . pp*)
  (descr-append* pp*))

(define (descr-append* pp*)
  (apply vl-append tiny-y-sep pp*))

(define judgment-x-sep small-x-sep)
(define judgment-y-sep small-y-sep)

(define (judgment-bar color)
  (filled-rectangle client-w 5 #:color color #:draw-border? #t #:border-color black #:border-width 1))

(define (at-judgment-bar tag)
  (at-find-pict tag lb-find 'lt #:abs-x (- tiny-x-sep) #:abs-y 1))

(define (make-overhead-plot e* [w 500] #:legend? [legend? #t])
  (define x-min 0)
  (define x-max (+ (/ pi 10) pi))
  (define pp
    (parameterize ((plot-x-ticks no-ticks)
                   (plot-y-ticks no-ticks)
                   (plot-font-size (current-font-size)))
      (plot-pict
        (for/list ((e (in-list e*)))
          (define c (symbol->color e))
          (function (make-embedding-function e x-min x-max)
                    #:width (* 15 (line-width))
                    #:alpha (if (eq? e 'H1) 1 PLOT-FN-ALPHA)
                    #:color c))
        #:y-label #f ;;"Overhead vs. Untyped"
        #:x-label #f ;;"Num. Type Ann."
        #:width w
        #:height (* 3/4 w)
        #:x-min x-min
        #:x-max x-max
        #:y-min 0
        #:y-max (* 10 (+ 1 (order-of-magnitude x-max))))))
  (add-overhead-axis-labels (tag-pict pp 'the-plot) legend?))

(define (symbol->color e)
  (case e
    ((H)
     LIGHT-RED)
    ((E)
     GREEN)
    ((1)
     BLUE)
    ((H1)
     goal-color)
    (else
      (raise-argument-error 'symbol->color "(or/c 'H 'E '1)" e))))

(define (make-embedding-function e x-min x-max)
  (define pi/4 (/ 3.14 4))
  (define 3pi/4 (* 3.5 pi/4))
  (define h-min 0.4)
  (case e
    ((H)
     (lambda (n)
       (cond
         [(< n pi/4)
          (max 1 (+ 0.9 (sin n)))]
         [(< n 3pi/4)
          10]
         [else
          h-min])))
    ((E)
     (lambda (n) 1))
    ((1)
     (lambda (n)
       (cond
         [(< n 3pi/4)
          (add1 n)]
         [else
           (- (+ 1 3pi/4) (* 2/10 (- n 3pi/4)))
          ]))
     #;(lambda (n) (add1 n)))
    ((H1)
     (lambda (n)
       (cond
         [(< n pi/4)
          (max 1 (+ 0.9 (sin n)))]
         [(< n 3pi/4)
          (add1 n)]
         [else
           h-min])))
    (else
      (raise-argument-error 'make-embedding-line "embedding?" e))))

(define (frame-plot p)
  (add-axis-arrow (add-axis-arrow p 'x) 'y))

(define (add-axis-arrow p xy)
  (define find-dest
    (case xy
      ((x)
       rb-find)
      ((y)
       lt-find)
      (else (raise-argument-error 'add-axis-arrow "(or/c 'x 'y)" 1 p xy))))
  (pin-arrow-line 20 p p lb-find p find-dest #:line-width 6))

(define (add-overhead-axis-labels pp [legend? #t])
  (define y-label (tcodesize "Overhead"))
  (define x-label (tcodesize "Num. Types"))
  (define fp (frame-plot pp))
  (if legend?
    (ht-append pico-x-sep y-label (vr-append pico-y-sep fp x-label))
    fp))

(define example-plot-w 240)
(define example-perf-y 12/100)

(define (make-scatterplots-pict)
  (scale-to-fit (bitmap "src/cache-scatterplots.png") (* 95/100 client-w) (* 9/10 client-h)))

(define (make-clock height)
  (bitmap (clock-icon 0 15 #:height height #:face-color white #:hand-color "darkblue")))

(define (make-sieve-pict)
  (define base-plot (bitmap "src/sieve.png"))
  (define full-plot
    (ppict-do base-plot
      #:go (coord -1/100 0 'rt) (make-clock 50)
      #:go (coord 101/100 1 'lb) (scale T-node 55/100)))
  (define mini-w (w%->pixels 2/10))
  (define mini-h (h%->pixels 3/10))
  (define lattice-pict
    (let* ((point* (for*/list ((a (in-list '(U T))) (b (in-list '(U T)))) (make-lattice-point a b)))
           (h (hc-append small-x-sep (car point*) (car (cdddr point*))))
           (hv (vc-append tiny-y-sep (cadr point*) h (caddr point*))))
      (scale-to-fit hv mini-w mini-h)))
  (hb-append small-x-sep (cc-superimpose (blank mini-w mini-h) lattice-pict) full-plot))

(define (sample-disk sym)
  (define color (if sym (symbol->color sym) "dark gray"))
  (cellophane (disk (pict-height @t{x}) #:draw-border? (not sym) #:color color) 8/10))

(define (sample-bar)
  (define color (symbol->color 'E))
  (cellophane (filled-rectangle 50 8 #:color color #:draw-border? #f) 8/10))

(define (make-timeline-bar w h label)
  (define color (if label gray white))
  (define bar (filled-rounded-rectangle w h 1 #:color color #:draw-border? #f))
  (ppict-do bar
    #:go (coord 2/100 48/100 'lc) (tcodesize (or label "."))
    #:go (coord 98/100 48/100 'rc) (tcodesize ".")))

(define (make-timeline-span h label)
  (define span-radius 7)
  (define bar-pict (filled-rounded-rectangle 25 h span-radius #:color timeline-span-color #:draw-border? #f))
  (define label-pict (ct label))
  (ht-append 10 bar-pict label-pict))

(define (make-timeline w h)
  (let* ((month*
           '("Nov" "Dec" "Jan'20" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"))
         (bar-h
           (/ h (* 2 (length month*))))
         (make-span-h
           (lambda (i) (* i bar-h)))
         (make-span-%
           (lambda (i) (/ (make-span-h i) h)))
         (base
           (for/fold ((acc (blank)))
                     ((m (in-list month*)))
             (vl-append 0 acc (make-timeline-bar w bar-h m) (make-timeline-bar w bar-h #f))))
         (timeline
           (ppict-do base
             #:go (coord 14/100 0 'lt)
             (make-timeline-span (make-span-h 6) "model")
             #:go (coord 29/100 0 'lt)
             (make-timeline-span (make-span-h 12) "implementation")
             #:go (coord 44/100 (make-span-%  7) 'lt)
             (make-timeline-span (make-span-h 8) "evaluation")
             #:go (coord 59/100 (make-span-% 11) 'lt)
             (make-timeline-span (make-span-h 4) "paper")
             #:go (coord 74/100 (make-span-% 13) 'lt)
             (make-timeline-span (make-span-h 7) "dissertation"))))
    (add-rounded-border
      #:radius 5 #:y-margin 6 #:frame-width 3 #:frame-color "slategray"
      timeline)))

(define q-black (hex-triplet->color% #x333333))

(define q-pict
  (cc-superimpose
    (disk 55 #:color q-black #:draw-border? #f)
    ((make-string->text #:font (cons 'bold code-font) #:color white #:size title-size) "?")))

(define benefits-plot-w 400)

(define (make-benefits-plot-pict stage-sym)
  (if (eq? stage-sym 'A)
    (make-overhead-plot '(H 1) benefits-plot-w #:legend? #f)
    (let ((pp (make-overhead-plot '(H 1 H1) benefits-plot-w #:legend? #f)))
      (case stage-sym
        ((B)
         pp)
        ((C)
         (ppict-do pp
            #:go (coord 66/100 82/100 'cc) q-pict))
        (else
          (raise-argument-error 'make-benefits-plot-pict "(or/c 'A 'B 'C)" stage-sym))))))

(define (symbol->node sym)
  (case sym
    ((U #f) U-node)
    ((T #t) T-node)
    (else (raise-argument-error 'symbol->node "(or/c 'U 'T #f #t)" sym))))

(define (make-node-cluster tag kind*)
  (define n0 (symbol->node (car kind*)))
  (define n1 (symbol->node (cadr kind*)))
  (define n2 (symbol->node (caddr kind*)))
  (define combo
    (vc-append tiny-y-sep (hc-append tiny-x-sep n0 n1) n2))
  (add-hubs combo tag))

(define (make-benefits-boundary-pict)
  (let* ((lhs (make-node-cluster 'L '(U U U)))
         (rhs (add-spotlight-background
                (make-node-cluster 'R '(T T T))
                #:border-color honest-color #:color honest-color
                #:border-width 1 #:x-margin pico-x-sep #:y-margin pico-y-sep))
         (mid (add-hubs T-node 'C))
         (combo (hc-append med-x-sep  lhs mid rhs))
         (arr* (list
                 (program-arrow 'L-E rb-find 'C-W lb-find (* 85/100 turn) (* 10/100 turn) 1/4 1/4 black)
                 (program-arrow 'C-W lt-find 'L-E rt-find (* 35/100 turn) (* 60/100 turn) 1/4 1/4 black)
                 (program-arrow 'C-E rb-find 'R-W lb-find (* 85/100 turn) (* 10/100 turn) 1/4 1/4 black)
                 (program-arrow 'R-W lt-find 'C-E rt-find (* 35/100 turn) (* 60/100 turn) 1/4 1/4 black)))
         (pp
           (for/fold ((acc combo))
                     ((arr (in-list arr*)))
             (add-program-arrow acc arr #:style 'solid)))
         (pp
           (ppict-do pp
             #:go (at-find-pict 'C cc-find 'cc)
             (add-spotlight-background
               T-node
               #:border-color lying-color #:color lying-color
               #:border-width 1 #:x-margin pico-x-sep #:y-margin med-y-sep))))
    pp))

(define (make-lattice-point . k*)
  (make-lattice-point* k*))

(define (make-lattice-point* k*)
  (define x-sep 20)
  (define y-sep 20)
  (define n* (map symbol->node k*))
  (define top (apply hb-append tiny-x-sep n*))
  (make-program-pict
    #:x-margin x-sep #:y-margin y-sep
    #:bg-color white #:frame-color black
    top))

(define lib/spotlight
  (add-spotlight-background
    #:color lying-color #:border-width 1 #:border-color lying-color
    #:x-margin med-x-sep #:y-margin small-y-sep
    Lib-node))

(define (make-benefits-lattice-pict)
  (define point* (for*/list ((a (in-list '(U T))) (b (in-list '(U T)))) (make-lattice-point a b)))
  (define h (hc-append tiny-x-sep (cadr point*) (caddr point*)))
  (define hv (vc-append tiny-y-sep (car (cdddr point*)) h (car point*) lib/spotlight))
  hv)

(define (make-small-benefits-lattice-pict)
  (define point (make-lattice-point 'U 'U))
  (define point-blank (ghost point))
  (define hv (vc-append tiny-y-sep point-blank point-blank point lib/spotlight))
  hv)

(define model-sidebar-x 14/100)

(define model-text-y 19/100)

(define model-text-coord (coord 3/10 model-text-y 'lt #:sep small-y-sep))
(define model-illustration-coord (coord 90/100 model-text-y 'cc #:abs-y tiny-y-sep))

(define perf-illustration-coord (coord 38/100 30/100 'ct))
(define perf-text-coord (coord slide-text-left model-text-y 'lt))
(define perf-sidebar-x 86/100)
(define perf-sidebar-w (w%->pixels 24/100))

(define lattice-text-coord (coord 5/10 model-text-y 'ct))
(define lattice-illustration-coord (coord 5/10 35/100 'ct))

(define (scale-perf-sidebar pp)
  (scale-to-fit pp (- perf-sidebar-w 30) (h%->pixels 18/100)))

(define (make-model-pict)
  (define bg (ghost med-PLT-pict))
  (define ht (make-string->title #:size (+ 16 title-size)))
  (add-rounded-border
    #:background-color white
    #:frame-color black
    #:frame-width 2
    (ppict-do bg
      #:go (coord 2/10 30/100 'cc) (ht "λ")
      #:go (coord 5/10 65/100 'cc) (ht "τ")
      #:go (coord 75/100 35/100 'cc) (ht "→"))))

(define (make-impl-pict)
  med-PLT-pict)

(define (make-sidebar-background)
  (filled-rectangle perf-sidebar-w client-h #:color bg-accent-color #:draw-border? #f))

(define (make-model-sidebar)
  (define bg (make-sidebar-background))
  (ppict-do bg
    #:go (coord 1/2 25/100 'ct) (scale-perf-sidebar (make-model-pict))
    #:go (coord 1/2 55/100 'ct) (scale-perf-sidebar (make-impl-pict))))

(define (make-perf-sidebar)
  (define bg (make-sidebar-background))
  (ppict-do bg
    #:go (coord 1/2 20/100 'ct) (scale-perf-sidebar (make-benefits-plot-pict 'B))
    #:go (coord 1/2 48/100 'ct) (scale-perf-sidebar (make-benefits-boundary-pict))
    #:go (coord 1/2 65/100 'ct) (scale-perf-sidebar (make-benefits-lattice-pict))))

(define complement-pict
  (make-overhead-plot '(H 1) example-plot-w #:legend? #f))

(define math-warning-pict
  (scale-to-fit (bitmap "src/array-warning.png") (w%->pixels 9/10) client-h))

(define design-cloud-y 20/100)

(define guarantees-cloud-coord (coord 5/100 design-cloud-y 'lt))
(define performance-cloud-coord (coord 95/100 design-cloud-y 'rt))

(define (text-cloud str label-pict . pp*)
  (define txt-pict (apply vl-append tiny-y-sep pp*))
  (define title-pict (sbt str))
  (define radius 8)
  (add-rounded-border
      #:frame-width 0 #:frame-color black #:background-color white #:radius radius
      #:x-margin 0 #:y-margin 0
    (add-rounded-border
      #:frame-width 3 #:frame-color black #:background-color bg-accent-color
      #:radius radius #:x-margin small-x-sep #:y-margin small-y-sep
      (vc-append
        pico-y-sep
        (hc-append small-y-sep title-pict (scale-to-fit label-pict 80 80))
        txt-pict))))

(define (large-rounded-border pp #:bg-color [bg-color #f])
  (my-rounded-border pp 4 #:bg-color bg-color))

(define (thin-rounded-border pp)
  (my-rounded-border pp 2))

(define (my-rounded-border pp fw #:bg-color [pre-bg-color #f])
  (define bg (or pre-bg-color white))
  (add-rounded-border
    #:radius 2 #:frame-width fw
    #:x-margin small-x-sep #:y-margin small-y-sep #:background-color bg
    pp))

(define (double-sample-disk s0 s1)
  (define p0 (sample-disk s0))
  (define p1 (sample-disk s1))
  (define w (pict-width p0))
  (define h (pict-height p0))
  (define both (ppict-do (blank (* 2 w) (* 2 h)) #:go (coord 0 0 'lt) p0 #:go (coord 1 1 'rb) p1))
  (scale-to-fit both w h))

(define lattice-num-bits 6)
(define lattice-red honest-color)
(define lattice-orange lying-color)

(define (add-lattice-background #:color color pp)
  (add-rectangle-background
    #:color white #:radius 12
    (add-rounded-border
      #:radius 12 #:x-margin tiny-x-sep #:y-margin tiny-y-sep #:background-color (color%-update-alpha color 0.5)
      #:frame-color black #:frame-width 2
      pp)))

(define (make-transient-lattice bits)
  (add-lattice-background
    #:color lattice-orange
    (scale (make-lattice bits make-lattice-point*) 6/100)))

(define (make-takikawa-lattice #:color [color lattice-red])
  (add-lattice-background
    #:color color
    (scale (make-lattice lattice-num-bits make-lattice-point*) 1/5)))

(define (make-greenman-lattice)
  (define l0 (make-takikawa-lattice))
  (define l1 (make-takikawa-lattice #:color lattice-orange))
  (ppict-do l0 #:go (coord 58/100 10/10 'ct) l1))

(define (make-3N-lattice)
  (define l0 (make-takikawa-lattice))
  (define l1 (scale (make-takikawa-lattice #:color lattice-orange) 3/10))
  (ppict-do l0
    #:go (coord 1/2 5/100 'cb) (make-transient-lattice lattice-num-bits)
    #:go (coord 4/10 6/10 'cb) (make-transient-lattice 2)
    #:go (coord 525/1000 47/100 'cb) (make-transient-lattice 3)))

(define transition-background
  (let ()
    (define w (* 3/2 client-w))
    (define h (* 3/2 client-h))
    (define c (color%-update-alpha black 0.2))
    (define (draw-rect dc dx dy)
      (define old-brush (send dc get-brush))
      (define old-pen (send dc get-pen))
      (send dc set-pen (new pen% [width 0] [color transparent]))
      (send dc set-brush (new brush% [color c] [style 'bdiagonal-hatch]))
      (define path (new dc-path%))
      (send path rectangle 0 0 w h)
      (send dc draw-path path dx dy)
      (send dc set-pen old-pen)
      (send dc set-brush old-brush))
    (dc draw-rect w h)
    #;(filled-rectangle
      w
      h
      #:color c
      #:draw-border? #f)))

(define (make-transition-slide str)
  (pslide
    #:go center-coord
    transition-background
    #:go (coord 1/2 25/100 'ct)
    (add-landscape-background
      #:x-margin small-x-sep
      #:y-margin tiny-y-sep
      (sbt str))))

(define (make-measurements+venue venue-str base-str expt-str)
  (define base-pict (t base-str))
  (define expt-pict (tcodesize expt-str))
  (define base/expt (ht-append (vl-append (blank 0 (* 2/4 (pict-height expt-pict))) base-pict) expt-pict))
  (define-values [venue-pict h-append]
    (if (regexp-match? #rx"[0-9]+$" venue-str)
      (values (make-short-citation venue-str) hc-append)
      (values (t venue-str) hb-append)))
  (h-append venue-pict (hb-append @t{ = } base/expt  @t{ measurements})))

(define (make-short-citation str #:wide? [wide? #false])
  (define bg-color (venue->color (string->symbol (car (regexp-match #rx"[A-Z]+" str)))))
  (add-rounded-border
    #:radius 16 #:x-margin small-x-sep #:y-margin small-y-sep
    #:frame-width 2 #:frame-color black #:background-color bg-color
    (let* ((ab (string-split str " "))
           (a (car ab))
           (b (cadr ab)))
      (hb-append
        (lc-superimpose
          (if wide? (ghost (tcodesize "OOPSLA")) (blank))
          (tcodesize a))
        (tcodesize (format " ~a" b))))))

(define (venue->color sym)
  (define conference-color water-color)
  (define journal-color conference-color)
  (define workshop-color journal-color)
  (case sym
    ((POPL ICFP OOPSLA PLDI) conference-color)
    ((JFP) journal-color)
    ((PEPM) workshop-color)
    (else workshop-color)))

(define (make-long-citation venue-str #:title title-str #:author* author-str*)
  (ht-append
    tiny-x-sep
    (make-short-citation venue-str #:wide? #true)
    (vl-append
      pico-y-sep
      #;(btcodesize title-str)
      (parameterize ((current-font-size 2))
        (para (map tcodesize (authors* author-str*)) #:width (w%->pixels 7/10))))))

(define (make-research-topic title #:background-color bg-color . cite*)
  (make-research-topic* title cite* #:background-color bg-color))

(define (make-research-topic* title cite* #:background-color bg-color)
  (define body (apply vl-append tiny-y-sep cite*))
  (define whole
    (vl-append (h%->pixels 3/100) (st title) (ht-append (blank small-x-sep 0) body)))
  (lt-superimpose
    (filled-rectangle
      (* 5/4 client-w)
      (+ (h%->pixels 4/100) (pict-height whole))
      #:color bg-color
      #:draw-border? #true
      #:border-width 1
      #:border-color black)
    (vl-append (blank 0 tiny-y-sep) (ht-append (blank (w%->pixels 6/100) 0) whole))))

(define (make-checklist kv* #:hide? [hide? #false])
  (define text-width (h%->pixels 86/100))
  (define x-pict @titlet{X})
  (define items-pict
    (make-2table
      #:col-sep tiny-x-sep
      #:row-sep small-y-sep
      (for/list ((kv (in-list kv*)))
        (define k-data (car kv))
        (define v-str (cdr kv))
        (define k-pict
          (hc-append @titlet{[} (if (and (not hide?) k-data) x-pict (ghost x-pict)) @titlet{]}))
        (define v-pict
          (parameterize ((current-font-size body-size))
            (para
              #:width text-width
              (append
                (map t (string-split v-str))
                (for/list ((str (in-list (if (list? k-data) k-data '()))))
                  (let* ((pp (scale (make-short-citation str) 7/10))
                         (pp (ppict-do (blank (pict-width pp) small-y-sep) #:go (coord 0 0 'lt) pp))
                         (pp (if hide? (ghost pp) pp)))
                    pp))))))
        (cons k-pict v-pict))))
  (add-rounded-border
    #:radius 1 #:frame-width 1 #:frame-color black #:x-margin tiny-x-sep #:y-margin small-y-sep
    #:background-color sand-color
    items-pict))

(define full-checklist-data
  (list
    (cons '("POPL 2016" "JFP 2019")
          "measure the performance of honest types")
    (cons '("OOPSLA 2018")
          "try to directly improve performance")
    (cons '("ICFP 2018" "OOPSLA 2019")
          "formally classify alternative types")
    ;;(cons #f "measure performance of other appreaches")
    (cons #f
          "develop a combined model, measure combined performance")))

(define checklist-coord (coord 1/2 4/100 'ct))

(define topbar-h (h%->pixels 9/100))

(define (make-topbar-background)
  (filled-rectangle client-w topbar-h #:color topbar-accent-color #:draw-border? #f))

(define (scale-topbar pp)
  (scale-to-fit pp (w%->pixels 25/100) (- topbar-h 20)))

(define (make-benefit-migration-pict)
  (hc-append tiny-x-sep U-node (make-migration-arrow) T-node))

(define (make-benefit-library-pict)
  (hc-append tiny-x-sep Lib-node (scale (vc-append pico-y-sep U-node T-node) 8/10)))

(define (make-benefit-compatibility-pict)
  (bitmap "src/bridge.png"))
  #;(
  (define border-width 3)
  (define pipe-end
    (filled-rounded-rectangle small-x-sep 100 4 #:color pipe-color #:draw-border? #t #:border-width border-width #:border-color black))
  (define pipe-mid
    (filled-rectangle (* 2 med-x-sep) 80 #:color pipe-color #:draw-border? #t #:border-width border-width #:border-color black))
  (lc-superimpose
    (rc-superimpose pipe-mid pipe-end)
    pipe-end))

(define (make-benefits-topbar)
  (define bg (make-topbar-background))
  (ppict-do bg
    #:go (coord 20/100 1/2 'cc) (scale-topbar (make-benefit-migration-pict))
    #:go (coord 50/100 1/2 'cc) (scale-topbar (make-benefit-library-pict))
    #:go (coord 80/100 1/2 'cc) (scale-topbar (make-benefit-compatibility-pict))))

(define benefits-bar-coord (coord 1/2 18/100 'ct))
(define benefits-below-bar-y 34/100)
(define benefits-pict-coord (coord 93/100 slide-top 'rt))

(define (make-library-octopus)
  (let* ((node*
           (for/list ((pp (in-list (list U-node U-node T-node U-node)))
                      (i (in-naturals)))
             (add-hubs pp (string->symbol (format "N~a" i)))))
         (bot-row (apply hc-append med-x-sep node*))
         (top+bot
           (ppict-do (vc-append med-y-sep (add-hubs Lib-node 'lib) bot-row)
             #:go (at-find-pict 'lib cb-find 'ct)
             (let ((arr-sep 20))
               (ht-append (* 3/4 arr-sep) (tag-pict (blank arr-sep 0) 'lib-left) (tag-pict (blank arr-sep 0) 'lib-right)))))
         (arr*
           (list
             (program-arrow 'N0-N ct-find 'lib-left lb-find (* 25/100 turn) (* 14/100 turn) 1/4 1/4 black)
             (program-arrow 'N1-N ct-find 'lib-left rb-find (* 25/100 turn) (* 20/100 turn) 2/4 2/4 black )
             (program-arrow 'N2-N ct-find 'lib-right lb-find (* 25/100 turn) (* 30/100 turn) 2/4 2/4 black )
             (program-arrow 'N3-N ct-find 'lib-right rb-find (* 25/100 turn) (* 36/100 turn) 1/4 1/4 black ))))
    (for/fold ((acc top+bot))
              ((arr (in-list arr*)))
      (add-program-arrow acc arr #:style 'solid))))

(define guarantee-bubble*
  (list
    (tag-pict (make-complete-monitoring-bubble) complete-monitoring-tag)
    (tag-pict (make-type-sound-bubble) type-sound-tag)
    (tag-pict (make-tag-sound-bubble) tag-sound-tag)
    (tag-pict (make-uni-sound-bubble) uni-sound-tag)))

(define (interleave aa* bb*)
  (let loop ((a* aa*)
             (b* bb*))
    (cond
      [(and (null? a*) (null? b*))
       '()]
      [(and (not (null? a*)) (not (null? b*)))
       (cons (car a*) (cons (car b*) (loop (cdr a*) (cdr b*))))]
      [else
        (raise-arguments-error 'interleave "lists with different length" "len0" (length aa*) "len1" (length bb*))])))

(define (make-bubble-table pp*)
  (table 2 pp* lc-superimpose cc-superimpose tiny-x-sep tiny-y-sep))

(define bubble-table-coord (coord slide-left slide-top 'lt))

(define (at-bar-find tag)
  (at-find-pict tag lt-find 'lt #:abs-x pico-x-sep #:abs-y tiny-y-sep))

(define (bubble-with-bar sym)
  (define-values [base-pict color left?]
    (case sym
      ((cm) (values (make-complete-monitoring-bubble) honest-color #f))
      ((tag) (values (make-tag-sound-bubble) lying-color #t))
      (else (raise-argument-error 'bubble-with-bar "(or/c cm tag)" sym))))
  (define bubble
    (scale base-pict 8/10))
  (define rect
    (filled-rectangle (w%->pixels 45/100) (h%->pixels 8/100) #:color color))
  (define sep
    (blank small-x-sep 0))
  (if left?
    (lc-superimpose rect (hc-append sep bubble))
    (rc-superimpose rect (hc-append bubble sep))))

(define (make-interaction l-node r-node str)
  (define base
    (hc-append med-x-sep (add-hubs l-node 'L) (add-hubs r-node 'R)))
  (define pp
    (add-program-arrow base (program-arrow 'L-E rt-find 'R-W lt-find (* 18/100 turn) (* 83/100 turn) 1/2 1/2 black)))
  (vc-append pico-y-sep (t str) pp))

(define (make-tu-interaction str)
  (make-interaction T-node U-node str))

(define (make-ut-interaction str)
  (make-interaction U-node T-node str))

(define example-library-x 824/1000)
(define example-library-coord (coord example-library-x big-program-y 'ct))
(define example-api-x 497/1000)
(define example-api-coord (coord example-api-x big-program-y 'ct))
(define example-client-x 178/1000)
(define example-client-coord (coord example-client-x big-program-y 'ct))
(define example-code-x-margin (w%->pixels 4/100))
(define example-code-y-margin (h%->pixels 4/100))

(define (make-typed-codeblock #:title [title #f] #:x-margin [x #f] #:y-margin [y #f] . pp*)
  (make-typed-codeblock* pp* #:title title #:x-margin x #:y-margin y))

(define (make-untyped-codeblock #:title [title #f] #:x-margin [x #f] #:y-margin [y #f] . pp*)
  (make-untyped-codeblock* pp* #:title title #:x-margin x #:y-margin y))

(define (make-typed-codeblock* pp* #:title [title #f] #:x-margin [x #f] #:y-margin [y #f])
  (make-codeblock pp* #:title title #:label (scale T-node codeblock-label-scale) #:bg-color typed-color #:x-margin x #:y-margin y))

(define (make-untyped-codeblock* pp* #:title [title #f] #:x-margin [x #f] #:y-margin [y #f])
  (make-codeblock pp* #:title title #:label (scale U-node codeblock-label-scale) #:bg-color untyped-color #:x-margin x #:y-margin y))

(define codeblock-label-scale 0.65)

(define (make-codeblock pp* #:title [title #f] #:label [label #f] #:bg-color [pre-bg-color #f] #:x-margin [pre-x-margin #f] #:y-margin [pre-y-margin #f])
  (define bg-c (or pre-bg-color (string->color% "lightgray")))
  (define label-margin (if label (* 50/100 (pict-height label)) 0))
  (define (add-label-margin pp [extra 0]) (vl-append (+ extra label-margin) (blank) pp))
  (let* ((block-pict
          (make-program-pict
            #:frame-color bg-c
            #:bg-color (color%-update-alpha bg-c 0.4)
            #:x-margin pre-x-margin
            #:y-margin pre-y-margin
            #:radius 4
            (add-label-margin (apply vl-append (h%->pixels 2/100) pp*))))
         (title-pict (and title (tcodesize title))))
    (if label
      (let ((block-pict (add-label-margin block-pict 2)))
        (ppict-do (if title-pict (lt-superimpose block-pict (ht-append 4 (blank) title-pict)) block-pict)
          #:go (coord 1/2 0 'ct) label))
      (if title-pict (vl-append 0 title-pict block-pict) block-pict))))

(define (make-example-codeblocks lib api usr)
  (values
    (make-untyped-codeblock* lib #:title "Library" #:x-margin example-code-x-margin #:y-margin example-code-y-margin)
    (make-typed-codeblock* api #:title "API" #:x-margin example-code-x-margin #:y-margin example-code-y-margin)
    (make-untyped-codeblock* usr #:title "Client" #:x-margin example-code-x-margin #:y-margin example-code-y-margin)))

(define client-f-tag 'client-f)
(define client-fold-tag 'client-fold)
(define api-fold-tag 'api-fold)
(define api-lib-tag 'api-lib)
(define lib-def-tag 'lib-def)
(define callback-tag 'callback)

(define example-client-code*
  (list
    (hb-append @ct{(define path "/tmp/file.txt")})
    @ct{ }
    (hb-append @ct{(define } (tag-pict @ct{(count acc ln)} client-f-tag))
    (hb-append @ct{  (+ 1 acc))})
    @ct{ }
    (hb-append @ct{(} (tag-pict @ct{t:fold-file} client-fold-tag) @ct{ path 0 count)})))

(define example-api-code*
  (list
    (hb-append @ct{(provide})
    (hb-append @ct{  } (tag-pict @ct{t:fold-file} api-fold-tag) @ct{ : (All (Acc) (-> Path Acc})
    (hb-append @ct{                           (-> Acc Str Acc) Acc)))})
    (hb-append @ct{(define t:fold-file } (tag-pict @ct{u:fold-file} api-lib-tag) @ct{)})))

(define example-library-code*
  (list
    (hb-append @ct{(define (} (tag-pict @ct{fold-file path acc f)} lib-def-tag))
    (hb-append @ct{  ... ; read `ln` from `path`})
    (hb-append @ct{  ... } (tag-pict @ct{(f ln acc)} callback-tag) @ct{ ...})
    (hb-append @ct{  ...)})))

(define-values [example-library-code example-api-code example-client-code]
  (make-example-codeblocks example-library-code* example-api-code* example-client-code*))

(define (make-x-line angl [w #f] [h #f])
  (filled-rounded-rectangle w h 2 #:color "red" #:angle angl #:draw-border? #f))

(define implies-pict
  ((make-string->body #:size 70) "⇒"))

(define title-pict
  (vr-append
    (h%->pixels 2/100)
    (add-landscape-background
      #:x-margin small-x-sep
      #:y-margin tiny-y-sep
      (vc-append
        small-y-sep
        @titlet{Honest and Lying Types}
        @t{Thesis Proposal}))
    (add-landscape-background
      #:x-margin small-x-sep
      #:y-margin tiny-y-sep
      (vc-append
        tiny-y-sep
        @t{Ben Greenman}
        @t{2019-11-25}))))

(define (compare-append . pp*)
  (compare-append* pp*))

(define (compare-append* pp*)
  (define eq-pict (titlet "~"))
  (add-rounded-border
    #:radius 4 #:frame-width 2 #:frame-color black #:x-margin tiny-x-sep #:y-margin tiny-y-sep #:background-color white
    (for/fold ((acc (car pp*)))
              ((pp (in-list (cdr pp*))))
      (hc-append tiny-x-sep acc eq-pict pp))))

(define (apple-pict)
  (scale-to-fit (fruit-bitmap "apple") 120 120))

(define (pear-pict)
  (scale-to-fit (fruit-bitmap "pear") 120 120))

(define (orange-pict)
  (scale-to-fit (fruit-bitmap "orange") 120 120))

(define (fruit-bitmap str)
  (bitmap (format "src/~a.png" str)))

(define (icon-credit-pict)
  (hb-append @tcodesize{Icons made by Freepik from Flaticon.com}))

(define fruit-coord (coord 1/2 68/100 'ct))

(define (make-goal+problem-table [y-sep med-y-sep])
  (table 3
    (list
      @sbt{Goal} @st{=} @st{Migratory Typing}
      @sbt{Problem} @st{=} @st{Performance})
    lc-superimpose cc-superimpose tiny-x-sep y-sep))

(define (make-city-dot str #:tag [tag #f])
  (define d
    (let ((base-d (disk 16 #:draw-border? #f #:color city-color)))
      (if tag (tag-pict base-d tag) base-d)))
  (vc-append pico-y-sep d (bt str)))

(define mt-code-y 80/100)

(define (color-text str #:color color)
  ((make-string->text #:font (cons 'bold body-font) #:size body-size #:color color) str))

(define stx-compile-txt
  (hb-append @t{Type Check: } (color-text "Ok" #:color "ForestGreen")))

(define stx-run-txt
  (hb-append @t{Runtime: } (color-text "Error" #:color "red") @t{ could not convert type to a contract}))

(define (make-thesis-proposal name day)
  (define pp
    (frame
      (bitmap (build-path "src" (format "~a.jpg" name)))
      #:line-width 4))
  (define txt
    (vl-append tiny-y-sep
               (t "Thesis Proposal: \"Gradual Typing\"")
               (t (format "November ~a, 2019" day))))
  (ht-append small-x-sep pp txt))

(define (add-outline-axis-arrow pp src-tag find-src tgt-tag find-tgt)
  (define arr-spec (program-arrow src-tag find-src tgt-tag find-tgt #f #f #f #f black))
  (add-program-arrow pp arr-spec #:arrow-size 20 #:line-width 6 #:style 'solid))

(define (make-origin-coord align [abs-x 0] [abs-y 0]) (coord 40/100 6/10 align #:abs-x abs-x #:abs-y abs-y))

(define (make-outline-square)
  (define c (string->color% "LightSkyBlue"))
  (define w (w%->pixels 20/100))
  (filled-rectangle w w #:color (color%-update-alpha c 8/10) #:draw-border? #f #;(#:border-width 4 #:border-color c)))

(define (make-no-box . pp*)
  (make-no-box* pp*))

(define (make-no-box* pp*)
  (large-rounded-border #:bg-color no-box-color (apply vc-append tiny-y-sep pp*)))

(define (make-yes-box . pp*)
  (make-yes-box* pp*))

(define (make-yes-box* pp*)
  (add-rounded-border
    #:x-margin small-x-sep #:y-margin small-y-sep #:background-color yes-box-color
    #:radius 10 #:frame-width 4
    (apply vc-append tiny-y-sep pp*)))

(define (add-research-arrow pp spec #:style [style 'solid])
  (add-program-arrow pp spec #:style style #:line-width 6 #:arrow-size 20))

(define (honest-lying-rect w h)
  (define c0 honest-color)
  (define c1 white)
  (define c2 lying-color)
  (define (draw-rect dc dx dy)
    (define old-brush (send dc get-brush))
    (define old-pen (send dc get-pen))
    (define x-mid (+ dx (* w 1/2)))
    (send dc set-pen (new pen% [width 1] [color black]))
    (send dc set-brush
          (new brush%
               [gradient
                 (new linear-gradient%
                      [x0 x-mid] [y0 dy]
                      [x1 x-mid] [y1 (+ dy h)]
                      [stops `((0 ,c0)
                               (55/100 ,c1)
                               (65/100 ,c1)
                               (1 ,c2))])]))
    (define path (new dc-path%))
    (send path rectangle 0 0 w h)
    (send dc draw-path path dx dy)
    (send dc set-pen old-pen)
    (send dc set-brush old-brush))
  (dc draw-rect w h))

;; =============================================================================

(define (do-show)
  (set-page-numbers-visible! PREVIEW)
  (set-spotlight-style! #:size 60 #:color (color%-update-alpha highlight-brush-color 0.6))
  ;; --
  (sec:title)
  (parameterize ([current-slide-assembler (slide-assembler/background (current-slide-assembler) #:color ice-color)])
    (void)
;    (sec:gtt-compare)
;    (sec:migratory-typing)
;    (sec:design-space)
    (sec:proposal)
    (sec:plan)
    (sec:timeline)
    (pslide)
    (sec:QA)
    (void)))

;; -----------------------------------------------------------------------------
;; title

(define (sec:title)
  (pslide
    #:go center-coord meeting-of-the-waters
    #:go (coord 1/2 20/100 'ct) title-pict)
  (pslide
    #:go center-coord
    (vl-append
      tiny-y-sep
      @t{Committee:}
      @t{  1. Matthias Felleisen}
      @t{  2. Amal Ahmed}
      @t{  3. Jan Vitek}
      @t{  4. Shriram Krishnamurthi}
      @t{  5. Fritz Henglein}
      @t{  6. Sam Tobin-Hochstadt}))
  (pslide
    #:go center-coord meeting-of-the-waters
    #:go (coord 1/2 20/100 'ct) title-pict)
  (void))

(define (sec:gtt-compare)
  (pslide
    #:go (coord slide-text-left 2/10 'lt #:sep med-y-sep)
    (make-thesis-proposal "max_new" 18)
    (make-thesis-proposal "ben_greenman" 25))
  (pslide
    #:go (make-origin-coord 'cc) (tag-pict (blank 0 0) 'origin)
    #:go (make-origin-coord 'rt) (tag-pict (blank (h%->pixels 1/10) (h%->pixels 5/100)) 'q3)
    #:go (make-origin-coord 'lb) (tag-pict (blank (h%->pixels 4/10) (h%->pixels (* 4 5/100))) 'q1)
    #:go (make-origin-coord 'lb) (tag-pict (blank (w%->pixels 4/10) 0) 'x-max)
    #:go (make-origin-coord 'rt) (tag-pict (blank 0 (h%->pixels 1/10)) 'y-min)
    #:go (make-origin-coord 'cb) (tag-pict (blank 0 (h%->pixels 4/10)) 'y-max)
    #:set (add-outline-axis-arrow ppict-do-state 'y-min cb-find 'y-max ct-find)
    #:go (at-find-pict 'y-max lt-find 'rt #:abs-x (- tiny-x-sep)) @bt{Proofs}
    #:alt
    [#:go heading-text-coord @st{Last Week:}
     #:go (at-find-pict 'y-max ct-find 'cb #:abs-y pico-y-sep) (bitmap (star 40 "solid" "goldenrod"))]
    #:set (add-outline-axis-arrow ppict-do-state 'q3 lt-find 'x-max rc-find)
    #:go (at-find-pict 'x-max rb-find 'ct #:abs-y pico-y-sep) @bt{Performance}
    #:alt
    [#:go heading-text-coord @st{Today:}
     #:go (make-origin-coord 'lb 2 (- 3)) (make-outline-square)]
    #:go heading-text-coord @st{Future:}
    #:set (add-outline-axis-arrow ppict-do-state 'q3 lb-find 'q1 rt-find)
    #:go (at-find-pict 'q1 rt-find 'lc #:abs-x pico-x-sep) @bt{People})
  (void))

(define (sec:migratory-typing)
  (make-transition-slide
    "Migratory Typing")
  (pslide
    #:go heading-text-coord
    @sbt{Migratory Typing}
    #:go (coord slide-text-left 14/100 'lt)
    @t{Add types to a dynamically-typed language}
    #:go (coord 75/100 (+ 4/100 mt-code-y) 'cb)
    (add-caption "Mixed-Typed code" (make-sample-program #f #f 1))
    #:go (coord (+ 2/100 slide-text-left) mt-code-y 'lb)
     (vl-append
       med-y-sep
       (vl-append small-y-sep (blank) (hc-append tiny-x-sep U-node @t{= untyped code}))
       (hc-append tiny-x-sep T-node (hc-append @t{= } (tag-pict @t{simply-typed} 't-line)))
       ;; TODO maybe cut Dynamic
       (vl-append small-y-sep (blank) @t{(no 'Dynamic' type)})))
  (pslide
    #:go heading-text-coord
    @st{Motivation}
    #:go center-coord
    @t{Because lots of untyped code exists.})
  (pslide
    #:go heading-text-coord
    @st{Landscape of Models and Implementations}
    #:go big-landscape-coord
    (make-implementation-landscape)
    ;;#:next
    ;;#:go (coord 1/2 35/100 'ct)
    #;(large-rounded-border (hb-append @st{Rich design space, due to a challenge})))
  (pslide
    #:go heading-text-coord
    @st{Challenge = Interoperability}
    #:alt
    [#:go (coord slide-text-left 20/100 'lt)
     (make-sample-program #f #t 1)
     #:go (coord 48/100 20/100 'lt #:sep tiny-x-sep)
     (vl-append
       tiny-x-sep
       @t{What do types mean when}
       (hb-append @t{ untyped values and})
       (hb-append @t{ typed values } @bt{interact} @t{?}))]
    #:go (coord 25/100 1/2 'cc)
    (vl-append
      (h%->pixels 15/100)
      (make-ut-interaction "Int")
      (make-ut-interaction "Listof Str"))
    #:next
    #:go (coord 75/100 1/2 'cc)
    (make-tu-interaction "Nat -> Bool"))
  (pslide
    #:go heading-text-coord
    @st{Many Answers, Many Implementations}
    #:go big-landscape-coord
    (make-implementation-landscape)
    #:next
    #:go guarantees-cloud-coord
    (st-cloud "Guarantees?" #:style '())
    #:go performance-cloud-coord
    (st-cloud "Performance?")
    #:go fruit-coord (compare-append (apple-pict) (pear-pict) (orange-pict))
    #:go (coord 96/100 96/100 'rb)
    (icon-credit-pict))
  (void))

(define (sec:design-space)
  (make-transition-slide
    "My Research")
  (pslide
    #:go heading-text-coord
    @st{Research Agenda: Scientific Comparison}
    #:go big-landscape-coord
    (make-big-landscape-background)
    #:next
    #:go guarantees-cloud-coord
    (text-cloud
      "Guarantees"
      (make-model-pict)
      @t{one syntax, many}
      @t{semantics for what}
      @t{flows across channels})
    #:go performance-cloud-coord
    (text-cloud
      "Performance"
      (make-impl-pict)
      @t{one syntax, many}
      @t{type-compilers})
    #:next
    #:go fruit-coord (compare-append (apple-pict) (apple-pict))
    #:go (coord 96/100 96/100 'rb)
    (icon-credit-pict))
  (pslide
    #:go heading-text-coord
    @st{Research Agenda: Results so Far}
;    (make-long-citation
;      "OOPSLA 18"
;      #:title "Collapsible Contracts: Fixing a Pathology of Gradual Typing"
;      #:author* '("Daniel Feltey" "Ben Greenman" "Christophe Scholliers" "Robert Bruce Findler" "Vincent St-Amour"))
    #:go (coord -2/100 12/100 'lt #:sep tiny-y-sep)
    (make-research-topic
     "Design Space Analysis"
     #:background-color "palegreen"
     (make-long-citation
       "OOPSLA 19"
       #:title "Complete Monitors for Gradual Types"
       #:author* '("Ben Greenman" "Matthias Felleisen" "Christos Dimoulas"))
     (make-long-citation
       "ICFP 18"
       #:title "A Spectrum of Type Soundness and Performance"
       #:author* '("Ben Greenman" "Matthias Felleisen")))
    #:next
    (make-research-topic
      "Performance Evaluation"
      #:background-color "lightblue"
      (make-long-citation
        "JFP 19"
        #:title "How to Evaluate the Performance of Gradual Type Systems"
        #:author* '("Ben Greenman" "Asumu Takikawa" "Max S. New" "Daniel Feltey" "Robert Bruce Findler" "Jan Vitek" "Matthias Felleisen"))
      (make-long-citation
        "PEPM 18"
        #:title "On the Cost of Type-Tag Soundness"
        #:author* '("Ben Greenman" "Zeina Migeed"))
      (make-long-citation
        "POPL 16"
        #:title "Is Sound Gradual Typing Dead?"
        #:author* '("Asumu Takikawa" "Daniel Feltey" "Ben Greenman" "Max S. New" "Jan Vitek" "Matthias Felleisen"))))
  (pslide
    #:go heading-text-coord (hb-append @st{Landscape: } @sbt{Guarantees})
    #:alt [#:go big-landscape-coord (make-big-landscape-background)]
    #:go big-landscape-coord (make-theorem-landscape)
    #:go (coord slide-text-right slide-bottom 'rb)
    (hb-append @t{(a } @it{total spectrum} @t{)}))
  (let* ((smallt (make-string->body #:size (- body-size 6)))
         (txt-pict*
           (list
             @smallt{types predict behavior}
             (descr-append
               @smallt{types predict behavior in typed}
               @smallt{code, nothing in untyped code})
             (descr-append
               @smallt{types predict shapes in typed}
               @smallt{code, nothing in untyped code})
             @smallt{types predict nothing})))
    (for ((i (in-range (length txt-pict*))))
      (pslide
        #:go bubble-table-coord
        (make-bubble-table
          (interleave
            guarantee-bubble*
            (for/list ((txt (in-list txt-pict*))
                       (j (in-naturals)))
              (if (= j i) txt (cellophane txt 5/10)))))))
    (pslide
      #:go bubble-table-coord
      (make-bubble-table (interleave guarantee-bubble* txt-pict*))))
  (let* ((spec* (list (list @bt{Honest} complete-monitoring-tag honest-color (h%->pixels 13/100))
                      (list @bt{Lying} type-sound-tag lying-color (h%->pixels 32/100))
                      (list @bt{Vacuous} uni-sound-tag vacuous-color (h%->pixels 13/100))))
         (len (length spec*)))
    (for ((i (in-range (+ 1 len))))
      (define cm-val (and (< 0 i) (list-ref spec* 0)))
      (define t-val (and (< 1 i) (list-ref spec* 1)))
      (define u-val (and (< 2 i) (list-ref spec* 2)))
      (define txt-list
        (and (< 0 i)
             (list (if cm-val (car cm-val) (blank))
                   (if t-val (car t-val) (blank))
                   (blank)
                   (if u-val (car u-val) (blank)))))
      (pslide
        #:go bubble-table-coord
        (make-bubble-table (interleave guarantee-bubble* (map (lambda (_) (blank)) guarantee-bubble*)))
        #:go (if cm-val (at-bar-find (cadr cm-val)) center-coord)
        (if cm-val (filled-rectangle client-w (car (cdddr cm-val)) #:color (caddr cm-val)) (blank))
        #:go (if t-val (at-bar-find (cadr t-val)) center-coord)
        (if t-val (filled-rectangle client-w (car (cdddr t-val)) #:color (caddr t-val)) (blank))
        #:go (if u-val (at-bar-find (cadr u-val)) center-coord)
        (if u-val (filled-rectangle client-w (car (cdddr u-val)) #:color (caddr u-val)) (blank))
        #:go bubble-table-coord
        (if txt-list
          (make-bubble-table (interleave guarantee-bubble* txt-list))
          (blank)))))
  (pslide
    #:go heading-text-coord (hb-append @sbt{Honest} @st{ vs. } @sbt{Lying} @st{ Types})
    #:next
    #:go (coord 2/100 12/100 'lt) example-client-code
    #:go (coord 50/100 60/100 'ct) example-api-code
    #:set (add-hl-arrow ppict-do-state (program-arrow client-fold-tag lb-find api-fold-tag lc-find (* 3/4 turn) (* 0 turn) 3/4 1/4 code-highlight-color))
    #:next
    #:go (coord 51/100 12/100 'lt) example-library-code
    #:set (add-hl-arrow ppict-do-state (program-arrow api-lib-tag cb-find lib-def-tag rb-find (* 99/100 turn) (* 27/100 turn) 90/100 1/4 code-highlight-color))
    #:next
    #:set (add-hl-arrow ppict-do-state (program-arrow callback-tag lc-find client-f-tag rc-find(* 45/100 turn)  (* 55/100 turn) 1/4 1/4 code-highlight-color) #:style 'dot)
    #:next
    #:go (coord 1/2 45/100 'ct #:sep pico-y-sep)
    (large-rounded-border
      @t{Do the API types protect the Client?})
    #:next
    (ht-append
      pico-x-sep
      (large-rounded-border
        #:bg-color honest-color
        (hc-append @bt{Honest } implies-pict @t{ yes}))
      (large-rounded-border
        #:bg-color lying-color
        (hc-append (cc-superimpose (ghost @bt{Honest }) @bt{Lying}) (tag-pict implies-pict 'ts-implies) @t{ yes})))
    #:go (at-find-pict 'ts-implies cc-find 'cc)
    (make-x-line (* 40/100 turn) 10 80))
  (pslide
    #:go heading-text-coord (hb-append @st{Landscape: } @sbt{Guarantees})
    #:go big-landscape-coord (make-theorem-landscape))
  (pslide
    #:go heading-text-coord
    (hb-append @st{Landscape: } @sbt{Performance})
    #:go big-landscape-coord
    #:alt [(make-big-landscape-background)]
    (make-performance-landscape)
    #:go (coord slide-text-right slide-bottom 'rb)
    (hb-append @t{Varied space, difficult to rank alternatives}))
  (pslide
    #:go heading-text-coord
    @st{Performance Comparison}
    #:go (coord slide-right slide-top 'rt)
    (make-short-citation "ICFP 2018")
    #:go (coord 50/100 3/10 'ct #:sep small-y-sep)
    (hb-append @bt{Natural} @t{ vs. } @bt{Transient})
    #:next
    #:go (coord 44/100 4/10 'rt #:sep small-y-sep) (bubble-with-bar 'cm)
    (vl-append
      tiny-y-sep
      @t{guard boundaries}
      @t{with deep checks})
    #:go (coord 54/100 4/10 'lt #:sep small-y-sep) (bubble-with-bar 'tag)
    (vl-append
      tiny-y-sep
      @t{rewrite typed code to}
      @t{tag-check inputs}))
  (pslide
    #:go heading-text-coord @st{Performance Comparison}
    #:go (coord slide-right slide-top 'rt) (make-short-citation "ICFP 2018")
    #:go (coord 1/2 26/100 'ct)
    (filled-rectangle (* 2 client-w) pico-y-sep #:color honest-color)
    #:go (coord slide-text-left 2/10 'lt #:sep tiny-y-sep)
    @bt{Natural}
    (tag-pict
      (vl-append
        tiny-y-sep
        @t{boundaries add "large"}
        @t{overhead})
      'N-txt)
    #:go (coord slide-text-right 2/10 'rt)
    (make-overhead-plot '(H) example-plot-w)
    #:go (coord 1/2 61/100 'ct)
    (filled-rectangle (* 2 client-w) pico-y-sep #:color lying-color)
    #:go (coord slide-text-left 55/100 'lt #:sep tiny-y-sep)
    @bt{Transient}
    (tag-pict
      (vl-append
        tiny-y-sep
        @t{types add "small"}
        @t{overhead})
      'S-txt)
    #:go (coord slide-text-right 55/100 'rt)
    (make-overhead-plot '(1) example-plot-w))
  #;(pslide
    #:go heading-text-coord
    (hb-append @st{Data: TR-} @sbt{Natural} @st{ (} (sample-disk 'H) @st{) vs. TR-} @sbt{Transient} @st{ (} (sample-disk '1) @st{) })
    #:go (coord 1/2 slide-text-top 'ct #:sep small-y-sep)
    (make-sieve-pict)
    (make-2table
      (list (cons (hb-append @bt{y} @t{-axis = Overhead})
                  (hb-append @bt{x} @t{-axis = Num. Types}))
            (cons (hc-append (sample-bar) @t{ = Untyped Perf.})
                  (hc-append (double-sample-disk 'H '1)  @t{ = Mixed-typed Perf.})))
      #:col-sep med-x-sep
      #:row-sep small-y-sep))
  (pslide
    #:go (coord 1/2 0 'ct #:sep pico-y-sep)
    (make-scatterplots-pict)
    (hc-append
      small-x-sep
      (hc-append (sample-bar) @t{ = Untyped Perf.})
      (hc-append (scale (sample-disk 'H) 6/10) @t{ = Natural})
      (hc-append (scale (sample-disk '1) 6/10) @t{ = Transient})))
  (void))

(define (sec:proposal)
  (make-transition-slide
    "Thesis Question")
  (pslide
    #:go center-coord
    (scale-to-fit
      (ht-append
        tiny-x-sep
        (make-theorem-landscape)
        (make-performance-landscape))
      client-w client-h)
    #:go (coord 1/2 4/100 'ct)
    (scale (make-tree 270 220 mixed-program-code* #:arrows? #f) 7/10))
  (pslide
    #:go (coord 1/2 slide-top 'ct #:sep small-y-sep)
    (add-hubs (large-rounded-border (make-goal+problem-table tiny-y-sep)) 'problem)
    #:go (at-find-pict 'problem rc-find 'cc #:abs-x small-x-sep) (make-lang-pict "L")
    #:alt [#:go (coord 1/2 1/2 'cb) @t{What to do?}]
    #:next
    #:go (coord 15/100 57/100 'ct)
    (add-hubs (make-no-box @t{Improve the} @t{compiler}) 'improve-compiler)
    #:set (add-research-arrow ppict-do-state
                             (program-arrow 'problem-S lb-find 'improve-compiler-N ct-find (* 55/100 turn) (* 3/4 turn) 30/100 30/100 black))
    #:next
    #:go (coord 45/100 60/100 'ct)
    (add-hubs (make-no-box @t{Build a new} @t{compiler}) 'new-compiler)
    #:set (add-research-arrow ppict-do-state
                              (program-arrow 'problem-S cb-find 'new-compiler-N ct-find (* 3/4 turn) (* 3/4 turn) 50/100 50/100 black))
    #:next
    #:go (coord 75/100 56/100 'ct)
    (add-hubs (make-no-box @t{Build a new} @t{language}) 'new-lang)
    #:go (at-find-pict 'new-lang rc-find 'cc #:abs-x small-x-sep) (make-lang-pict "L'")
    #:set (add-research-arrow ppict-do-state
                              (program-arrow 'problem-S rb-find 'new-lang-N ct-find (* 95/100 turn) (* 3/4 turn) 50/100 50/100 black))
    #:next
    #:go (at-find-pict 'improve-compiler-S cb-find 'ct #:abs-y pico-y-sep) (scale (make-short-citation "OOPSLA 18") 9/10)
    #:next
    #:go (at-find-pict 'new-compiler-S cb-find 'ct #:abs-y pico-y-sep) @st{Pycket}
    #:next
    #:go (at-find-pict 'new-lang-S cb-find 'ct #:abs-y pico-y-sep) @st{Grift, Nom}
    #:next
    #:go (coord 24/100 33/100 'ct)
    (add-hubs (make-yes-box @t{Interoperate with a} @t{weaker semantics}) 'weak-lang)
    #:set (add-research-arrow ppict-do-state
                              (program-arrow 'problem-S lb-find 'weak-lang-N ct-find (* 55/100 turn) (* 3/4 turn) 80/100 25/100 black)
                              #:style 'dot))
  (pslide
    #:go (coord 1/2 6/100 'ct)
    (honest-lying-rect (* 3/2 client-w) (* 65/100 client-h))
    #:go (coord slide-text-left 10/100 'lt #:sep small-y-sep)
    (make-thesis-question #t)
    #:next
    #:go (coord 163/1000 50/100 'lt)
    (vl-append
      tiny-y-sep
      @big-t{In particular,}
      (hb-append @big-tb{  Natural} @big-t{ + } @big-tb{Transient})))
  (pslide
    #:go heading-text-coord @st{Complementary Strengths}
    #:go (coord 1/2 26/100 'ct) (filled-rectangle (* 2 client-w) pico-y-sep #:color honest-color)
    #:go (coord 97/100 06/100 'rt) complement-pict
    #:go (coord slide-text-left 2/10 'lt #:sep tiny-y-sep)
    @bt{Natural}
    (vl-append
      tiny-y-sep
      (hb-append @t{types predict full behavior, but})
      (hb-append @t{need to avoid critical boundaries}))
    #:go (coord 1/2 61/100 'ct) (filled-rectangle (* 2 client-w) pico-y-sep #:color lying-color)
    #:go (coord slide-text-left 55/100 'lt #:sep tiny-y-sep)
    @bt{Transient}
    (vl-append
      tiny-y-sep
      (hb-append @t{types predict shapes, but add})
      (hb-append @t{overhead to all typed code})))
  (pslide
    #:go heading-text-coord @st{Benefits (1/3): Migration}
    #:go benefits-pict-coord (scale (make-benefit-migration-pict) 8/10)
    #:go benefits-bar-coord (make-benefits-topbar)
    #:go (coord slide-text-left benefits-below-bar-y 'lt #:sep (h%->pixels 7/100))
    (hb-append @t{1. Begin with } @bt{Natural} @t{ types})
    (hb-append @t{2. } @t{Switch to } @bt{Transient} @t{ for performance})
    (hb-append @t{3. Revisit } @bt{Natural} @t{ for debugging})
    (vl-append
      tiny-y-sep
      (hb-append @t{4. } @t{Return to } @bt{Natural} @t{ after typing all})
      @t{    critical boundaries})
    )
  (pslide
    #:go heading-text-coord @st{Benefits (2/3): Library Interaction}
    #:go benefits-pict-coord (scale (make-benefit-library-pict) 7/10)
    #:go benefits-bar-coord (make-benefits-topbar)
    #:go (coord 1/2 benefits-below-bar-y 'ct #:sep tiny-y-sep)
    #:alt
    [(hb-append @bigct{math/array} @t{: "25 to 50 times slower"})
     math-warning-pict]
    #:go (coord 1/2 40/100 'ct)
    (vc-append
      tiny-y-sep
      (blank)
      @t{Changing a library to Transient may improve}
      @t{overall performance}))
  (pslide
    #:go heading-text-coord @st{Benefits (3/3): Compatibility}
    #:go benefits-pict-coord (scale (make-benefit-compatibility-pict) 7/10)
    #:go benefits-bar-coord (make-benefits-topbar)
    #:go (coord 1/2 benefits-below-bar-y 'ct #:sep (h%->pixels 7/100))
    #:alt
    [(ht-append
       small-x-sep
       (make-typed-codeblock*
         #:title "A" #:x-margin example-code-x-margin #:y-margin example-code-y-margin
         (list
           @ct{(define stx}
           @ct{  #`#,(vector 0 1))}
           @ct{ }
           @ct{(provide stx)}))
       (make-untyped-codeblock*
         #:title "B" #:x-margin example-code-x-margin #:y-margin example-code-y-margin
         (list
           @ct{(require Library)}
           @ct{ }
           @ct{stx})))
     #:next
     #:alt [stx-compile-txt]
     (vc-append tiny-y-sep stx-compile-txt stx-run-txt)]
    (vc-append
      tiny-y-sep
      @t{Typed Racket provides 203 base types;}
      @t{12 lack runtime support (wrappers)})
    #:next
    #:alt
    [(table 3
       (map tcodesize
            '("(Async-Channel T)"
              "(Custodian-Box T)"
              "(C-Mark-Key T)"
              "(Evt T)"
              "(Ephemeron T)"
              "(Future T)"
              "(MPair T T')"
              "(MList T)"
              "(Prompt-Tag T T')" ;; Asumu SHOULD have fixed this, but appears to be old bug in the implementation --- no tests. Issue #876
              "(Syntax T)"
              "(Thread-Cell T)"
              "(Weak-Box T)"))
       cc-superimpose cc-superimpose small-x-sep small-y-sep)]
    (vc-append
      tiny-y-sep
      (hb-append @bt{Transient} @t{ does not need runtime support,})
      @t{so more code can run}))
  (void))

(define (sec:plan)
  (make-transition-slide
    "To Do")
  (pslide
    #:go (coord 1/2 6/100 'ct)
    (honest-lying-rect (* 3/2 client-w) (* 33/100 client-h))
    #:go (coord slide-text-left 10/100 'lt #:sep med-y-sep)
    (make-thesis-question #f)
    #:next
    @t{  Q1. Can honest and lying types coexist?}
    @t{  Q2. Are the benefits measurably significant?})
  (pslide
    #:go (coord model-sidebar-x 0 'ct) (make-model-sidebar)
    #:go (coord slide-right slide-top 'rt)
    @st{Q1. Can honest and lying coexist?}
    #:go model-text-coord
    #:alt
    [@t{Model:}
     (hb-append @t{- develop a combined } (tag-pict @t{model} 'model))
     @t{- formally prove basic properties}
     @t{- reduce overlap in runtime checks}
     #:go model-illustration-coord
     (scale (make-model-pict) 1/2)]
    #:next
    @t{Implementation:}
    @t{- re-use the type checker}
    @t{- support all Racket values}
    @t{- avoid the contract library}
    (vl-append
      tiny-y-sep
      @t{- adapt the TR optimizer to}
      @t{   lying types})
    #:go model-illustration-coord
    (scale (make-impl-pict) 1/2))
  (pslide
    #:go heading-text-coord
    @st{Q2. Are the benefits significant?}
    #:go (coord perf-sidebar-x 0 'ct) (make-perf-sidebar)
    #:next
    #:alt
    [#:go perf-text-coord
     @t{Goal: min(Natural, Transient)}
     #:go perf-illustration-coord
     (make-benefits-plot-pict 'B)]
    #:next
    #:go perf-text-coord
    @t{Maybe: reduce cost of U/T edge}
    #:go perf-illustration-coord
    (tag-pict (vc-append (h%->pixels 1/10) (blank) (make-benefits-boundary-pict)) 'bp)
    #:next
    #:go (at-find-pict 'bp ct-find 'ct)
    (large-rounded-border
      @t{How to find?}))
  (pslide
    #:go heading-text-coord
    @st{How to measure performance?}
    #:alt
    [#:go lattice-text-coord
     (make-measurements+venue "POPL 2016" "2" "N")
     #:go lattice-illustration-coord
     (make-takikawa-lattice)]
    #:alt
    [#:go lattice-text-coord
     (make-measurements+venue "ICFP 2018" "2" "(N+1)")
     #:go perf-illustration-coord
     (make-greenman-lattice)]
    #:go lattice-text-coord
    (hb-append (make-measurements+venue "Next" "3" "N") @t{?})
    #:go (coord 1/2 4/10 'ct)
    (make-3N-lattice)
    #:next
    #:go (coord 1/2 65/100 'ct)
    (large-rounded-border
      (vc-append
        tiny-y-sep
        @t{Need an alternative method}
        @t{to measure performance})))
  (pslide
    ;; for benchmarks that depend on typed libraries, try with + without Transient
    #:go heading-text-coord
    @st{Q2. Are the benefits significant?}
    #:go (coord perf-sidebar-x 0 'ct) (make-perf-sidebar)
    #:next
    #:go perf-text-coord
    @t{Goal: change lib, improve overall}
    #:go perf-illustration-coord
    #:alt [(make-small-benefits-lattice-pict)]
    (make-benefits-lattice-pict))
  (void))

(define (sec:timeline)
  (make-transition-slide
    "Toward Practical Gradual Typing")
  (pslide
    ;; ok you've seen the benefits and the plans
    #:go (coord (+ model-sidebar-x 2/10) 0 'ct) (make-model-sidebar)
    #:go (coord (- perf-sidebar-x 2/10) 0 'ct) (make-perf-sidebar)
    #:go (coord 1/2 8/100 'ct) (add-rectangle-background #:radius 2 #:draw-border? #t #:color white #:x-margin 2 #:y-margin 0 (make-benefits-topbar)))
  (pslide
    #:go checklist-coord
    (make-checklist #:hide? #true full-checklist-data))
  (pslide
    #:go checklist-coord
    (make-checklist full-checklist-data))
  (pslide
    #:go heading-text-coord
    (make-timeline (* 95/100 client-w) (* 86/100 client-h)))
  (pslide
    ;; TODO lying code, behind timeline
    )
  (pslide
    #:go heading-text-coord
    (make-timeline (* 95/100 client-w) (* 86/100 client-h)))
  (pslide
    #:go center-coord
    @st{The End})
  (void))

(define (sec:QA)
  ;; Possible "extra" slides:
  ;; - optimizer
  ;; - why not sampling
  ;; - 
  (pslide
    ;; TODO show code that built a slide, with honest and lying
    )
  (pslide
    #:go (coord -2/100 24/100 'lt #:sep tiny-y-sep)
    (make-research-topic
     "Expressiveness"
     #:background-color "plum"
     (make-long-citation
       "DLS 18"
       #:title "The Behavior of Gradual Types: A User Study"
       #:author* '("Preston Tunnell Wilson" "Ben Greenman" "Justin Pombrio" "Shriram Krishnamurthi"))))
  (pslide
    ;; TODO optimization slides
    )
  #;(pslide
    #:go heading-text-coord
    @st{but, Research can Fail}
    #:go slide-text-coord
    @t{- lack of synergy ... ST too slow}
    @t{- elim. form error messages})
  #;(pslide
    how does cm relate to gtt? conjecture "equal"
    if sat. cm, then can model gtt
    if model, then can prove cm for the semantics)
  (void))

;; =============================================================================

(module+ main
  (do-show))

;; =============================================================================

(define code-underline-size 5)
(define code-highlight-color racket-blue)

(define (make-code-underline pp tag)
  (pin-code-line pp (find-tag pp tag) lb-find (find-tag pp tag) rb-find))

(define (pin-code-line pp src find-src tgt find-tgt #:label [label (blank)] #:color [pre-color #f])
  (pin-line pp src find-src tgt find-tgt #:line-width code-underline-size #:color (or pre-color code-highlight-color) #:label label))

(define (add-hl-arrow pp arr #:style [style 'solid])
  (let* ( #;(pp (make-code-underline pp (program-arrow-src-tag arr)))
          #;(pp (make-code-underline pp (program-arrow-tgt-tag arr))))
    (add-program-arrow pp arr #:arrow-size 22 #:style style #:line-width 5)))

(module+ raco-pict (provide raco-pict) (define raco-pict (add-rectangle-background #:x-margin 40 #:y-margin 40 (begin (blank 800 600)
  (ppict-do (filled-rectangle client-w client-h #:draw-border? #f #:color ice-color)

    #:go heading-text-coord @st{Benefits (3/3): Compatibility}
    #:go benefits-pict-coord (scale (make-benefit-compatibility-pict) 7/10)
    #:go benefits-bar-coord (make-benefits-topbar)
    #:go (coord 1/2 benefits-below-bar-y 'ct #:sep (h%->pixels 7/100))
    #:alt
    [(ht-append
       small-x-sep
       (make-typed-codeblock*
         #:title "A" #:x-margin example-code-x-margin #:y-margin example-code-y-margin
         (list
           @ct{(define stx}
           @ct{  #`#,(vector 0 1))}
           @ct{ }
           @ct{(provide stx)}))
       (make-untyped-codeblock*
         #:title "B" #:x-margin example-code-x-margin #:y-margin example-code-y-margin
         (list
           @ct{(require Library)}
           @ct{ }
           @ct{stx})))
     #:next
     #:alt [stx-compile-txt]
     (vc-append tiny-y-sep stx-compile-txt stx-run-txt)]
    (vc-append
      tiny-y-sep
      @t{Typed Racket provides 203 base types;}
      @t{12 lack runtime support (wrappers)})
    #:next
    #:alt
    [(table 3
       (map tcodesize
            '("(Async-Channel T)"
              "(Custodian-Box T)"
              "(C-Mark-Key T)"
              "(Evt T)"
              "(Ephemeron T)"
              "(Future T)"
              "(MPair T T')"
              "(MList T)"
              "(Prompt-Tag T T')" ;; Asumu SHOULD have fixed this, but appears to be old bug in the implementation --- no tests. Issue #876
              "(Syntax T)"
              "(Thread-Cell T)"
              "(Weak-Box T)"))
       cc-superimpose cc-superimpose small-x-sep small-y-sep)]
    (vc-append
      tiny-y-sep
      (hb-append @bt{Transient} @t{ does not need runtime support,})
      @t{so more code can run})

  )))))
