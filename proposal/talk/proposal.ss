#lang at-exp slideshow

;; image-color.com
;; /Users/ben/code/racket/gtp/shallow/gf-icfp-2018/talk/simple.ss
;; /Users/ben/code/racket/gtp/rrc/oopsla-2019/talk/splash.ss

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
  (only-in images/icons/symbol check-icon x-icon)
  images/icons/style
  ;images/icons/arrow images/icons/control images/icons/misc images/icons/symbol images/icons/style
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

;; COLOR
(define black (string->color% "black"))
(define white (string->color% "white"))
(define transparent (color%-update-alpha white 0))
(define program-color (hex-triplet->color% #xABC9CF)) ;; gray/blue
(define racket-red  (hex-triplet->color% #x9F1D20))
(define racket-blue (hex-triplet->color% #x3E5BA9))
(define ice-color (hex-triplet->color% #xF3F1F2))
(define sand-color (hex-triplet->color% #xFFF7C2))
(define stamp-color (hex-triplet->color% #xDCCC90))
(define cliff-color (hex-triplet->color% #x3A3B27))
(define sea-color (hex-triplet->color% #x84CEB3))

(define typed-color   (hex-triplet->color% #xF19C4D)) ;; orange
;; #xE59650 #xEF9036
(define untyped-color (hex-triplet->color% #x697F4D)) ;; dark green
;; #x72875C #x708E6D

(define uni-sound-color (hex-triplet->color% #x666666))
(define tag-sound-color (hex-triplet->color% #x888888))
(define type-sound-color (hex-triplet->color% #xBBBBBB))
(define complete-monitoring-color (hex-triplet->color% #xFFFFFF))

(define (tagof str) (string-append "⌊" str "⌋"))
(define tag-T (tagof "T"))

(define uni-sound-str "Unitype Sound")
(define tag-sound-str "Tag Sound")
(define type-sound-str "Type Sound")
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

(define racket-logo.png (build-path "src" "racket-logo2.png"))

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
(define ct (make-string->code))
(define bigct (make-string->code #:size (- body-size 4)))
(define cbt (make-string->text #:font (cons 'bold code-font) #:size code-size #:color black))
(define ckwdt cbt)
(define ctypet ct) ;; TODO pict type color (make-string->text #:font (cons 'bold code-font) #:size code-size #:color racket-blue))
(define tcodesize (make-string->body #:size code-size))
(define tsmaller (make-string->body #:size (- body-size 4)))
(define lang-text (make-string->code #:size 32))
(define blang-text (make-string->text #:font (cons 'bold code-font) #:size 32 #:color black))
(define captiont (make-string->body #:size caption-size #:color black))
(define (ownership-text str #:color [color black]) ((make-string->text #:font "PilGi" #:size 40 #:color color) str))
(define huge-t (make-string->body #:size 80))
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

(define program-w (w%->pixels 35/100))
(define program-h (h%->pixels 45/100))

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
  (cc-superimpose (make-big-landscape-background) (inset/clip (bitmap "src/hyrule.png") -1 -6)))

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
    ;; TODO better way to draw curves
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
  (for/fold ((acc (make-big-landscape-background)))
            ((xy (in-list '((89/100 07/100) (99/100 11/100) (73/100 11/100)
                            (83/100 18/100) (94/100 26/100) (62/100 12/100)
                            (52/100 22/100) (42/100 13/100) (32/100 20/100)
                            (22/100 11/100) (12/100 30/100) (09/100 12/100)
                            (80/100 31/100) (65/100 40/100) (43/100 38/100)
                            (27/100 40/100) (08/100 46/100) (87/100 51/100)
                            (71/100 60/100) (49/100 52/100) (24/100 56/100)
                            (12/100 62/100) (92/100 65/100) (75/100 75/100)
                            (46/100 71/100) (20/100 77/100) (30/100 73/100)
                            (87/100 76/100) (65/100 89/100) (49/100 87/100)
                            (35/100 85/100) (11/100 87/100) (93/100 89/100)
                            (59/100 67/100)
                            )))
             (i (in-naturals)))
    (ppict-do acc
      #:go (coord (car xy) (cadr xy) 'cc)
      (make-simple-flag the-flag-base
        #:flag-background-color (rgb-triplet->color% (->pen-color i))
        #:flag-brush-style 'horizontal-hatch
        #:flag-border-color (rgb-triplet->color% (->pen-color i))
        #:pole-height 70))))

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

(define PLT-pict
  (vl-append 2 (bitmap "src/racket-small.png") (blank)))

(define neu-pict
  (bitmap "src/neu-small.png"))

(define meeting-of-the-waters
  (bitmap "src/meeting-of-the-waters.jpg"))

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
      (blank 0 14)
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

(define (make-tu-icon str #:font-size [pre-font-size #f] #:width [pre-w #f] #:height [pre-h #f])
  (define font-size (or pre-font-size tu-size))
  (define w (or pre-w 70))
  (define h (or pre-h 70))
  (define str-pict (clip-descent (text str `(bold . ,tu-font) font-size)))
  (cc-superimpose (blank w h) str-pict))

(define U-node (make-untyped-pict (make-untyped-icon)))
(define T-node (make-typed-pict (make-typed-icon)))

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
  ;; TODO use a dc, draw more of a triangle with border?
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

(define reuse-pict
  (make-big-tu-pict
    (tag-pict ocaml-pict ocaml-tag)
    (bitmap "src/opengl-logo-small.png")))

(define (make-sample-program untyped? arrow? [scale-by 8/10])
  (scale
    (make-program-pict
      #:bg-color white #:frame-color black
      (make-tree program-w program-h (if untyped? untyped-program-code* mixed-program-code*) #:arrows? arrow?))
    scale-by))

(define (make-bubble pp)
  (add-rounded-border
    #:radius 6 #:frame-width 4
    #:x-margin small-x-sep #:y-margin small-y-sep
    pp))

(define (add-cloud-background pp #:color [pre-color #f] #:x-margin [pre-x-margin #f] #:y-margin [pre-y-margin #f] #:style [pre-style #f])
  (define x-margin (or pre-x-margin pico-x-sep))
  (define y-margin (or pre-y-margin pico-y-sep))
  (cc-superimpose
    (cloud (+ x-margin (pict-width pp)) (+ y-margin (pict-height pp)) (or pre-color "gray") #:style (or pre-style '(square wide)))
    pp))

(define (st-cloud str)
  (add-cloud-background
    #:x-margin small-x-sep #:y-margin med-y-sep #:color sea-color (st str)))

(define (make-uni-sound-bubble pp)
  (make-property-bubble uni-sound-str uni-sound-color pp))

(define (make-tag-sound-bubble pp)
  (make-property-bubble tag-sound-str tag-sound-color pp))

(define (make-type-sound-bubble pp)
  (make-property-bubble type-sound-str type-sound-color pp))

(define (make-complete-monitoring-bubble pp)
  (make-property-bubble complete-monitoring-str complete-monitoring-color pp))

(define (make-property-bubble lbl-str color pp)
  (define lbl-pict (make-region-label (t lbl-str)))
  (define txt-pict (if pp (vl-append (* 10/100 (pict-height lbl-pict)) (blank) pp) (blank)))
  (define bg-pict
    (filled-rounded-rectangle
      (* (if pp 95/100 45/100) client-w)
      (* 2 (pict-height lbl-pict))
      4
      #:color color
      #:draw-border? #true
      #:border-color black
      #:border-width region-border-width))
  (ppict-do bg-pict
    #:go (coord 0 10/100 'lt #:abs-x 10) (ht-append tiny-x-sep lbl-pict txt-pict)))

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

;; =============================================================================

(define (do-show)
  (set-page-numbers-visible! #true)
  (set-spotlight-style! #:size 60 #:color (color%-update-alpha highlight-brush-color 0.6))
  ;; --
;;  (sec:outline)
;  (sec:title)
  (parameterize ([current-slide-assembler (slide-assembler/background (current-slide-assembler) #:color ice-color)])
    (void)
;    (sec:the-question)
    (sec:design-space)
    (void)))

;; -----------------------------------------------------------------------------
;; title

(define (sec:title)
  (pslide
    #:go center-coord meeting-of-the-waters
    #:go (coord 1/2 20/100 'ct)
    (vr-append
      tiny-y-sep
      (add-landscape-background
        #:x-margin small-x-sep
        #:y-margin tiny-y-sep
        (vc-append
          tiny-y-sep
          @titlet{Honest and Lying Types}
          @t{Thesis Proposal}))
      (add-landscape-background
        (vc-append
          tiny-y-sep
          @t{Ben Greenman}
          @t{2019-1X-XX}))))
  (void))

(define mt-code-y 80/100)

(define motivation-x-l 22/100)
(define motivation-x-r 78/100)
(define motivation-y 18/100)

(define (sec:the-question)
  (pslide
    #:go (coord slide-text-left 10/100 'lt)
    (make-thesis-question #t))
  (pslide
    #:go heading-text-coord
    @st{Migratory Typing}
    #:next
    #:go (coord slide-text-left 14/100 'lt)
    @t{Add types to a dynamically-typed language}
    #:go (coord 75/100 mt-code-y 'cb)
    (add-caption "Untyped/Typed mix" (make-sample-program #f #f))
    #:alt
    [#:go (coord 25/100 mt-code-y 'cb)
     (add-caption "Untyped code" (make-sample-program #t #f))
     #:go center-coord (make-migration-arrow)]
    #:next
    #:go (coord (+ 2/100 slide-text-left) mt-code-y 'lb)
    (vl-append
      med-y-sep
      (vl-append small-y-sep (blank) (hc-append tiny-x-sep U-node @t{= untyped code}))
      (hc-append tiny-x-sep T-node (hc-append @t{= } (tag-pict @t{simply-typed} 't-line)))
      (vl-append small-y-sep (blank) @t{(no 'Dynamic' type)})))
  (pslide
    #:go heading-text-coord
    @st{Motivations}
    #:go (coord 1/2 20/100 'ct)
    (filled-rectangle 1 (* 90/100 client-h) #:color black #:draw-border? #f)
    #:go (coord 1/2 13/100 'ct)
    (make-sample-program #f #f 5/10)
    #:go (coord motivation-x-l motivation-y 'ct #:sep small-y-sep)
    @t{Prototyping}
    scripting-pict
    (make-illustration-text
      @t{write untyped code,}
      @t{rely on types})
    #:go (coord motivation-x-r motivation-y 'ct #:sep small-y-sep)
    @t{Re-Use}
    reuse-pict
    (make-illustration-text
      @t{write typed code,}
      @t{use old libraries}))
  (pslide
    ;; TODO fill margin with language logos? ruby, js, etc...
    #:go heading-text-coord
    @st{Landscape of Models and Implementations}
    #:go big-landscape-coord
    (make-implementation-landscape)
    #:next
    #:go (coord 1/2 35/100 'ct)
    (make-bubble (hb-append @st{Rich design space, due to a challenge})))
  (pslide
    #:go heading-text-coord
    @st{Challenge = Interoperability}
    #:go (coord slide-text-left 20/100 'lt)
    (make-sample-program #f #t 9/10)
    #:go (coord 48/100 20/100 'lt #:sep tiny-x-sep)
    @t{How do types restrict}
    (hb-append @t{ interactions between})
    (hb-append @t{ } @bt{untyped data})
    (hb-append @t{ and } @bt{typed data} @t{?}))
  (void))

(define (sec:design-space)
  (pslide
    ;; been studying landscape from 2 directions: guarantees and perf
    ;; TODO talk about method for studying?
    ;;  ... guarantees / perf via different semantics for one language
    ;;  ... maybe doesn't need illustration --- big question is whether N/A matter later
    #:go big-landscape-coord
    (make-big-landscape-background)
    #:go (coord 1/10 1/10 'lt)
    (st-cloud "Guarantees?")
    #:go (coord 9/10 1/10 'rt)
    (st-cloud "Performance?"))
  (pslide
    #:go heading-text-coord @st{Landscape of Formal Guarantees}
    #:alt [#:go big-landscape-coord (make-big-landscape-background)]
    #:go big-landscape-coord (make-theorem-landscape))
  (pslide
    #:go (coord slide-left slide-top 'lt #:sep pico-y-sep)
    (make-complete-monitoring-bubble
      @t{types predict behavior})
    (make-type-sound-bubble
      (descr-append
        @t{types predict behavior in typed}
        @t{code, nothing in untyped code}))
    (make-tag-sound-bubble
      (descr-append
        @t{types predict shapes in typed}
        @t{code, nothing in untyped code}))
    (make-uni-sound-bubble
      @t{types predict nothing}))
  (pslide
    #:go (coord slide-left slide-top 'lt #:sep pico-y-sep)
    (tag-pict (make-complete-monitoring-bubble #f) complete-monitoring-tag)
    (tag-pict (make-type-sound-bubble #f) type-sound-tag)
    (tag-pict (make-tag-sound-bubble #f) tag-sound-tag)
    (tag-pict (make-uni-sound-bubble #f) uni-sound-tag)
    #:next
    #:go (at-find-pict complete-monitoring-tag rt-find 'lt #:abs-x judgment-x-sep #:abs-y judgment-y-sep) @st{Honest}
    #:go (at-judgment-bar complete-monitoring-tag) (judgment-bar "green")
    #:next
    #:go (at-find-pict type-sound-tag rb-find 'lc #:abs-x judgment-x-sep) @st{Lying}
    #:go (at-judgment-bar tag-sound-tag) (judgment-bar "red")
    #:next
    #:go (at-find-pict uni-sound-tag rt-find 'lt #:abs-x judgment-x-sep #:abs-y judgment-y-sep) @st{Absent})
  (pslide
    #:go heading-text-coord
    @st{Landscape of Runtime Overhead}
    #:go big-landscape-coord
    #:alt [(make-big-landscape-background)]
    (make-performance-landscape))
  (pslide
    ;; perf: transient vs natural
    )
  (void))

;; -----------------------------------------------------------------------------

(define (sec:outline)
  (pslide
    @t{Outline: Honest and Lying Types}
    @t{Thesis Proposal}
    @t{Ben Greenman})
  (pslide
    ;; this is a thesis proposal, so lets start with the thesis question
    ;;  and then fill in background
    ;; ... 3 things to explain
    ;; First, migratory typing
    @t{Thesis question:}
    @t{Does migratory typing benefit from a combination of honest and lying types?})
  (pslide
    ;; use OOPSLA images
    @t{Migratory typing adds static types to an existing language}
    @t{dynamically-typed lang. -> mixed-typed lang.}
    @t{Interoperability problem})
  (pslide
    ;; emphasize the question
    @t{typed values and untyped values flow across boundaries ...}
    @t{... how do types control the interactions?})
  (pslide
    ;; many answers via implementations
    ;; use OOPSLA flag slide
    @t{Many Implementations}
    )
  (pslide
    ;; many answers via models
    ;; use OOPSLA flag slide 2
    @t{Many Models})
  (pslide
    ;; "blank" landscape
    ;;  pretty sure we WANT mixed-typed over migratory ... more accurate and people will get the idea
    @t{Landscape of Mixed-Typed Languages})
  (pslide
    ;; thanks to my past work, have much richer understanding
    ;;  may want concentric circles instead of boundaries, to be clearer tag-S => type-S etc
    ;;  maybe a hill of properties? up from Uni-sound to type-sound+CM ?
    ;; TODO need a new picture to work toward, based on Leif + Michael's feedback
    ;;  they're upset with the "subset" and the dots not looking like flag-models
    ;;  and the properties being 1-1 with dots
    @t{Prior Work: Characterizing the Landscape (2 Parts)})
  (pslide
    ;; want text alongside a landscape picture
    ;; - (erasure) ignore types
    ;; - (transient) tag-check boundaries + elim forms
    ;; ;; - (amnesic) wrappers for tag checks (MAYBE NOT, really its a proof artifact)
    ;; - (forgetful) wrappers, drop extras
    ;; - (conatural) late-check all things
    ;; - (natural) some eager some late
    ;; all for common surface lang, run same program different ways
    @t{Part 1: Comparable Models})
  (pslide
    ;; uni tag type CM ... thats all, right?
    @t{Part 2: Formal Characterization})
  (pslide
    ;; on LHS types don't mean anything; everywhere else have typed code no undef ops
    ;; so lets shrink our focus
    @t{Remove Erasure})
  (pslide
    ;; full landscape again ... draw weak -> strong properties axis
    ;; recall thesis question; RHS = honest "what I'm choosing to call honest types"
    @t{Honest Types})
  (pslide
    ;; full landscape
    ;; everything else = lying
    @t{Lying Types})
  (pslide
    ;; now have basic understanding of thesis question
    ;; but honest types apparently clear winner
    @t{Does migratory typing benefit from a combination of honest any lying types?}
    @t{in particular, transient})
  (pslide
    ;; change views, instead of looking at properties need to look at performance
    ;; again common model pays off
    ;; have Natural (TR) and prototype Transient
    ;; TODO need something that's not flags ... gee should models / impls be different things instead of both flags?
    @t{Performance Characterization})
  (pslide
    ;; 
    @t{Transient perf ~ linear with types})
  (pslide
    @t{Natural perf ~ maybe exponential, depends on boundaries})
  (pslide
    @t{See now why question is compelling, complementary strenghts})
  (pslide
    @t{Potential Benefits}
    @t{- lib T -> S, better overall}
    @t{- user T -> S, better perf while mixed}
    @t{- user S -> T, better perf near top}
    @t{- user T -> S, run more programs}
    @t{- user S -> T, debug incorrect program})
  (pslide
    @t{Challenges}
    @t{- model interactions, suggest costs}
    @t{- implementation}
    @t{- evaluation})
  (pslide
    ;; research is when it can fail (amnesic hard, need use-site types)
    @t{Alternative: forgetful})
  (pslide
    ;; easy, see document
    @t{Timeline})
  ;; ---
  ;; Sam questions:
  ;; - please explain all optimizations that do not apply
  ;; - run on Pycket, to compare to other transient work?
  ;; - get benchmarks where some contracts necessary
  ;; - why not use approx-D method?
  ;; Jan questions:
  ;; - drop static analysis? what can you do in the timeframe???
  ;; - 
  (void))

;; =============================================================================

(module+ main
  (do-show))

;; =============================================================================

(module+ raco-pict (provide raco-pict) (define raco-pict (add-rectangle-background #:x-margin 40 #:y-margin 40 (begin (blank 800 600)
  (ppict-do (filled-rectangle client-w client-h #:draw-border? #f #:color ice-color)

  )))))
