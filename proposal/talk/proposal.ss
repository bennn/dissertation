#lang at-exp slideshow

;; /Users/ben/code/racket/gtp/shallow/gf-icfp-2018/talk/simple.ss
;; /Users/ben/code/racket/gtp/rrc/oopsla-2019/talk/splash.ss

(require
  file/glob
  pict pict/convert pict/balloon pict/face
  (prefix-in pict: pict/shadow)
  pict-abbrevs pict-abbrevs/slideshow gtp-pict
  ppict/2
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
(define racket-red  (hex-triplet->color% #x9F1D20))
(define racket-blue (hex-triplet->color% #x3E5BA9))
(define ice-color (hex-triplet->color% #xF3F1F2))

(define typed-color   (hex-triplet->color% #xF19C4D)) ;; orange
;; #xE59650 #xEF9036
(define untyped-color (hex-triplet->color% #x697F4D)) ;; dark green
;; #x72875C #x708E6D

(define racket-logo.png (build-path "src" "racket-logo2.png"))

(define region-border-width 5)

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

(define ((make-string->text #:font font #:size size #:color color) str)
  (colorize (text str font size) color))

(define (make-string->title #:size [size title-size] #:color [color black])
  (make-string->text #:font title-font #:size size #:color color))

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

(define tau-str "τ")
(define (tagof str)
  (string-append "⌊" str "⌋"))

(define U-sound-str "Uni sound")
(define tag-sound-str (string-append (tagof "T") " sound"))
(define full-sound-str "T sound")
(define complete-monitoring-str "Complete monitoring")
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
;;
;;(define (make-bullet) (disk 11))
;;
;;(struct program-arrow [src-tag src-find tgt-tag tgt-find start-angle end-angle start-pull end-pull color] #:transparent)
;;
;;(define (add-landscape-line pp arrow)
;;  (add-program-arrow pp arrow #:hide? #true #:style 'solid #:line-width 4))
;;
;;(define (add-program-arrow pp arrow #:arrow-size [arrow-size 12] #:line-width [pre-line-width #f] #:style [style 'short-dash] #:label [label (blank)] #:hide? [hide? #false])
;;  (define line-width (or pre-line-width 3))
;;  (pin-arrow-line
;;    arrow-size pp
;;    (find-tag pp (program-arrow-src-tag arrow))
;;    (program-arrow-src-find arrow)
;;    (find-tag pp (program-arrow-tgt-tag arrow))
;;    (program-arrow-tgt-find arrow)
;;    #:line-width line-width
;;    #:label label
;;    #:hide-arrowhead? hide?
;;    #:style style
;;    #:start-angle (program-arrow-start-angle arrow)
;;    #:end-angle (program-arrow-end-angle arrow)
;;    #:start-pull (program-arrow-start-pull arrow)
;;    #:end-pull (program-arrow-end-pull arrow)
;;    #:color (program-arrow-color arrow)))


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

;; =============================================================================

(define (do-show)
  (set-page-numbers-visible! #true)
  (set-spotlight-style! #:size 60 #:color (color%-update-alpha highlight-brush-color 0.6))
  ;; --
  (sec:outline)
;  (sec:title)
  (parameterize ([current-slide-assembler (slide-assembler/background (current-slide-assembler) #:color ice-color)])
    (void)
    ;(sec:migration)
    ;(sec:TS)
    ;(sec:plot)
    ;(sec:CM)
    ;(sec:BSBC)
    ;(sec:takeaways)
    ;(sec:extra)
    (void)))

;; -----------------------------------------------------------------------------
;; title

;(define neu 'Northeastern)
;(define nwu 'Northwestern)
;
;(define PLT-pict
;  (vl-append 2 (bitmap "src/racket-small.png") (blank)))
;
;(define neu-pict
;  (bitmap "src/neu-small.png"))
;
;(define nwu-pict
;  (hc-append 41 (blank) (bitmap "src/nwu-small.png")))
;
;(define island-tag 'island)
;
;(define island-pict (tag-pict (bitmap (build-path "src" "island.png")) island-tag))
;
;(define map-bg-pict (cellophane island-pict 0.15))
;
;(define the-title-pict
;  (tag-pict
;    (vl-append
;      (h%->pixels 4/100)
;      @titlet[@string-upcase{Complete Monitors}]
;      @titlet[@string-upcase{for Gradual Types}]) 'title))
;
;(define authors-pict
;  (let ()
;    ;; TODO actual NEU NWU fonts?
;    (define base
;      (ht-append
;        pico-x-sep
;        (vl-append (w%->pixels 12/1000) (blank) (make-compass-pict 24))
;        (vl-append
;          (h%->pixels 8/100)
;          (tag-pict @sst{Ben Greenman} 'ben)
;          (tag-pict @sst{Matthias Felleisen} 'matthias)
;          (tag-pict @sst{Christos Dimoulas} 'christos))))
;    (for/fold ((acc base))
;              ((name (in-list '(ben matthias christos)))
;               (uni (in-list (list neu-pict neu-pict nwu-pict))))
;      (ppict-do acc
;                #:go (at-find-pict name lb-find 'lt #:abs-x pico-x-sep)
;                (tag-pict (hb-append PLT-pict ((make-string->body #:size (- body-size 4)) " at ")) 'aff)
;                #:go (at-find-pict 'aff rc-find 'lc #:abs-x -36 #:abs-y 2) uni))))
;
;(define (sec:title)
;  (let ((the-y 46/100))
;  (pslide
;    #:go center-coord map-bg-pict
;    #:go (coord slide-text-left 18/100 'lt) the-title-pict
;    #:go (coord slide-text-right the-y 'rt) authors-pict
;    #:next
;    #:go (coord 7/100 the-y 'lt)
;    (make-legend
;      @small-titlet{a careful analysis}
;      @small-titlet{of the mixed-typed}
;      @small-titlet{design space})))
;  (void))
;
;;; -----------------------------------------------------------------------------
;;; migration
;
;(define program-blank (blank program-w program-h))
;(define u-tag 'L)
;(define t-tag 'R)
;(define macro-L
;  (add-caption
;    "Untyped only"
;    (tag-pict (make-program-pict #:bg-color transparent #:frame-color transparent program-blank) u-tag)))
;(define macro-R
;  (add-caption
;    "Untyped/Typed mix"
;    (tag-pict (make-program-pict #:bg-color white #:frame-color black program-blank) t-tag)))
;(define abstract-L
;  (make-untyped-icon #:font-size big-node-size #:width program-w #:height program-h))
;(define abstract-R
;  (hc-append
;    (make-untyped-icon #:font-size big-node-size)
;    (make-tu-icon "+" #:font-size big-node-size)
;    (make-typed-icon #:font-size big-node-size)))
;(define concrete-L
;  (make-tree program-w program-h untyped-program-code* #:arrows? #false))
;(define concrete-R
;  (make-program-pict #:bg-color white #:frame-color black (make-tree program-w program-h mixed-program-code* #:arrows? #true)))
;
;(define sample-mixed-program
;  (make-program-pict #:bg-color white #:frame-color black (make-tree program-w program-h mixed-program-code* #:arrows? #true)))
;
;(define scripting-pict
;  (make-big-ut-pict
;    (lightbulb
;      #:border-width 1
;      #:bulb-radius 45
;      #:stem-width-radians (* 1/10 turn)
;      #:stem-height 12)
;    (bitmap "src/parthenon-logo.png")))
;

;; -----------------------------------------------------------------------------

(define (sec:outline)
  (pslide
    @t{Outline: Deep and Shallow Types})
  (pslide
    @t{Migratory Typing})
  (pslide
    @t{Diversity, what do types predict?})
  (pslide
    @t{Rule out Erasure})
  (pslide
    @t{Goal: combine})
  (pslide
    @t{Deep Shallow definitions})
  (pslide
    @t{Deep Shallow contrast})
  (pslide
    @t{Potential Benefits})
  (pslide
    @t{Challenges})
  (pslide
    @t{Timeline})
  (void))

;; =============================================================================

(module+ main
  (do-show))

;; =============================================================================

(module+ raco-pict (provide raco-pict) (define raco-pict (add-rectangle-background #:x-margin 40 #:y-margin 40 (begin (blank 800 600)
  (ppict-do (filled-rectangle client-w client-h #:draw-border? #f #:color ice-color)


  )))))
