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
(define racket-red  (hex-triplet->color% #x9F1D20))
(define racket-blue (hex-triplet->color% #x3E5BA9))
(define ice-color (hex-triplet->color% #xF3F1F2))
(define sand-color (hex-triplet->color% #xFFF7C2))
(define cliff-color (hex-triplet->color% #x3A3B27))

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
  (void))

;; =============================================================================

(module+ main
  (do-show))

;; =============================================================================

(define big-landscape-w (* 7/10 client-w))
(define big-landscape-h (* 55/100 client-h))

(define (make-landscape-background w h)
  (define c sand-color)
  (define bc cliff-color)
  (define (draw-box dc dx dy)
    (define old-brush (send dc get-brush))
    (define old-pen (send dc get-pen))
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

(module+ raco-pict (provide raco-pict) (define raco-pict (add-rectangle-background #:x-margin 40 #:y-margin 40 (begin (blank 800 600)
  (ppict-do (filled-rectangle client-w client-h #:draw-border? #f #:color ice-color)
    #:go big-landscape-coord
    hyrule-landscape


  )))))
