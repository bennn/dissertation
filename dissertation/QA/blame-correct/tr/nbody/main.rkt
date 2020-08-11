#lang typed/racket/base #:transient
(require racket/list racket/match typed/racket/flonum)

(define-type (MV T) (Mutable-Vectorof T))
(define-type C-Out
  (Listof
    (List
      (List (Vectorof Flonum) (Vectorof Flonum) Flonum)
      (List (Vectorof Flonum) (Vectorof Flonum) Flonum))))

(: combinations (-> (Listof (List (Vectorof Flonum) (Vectorof Flonum) Flonum)) C-Out))
(define (combinations l)
  (define res : C-Out '())
  (for ((x (in-range (sub1 (length l)))))
    (define ls (drop l (+ x 1)))
    (define l-x (list-ref l x))
    (for ((y (in-list ls)))
      (set! res
        (cons (list l-x y) res))))
  res)

(define PI : Flonum 3.14159265358979323)
(define SOLAR_MASS : Flonum (fl* 4.0 (fl* PI PI)))
(define DAYS_PER_YEAR : Flonum 365.24)

(define BODIES : (Immutable-HashTable String (List (Vectorof Flonum) (Vectorof Flonum) Flonum))
  {hash
    "sun" (list [vector 0.0 0.0 0.0] [vector 0.0 0.0 0.0] SOLAR_MASS)

    "jupiter" (list [vector 4.84143144246472090e+00
                 -1.16032004402742839e+00
                 -1.03622044471123109e-01]
                [vector (fl* 1.66007664274403694e-03 DAYS_PER_YEAR)
                 (fl* 7.69901118419740425e-03 DAYS_PER_YEAR)
                 (fl* -6.90460016972063023e-05 DAYS_PER_YEAR)]
                 (fl* 9.54791938424326609e-04 SOLAR_MASS))

    "saturn" (list [vector 8.34336671824457987e+00
                4.12479856412430479e+00
                -4.03523417114321381e-01]
               [vector (fl* -2.76742510726862411e-03 DAYS_PER_YEAR)
                (fl* 4.99852801234917238e-03 DAYS_PER_YEAR)
                (fl* 2.30417297573763929e-05 DAYS_PER_YEAR)]
                (fl* 2.85885980666130812e-04 SOLAR_MASS))

    "uranus" (list [vector 1.28943695621391310e+01
                -1.51111514016986312e+01
                -2.23307578892655734e-01]
               [vector (fl* 2.96460137564761618e-03 DAYS_PER_YEAR)
                (fl* 2.37847173959480950e-03 DAYS_PER_YEAR)
                (fl* -2.96589568540237556e-05 DAYS_PER_YEAR)]
                (fl* 4.36624404335156298e-05 SOLAR_MASS))

    "neptune" (list [vector 1.53796971148509165e+01
                 -2.59193146099879641e+01
                 1.79258772950371181e-01]
                [vector (fl* 2.68067772490389322e-03 DAYS_PER_YEAR)
                 (fl* 1.62824170038242295e-03 DAYS_PER_YEAR)
                 (fl* -9.51592254519715870e-05 DAYS_PER_YEAR)]
                 (fl* 5.15138902046611451e-05 SOLAR_MASS)) })

(define SYSTEM : (Listof (List (Vectorof Flonum) (Vectorof Flonum) Flonum)) (hash-values BODIES))
(define PAIRS : C-Out (combinations SYSTEM))

(: advance (-> Flonum Integer Void))
(define (advance dt n)
  (define bodies SYSTEM)
  (define pairs PAIRS)
  (for ((i (in-range n)))
    (for ((ppp (in-list pairs)))
      (match-define (list (list (vector x1 y1 z1) v1 m1)
                          (list (vector x2 y2 z2) v2 m2)) ppp)
      (define dx : Flonum (fl- x1 x2))
      (define dy : Flonum (fl- y1 y2))
      (define dz : Flonum (fl- z1 z2))
      (define mag : Flonum (fl* dt (flexpt (fl+ (fl* dx dx) (fl+ (fl* dy dy) (fl* dz dz))) -1.5)))
      (define b1m : Flonum (fl* m1 mag))
      (define b2m : Flonum (fl* m2 mag))
      (vector-set! v1 0 (fl- (vector-ref v1 0) (fl* dx b2m)))
      (vector-set! v1 1 (fl- (vector-ref v1 1) (fl* dy b2m)))
      (vector-set! v1 2 (fl- (vector-ref v1 2) (fl* dz b2m)))
      (vector-set! v2 0 (fl+ (vector-ref v2 0) (fl* dx b1m)))
      (vector-set! v2 1 (fl+ (vector-ref v2 1) (fl* dy b1m)))
      (vector-set! v2 2 (fl+ (vector-ref v2 2) (fl* dz b1m))))
    (for ((bbb (in-list bodies)))
      (match-define (list r (vector vx vy vz) m) bbb)
      (vector-set! r 0 (fl+ (vector-ref r 0) (fl* dt vx)))
      (vector-set! r 1 (fl+ (vector-ref r 1) (fl* dt vy)))
      (vector-set! r 2 (fl+ (vector-ref r 2) (fl* dt vz))))))

(: report_energy (-> Flonum))
(define (report_energy)
  (define bodies SYSTEM)
  (define pairs PAIRS)
  (define e : Flonum 0.0)
  (for ((ppp (in-list pairs)))
    (match-define (list (list (vector x1 y1 z1) v1 m1)
                        (list (vector x2 y2 z2) v2 m2)) ppp)
    (define dx (fl- x1 x2))
    (define dy (fl- y1 y2))
    (define dz (fl- z1 z2))
    (set! e (fl- e (fl/ (fl* m1 m2)
                    (flexpt (fl+ (fl* dx dx) (fl+ (fl* dy dy) (fl* dz dz))) 0.5)))))
  (for ((bbb (in-list bodies)))
    (match-define (list r (vector vx vy vz) m) bbb)
    (set! e (fl+ e (fl/ (fl* m (fl+ (fl* vx vx) (fl+ (fl* vy vy) (fl* vz vz)))) 2.0))))
  e)

(: offset_momentum (-> (List (Vectorof Flonum) (Vectorof Flonum) Flonum) Void))
(define (offset_momentum ref)
  (define bodies SYSTEM)
  (define px : Flonum 0.0)
  (define py : Flonum 0.0)
  (define pz : Flonum 0.0)
  (for ((bbb (in-list bodies)))
    (match-define (list r (vector vx vy vz) m) bbb)
    (set! px (fl- px (fl* vx m)))
    (set! py (fl- py (fl* vy m)))
    (set! pz (fl- pz (fl* vz m))))
  (match-define (list r v m) ref)
  (vector-set! v 0 (fl/ px m))
  (vector-set! v 1 (fl/ py m))
  (vector-set! v 2 (fl/ pz m))
  (void))

(: test_nbody (-> Void))
(define (test_nbody)
  (report_energy)
  (advance 0.01 20000)
  (report_energy)
  (void))

(define (main)
  (offset_momentum (hash-ref BODIES "sun"))
  (test_nbody))

(time (main))

;; goal (report_energy) =
;;  -0.1690751638285245
;;  -0.1690892627552678
