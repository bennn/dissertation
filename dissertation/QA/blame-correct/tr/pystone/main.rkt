#lang typed/racket/base #:transient

(require typed/racket/class racket/vector)

(define LOOPS 50000)

(define Ident1 1)
(define Ident2 2)
(define Ident3 3)
(define Ident4 4)
(define Ident5 5)
(define Ident6 6)

(define-type (MV T) (Mutable-Vectorof T))

(define-type PSRecord%
  (Class
    (field [StringComp String]
           [IntComp Integer]
           [Discr Integer]
           [EnumComp Integer]
           [PtrComp (U #f PSRecord)]) ;; Dyn in Retic
    (copy (-> PSRecord))))

(define-type PSRecord
  (Instance PSRecord%))

(define psrecord% : PSRecord%
  (class object%
    (super-new)
    (field [PtrComp : (U #f PSRecord) #f]
           [Discr : Integer 0]
           [EnumComp : Integer 0]
           [IntComp : Integer 0]
           [StringComp : String ""])

    (define/public (copy) : PSRecord
      (define p (make-object psrecord%))
      (set-field! PtrComp p (get-field PtrComp this))
      (set-field! Discr p (get-field Discr this))
      (set-field! EnumComp p (get-field EnumComp this))
      (set-field! IntComp p (get-field IntComp this))
      (set-field! StringComp p (get-field StringComp this))
      p)))

(define TRUE 1)
(define FALSE 0)

(define IntGlob 0)
(define BoolGlob FALSE)
(define Char1Glob #\0)
(define Char2Glob #\0)
(define Array1Glob : (MV Integer) (make-vector 51 0))
(define Array2Glob : (MV (MV Integer))
  (build-vector 51 (lambda (_) ((inst vector-copy Integer) Array1Glob))))
(define PtrGlb : (U #f PSRecord) (make-object psrecord%))
(define PtrGlbNext (make-object psrecord%))

(: Proc1 (-> PSRecord PSRecord))
(define (Proc1 PtrParIn)
  (define NextPSRecord (send (or PtrGlb (error 'Proc1:PtrGlb)) copy))
  (set-field! PtrComp PtrParIn NextPSRecord)
  (set-field! IntComp PtrParIn 5)
  (set-field! IntComp NextPSRecord (get-field IntComp PtrParIn))
  (set-field! PtrComp NextPSRecord (get-field PtrComp PtrParIn))
  (set-field! PtrComp NextPSRecord (Proc3 (or (get-field PtrComp NextPSRecord) (error 'Proc1:NextPS))))
  (cond
    [(= (get-field Discr NextPSRecord) Ident1)
     (set-field! IntComp NextPSRecord 6)
     (set-field! EnumComp NextPSRecord (Proc6 (get-field EnumComp PtrParIn)))
     (set-field! PtrComp NextPSRecord (get-field PtrComp (or PtrGlb (error 'Proc1:PtrGlb))))
     (set-field! IntComp NextPSRecord (Proc7 (get-field IntComp NextPSRecord) 10))]
    [else
      (set! PtrParIn (send NextPSRecord copy))])
  (set-field! PtrComp NextPSRecord #f)
  PtrParIn)

(: Proc2 (-> Integer Integer))
(define (Proc2 IntParIO)
  (define IntLoc (+ IntParIO 10))
  (define EnumLoc 0)
  (let loop ()
    (when (eq? Char1Glob #\A)
      (set! IntLoc (sub1 IntLoc))
      (set! IntParIO (- IntLoc IntGlob))
      (set! EnumLoc Ident1))
    (unless (= EnumLoc Ident1)
      (loop)))
  IntParIO)

(: Proc3 (-> PSRecord PSRecord))
(define (Proc3 PtrParOut)
  (if PtrGlb
    (set! PtrParOut (or (get-field PtrComp (or PtrGlb (error 'Proc3:PtrGlb))) (error 'Proc3:PtrComp)))
    (set! IntGlob 100))
  (set-field! IntComp (or PtrGlb (error 'Proc3:PtrGlb)) (Proc7 10 IntGlob))
  PtrParOut)

(: Proc4 (-> Void))
(define (Proc4)
  (define BoolLoc : (U Boolean Integer) (eq? Char1Glob #\A))
  (set! BoolLoc (or BoolLoc BoolGlob))
  (set! Char2Glob #\B))

(: Proc5 (-> Void))
(define (Proc5)
  (set! Char1Glob #\A)
  (set! BoolGlob FALSE))

(: Proc6 (-> Integer Integer))
(define (Proc6 EnumParIn)
  (define EnumParOut EnumParIn)
  (when (not (Func3 EnumParIn))
    (set! EnumParOut Ident4))
  (cond
    [(= EnumParIn Ident1)
     (set! EnumParOut Ident1)]
    [(= EnumParIn Ident2)
     (if (> IntGlob 100)
       (set! EnumParOut Ident1)
       (set! EnumParOut Ident4))]
    [(= EnumParIn Ident3)
     (set! EnumParOut Ident2)]
    [(= EnumParIn Ident4)
     (void)]
    [(= EnumParIn Ident5)
     (set! EnumParOut Ident3)])
  EnumParOut)

(: Proc7 (-> Integer Integer Integer))
(define (Proc7 IntParI1 IntParI2)
  (define IntLoc (+ IntParI1 2))
  (define IntParOut (+ IntParI2 IntLoc))
  IntParOut)

(: Proc8 (-> (MV Integer) (MV (MV Integer)) Integer Integer Void))
(define (Proc8 Array1Par Array2Par IntParI1 IntParI2)
  (define IntLoc (+ IntParI1 5))
  (vector-set! Array1Par IntLoc IntParI2)
  (vector-set! Array1Par (+ IntLoc 1) (vector-ref Array1Par IntLoc))
  (vector-set! Array1Par (+ IntLoc 30) IntLoc)
  (for ((IntIndex (in-range IntLoc (+ IntLoc 2))))
    (vector-set! (vector-ref Array2Par IntLoc) IntIndex IntLoc))
  (vector-set! (vector-ref Array2Par IntLoc) (sub1 IntLoc) (add1 (vector-ref (vector-ref Array2Par IntLoc) (sub1 IntLoc))))
  (vector-set! (vector-ref Array2Par (+ IntLoc 20)) IntLoc (vector-ref Array1Par IntLoc))
  (set! IntGlob 5))

(: Func1 (-> Char Char Integer))
(define (Func1 CharPar1 CharPar2)
  (define CharLoc1 CharPar1)
  (define CharLoc2 CharLoc1)
  (if (not (eq? CharLoc2 CharPar2))
    Ident1
    Ident2))

(: Func2 (-> String String Integer))
(define (Func2 StrParI1 StrParI2)
  (define IntLoc 1)
  (define CharLoc : Char
    (let ((CharLoc : (U #f Char) #f))
      (let loop ()
        (when (<= IntLoc 1)
          (when (= (Func1 (string-ref StrParI1 IntLoc) (string-ref StrParI2 (add1 IntLoc)))
                   Ident1)
            (set! CharLoc #\A)
            (set! IntLoc (add1 IntLoc)))
          (loop)))
      (or CharLoc (error 'Func2:CharLoc))))
  (when (and (char>=? CharLoc #\W)
             (char<=? CharLoc #\Z))
    (set! IntLoc 7))
  (if (eq? CharLoc #\X)
    TRUE
    (if (string>? StrParI1 StrParI2)
      (begin (set! IntLoc (+ IntLoc 7))
             TRUE)
      FALSE)))

(: Func3 (-> Integer Integer))
(define (Func3 EnumParIn)
  (define EnumLoc EnumParIn)
  (if (= EnumLoc Ident3)
    TRUE
    FALSE))

(define (main)
  (set! PtrGlbNext (make-object psrecord%))
  (set! PtrGlb (make-object psrecord%))
  (set-field! PtrComp (or PtrGlb (error 'main:PtrGlb)) PtrGlbNext)
  (set-field! Discr (or PtrGlb (error 'main:PtrGlb)) Ident1)
  (set-field! EnumComp (or PtrGlb (error 'main:PtrGlb)) Ident3)
  (set-field! IntComp (or PtrGlb (error 'main:PtrGlb)) 40)
  (set-field! StringComp (or PtrGlb (error 'main:PtrGlb)) "DHRYSTONE PROGRAM, SOME STRING")
  (define String1Loc "DHRYSTONE PROGRAM, 1'ST STRING")
  (vector-set! (vector-ref Array2Glob 8) 7 10)
  (for ((i (in-range LOOPS)))
    (Proc5)
    (Proc4)
    (define IntLoc1 2)
    (define IntLoc2 3)
    (define String2Loc "DHRYSTONE PROGRAM, 2'ND STRING")
    (define EnumLoc Ident2)
    (set! BoolGlob (if (zero? (Func2 String1Loc String2Loc)) 1 0))
    (define IntLoc3
      (let loop : Integer ([IntLoc3 0])
        (cond
          [(< IntLoc1 IntLoc2)
           (set! IntLoc3 (- (* 5 IntLoc1) IntLoc2))
           (set! IntLoc3 (Proc7 IntLoc1 IntLoc2))
           (set! IntLoc1 (add1 IntLoc1))
           (loop IntLoc3)]
          [else
           IntLoc3])))
    (Proc8 Array1Glob Array2Glob IntLoc1 IntLoc3)
    (set! PtrGlb (Proc1 (or PtrGlb (error 'main:PtrGlb))))
    (define CharIndex #\A)
    (let loop ()
      (when (char<=? CharIndex Char2Glob)
        (when (= EnumLoc (Func1 CharIndex #\C))
          (set! EnumLoc (Proc6 Ident1)))
        (set! CharIndex (integer->char (+ (char->integer CharIndex) 1)))
        (loop)))
    (set! IntLoc3 (* IntLoc2 IntLoc1))
    (set! IntLoc2 (quotient IntLoc3 IntLoc1))
    (set! IntLoc2 (- (* 7 (- IntLoc3 IntLoc2)) IntLoc1))
    (set! IntLoc1 (Proc2 IntLoc1))))

(time (main))

