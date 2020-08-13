#lang typed/racket/base #:transient

;; TODO collect random numbers

(require
  typed/racket/class
  typed/racket/flonum
  racket/list
  (only-in racket/set set-member?))

(require/typed racket/set
  (mutable-set (All (A) (-> A * (Setof A))))
  (set-add! (All (A) (-> (Setof A) A Void)))
  (set-clear! (All (A) (-> (Setof A) Void))))
(require "constants.rkt")
(require "square.rkt")

(define empty-set% : EmptySet%
  (class object%
    (super-new)
    (init (init-board : Board))
    (field (board : Board init-board)
           (empties : (Listof Integer) (range (* SIZE SIZE)))
           (empty_pos : (Listof Integer) (range (* SIZE SIZE))))

    (define/public (random_choice)
      (define choices (length empties))
      (define return-val
        (let loop : (U Void Integer)
                  ()
          (when (< 0 choices)
            (define i (random choices))
            (define pos (list-ref empties i))
            (if (send board useful pos)
              pos
              (let ()
                (set! choices (- choices 1))
                (send this set i (list-ref empties choices))
                (send this set choices pos)
                (loop))))))
      (if (void? return-val)
        PASS
        return-val))

    (define/public (add pos)
      (set-field! empty_pos this (list-set (get-field empty_pos this) pos (length (get-field empties this))))
      ;; bg TODO list-set may go out of bounds
      (set-field! empties this (append (get-field empties this) (list pos))))

    (define/public (remove pos update)
      (when (null? empties)
        (raise-user-error 'remove "empty list ~s" empties))
      (send this set (list-ref empty_pos pos) (list-ref empties (sub1 (length empties))))
      (define pop-empties
        (let loop : (Listof Integer)
                  ((lst (get-field empties this)))
          (if (null? (cdr lst))
            '()
            (cons (car lst) (loop (cdr lst))))))
      (set-field! empties this pop-empties)
      (void))

    (define/public (set i pos)
      (set-field! empties this (list-set (get-field empties this) i pos))
      (set-field! empty_pos this (list-set (get-field empty_pos this) pos i)))))

(define zobristhash% : ZobristHash%
  (class object%
    (super-new)
    (init [init-board : Board])
    (field [board : Board init-board]
           [hash_set : (Setof Integer) (mutable-set)]
           [hash : Integer 0])

    (for ((square (in-list (get-field squares board))))
      (set-field! hash this (bitwise-xor
                              (get-field hash this)
                              (list-ref (get-field zobrist_strings square) EMPTY))))
    (set-clear! (get-field hash_set this))
    (set-add! (get-field hash_set this) (get-field hash this))

    (define/public (update square color)
      (set-field! hash this (bitwise-xor
                              (get-field hash this)
                              (list-ref (get-field zobrist_strings square)
                                        (get-field color square))))
      (set-field! hash this (bitwise-xor
                              (get-field hash this)
                              (list-ref (get-field zobrist_strings square)
                                        color))))

    (define/public (add)
      (set-add! (get-field hash_set this)
                (get-field hash this)))

    (define/public (dupe)
      (set-member? (get-field hash_set this)
                   (get-field hash this)))))

(define board% : Board%
  (class object%
    (super-new)

    (field [squares : (Listof Square) '()]
           [emptyset : EmptySet (make-object empty-set% this)]
           [zobrist : ZobristHash (make-object zobristhash% this)]
           [color : Integer BLACK]
           [finished : Boolean #false]
           [history : (Listof Integer) '()]
           [white_dead : Integer 0]
           [black_dead : Integer 0]
           [lastmove : Integer -2])

    (set-field! squares this (for/list : (Listof Square)
                                       ((pos (in-range (* SIZE SIZE))))
                               (make-object square% this pos)))
    (for ((square (in-list (get-field squares this))))
      (send square set_neighbours)
      (set-field! color square EMPTY)
      (set-field! used square #false))

    (define/public (reset)
      (for ((square (in-list (get-field squares this))))
        (set-field! color square EMPTY)
        (set-field! used square #false))
      (set-field! emptyset this (make-object empty-set% this))
      (set-field! zobrist this (make-object zobristhash% this))
      (set-field! color this BLACK)
      (set-field! finished this #f)
      (set-field! lastmove this -2)
      (set-field! history this '())
      (set-field! white_dead this 0)
      (set-field! black_dead this 0))

    (define/public (move pos)
      (unless (exact-nonnegative-integer? pos)
        (raise-argument-error 'move "natural" pos))
      (define square (list-ref (get-field squares this) pos))
      (void
        (cond
          [(not (= pos PASS))
           (send square move (get-field color this))
           (send (get-field emptyset this) remove (get-field pos square) #true)]
          [(= (get-field lastmove this) PASS)
           (set-field! finished this #true)]))
      (void
        (if (= (get-field color this) BLACK)
          (set-field! color this WHITE)
          (set-field! color this BLACK)))
      (set-field! lastmove this pos)
      (set-field! history this (append (get-field history this) (list pos))))

    (define/public (random_move)
      (send (get-field emptyset this) random_choice))

    (define/public (useful_fast square)
      (if (not (get-field used square))
        (for/or ((neighbour (in-list (get-field neighbours square))))
          (= (get-field color neighbour) EMPTY))
        #false))

    (define/public (useful pos)
      (set-box! *TIMESTAMP (add1 (unbox *TIMESTAMP)))
      (define square (list-ref (get-field squares this) pos))
      (or
        (useful_fast square)
        (let ()
          (define old_hash (get-field hash (get-field zobrist this)))
          (send (get-field zobrist this) update square (get-field color this))
          (define empties 0)
          (define opps 0)
          (define weak_opps 0)
          (define neighs 0)
          (define weak_neighs 0)
          (for ((neighbour (in-list (get-field neighbours square))))
            (define neighcolor (get-field color neighbour))
            (if (= neighcolor EMPTY)
              (set! empties (add1 empties))
              (let ()
                (define neighbour_ref (send neighbour find #f))
                (unless (= (get-field timestamp neighbour_ref)
                           (unbox *TIMESTAMP))
                  (if (= neighcolor (get-field color this))
                    (set! neighs (add1 neighs))
                    (set! opps (add1 opps)))
                  (set-field! timestamp neighbour_ref (unbox *TIMESTAMP))
                  (set-field! temp_ledges neighbour_ref (get-field ledges neighbour_ref)))
                (set-field! temp_ledges neighbour_ref (sub1 (get-field temp_ledges neighbour_ref)))
                (when (= (get-field temp_ledges neighbour_ref) 0)
                  (if (= neighcolor (get-field color this))
                    (set! weak_neighs (add1 weak_neighs))
                    (begin
                      (set! weak_opps (add1 weak_opps))
                      (send neighbour_ref remove neighbour_ref #f)))))))
          (define dupe (send (get-field zobrist this) dupe))
          (set-field! hash (get-field zobrist this) old_hash)
          (define strong_neighs (- neighs weak_neighs))
          (define strong_opps (- opps weak_opps))
          (and (not dupe)
               (or (not (zero? empties))
                   (not (zero? weak_opps))
                   (and (not (zero? strong_neighs))
                        (or (not (zero? strong_opps))
                            (not (zero? weak_neighs)))))))))


    (define/public (useful_moves)
      (for/list ((pos (in-list (get-field empties (get-field emptyset this))))
                 #:when (useful pos))
        pos))

    (define/public (replay history)
      (for ((pos (in-list history)))
        (move pos)))

    (define/public (score color)
      (define count : Flonum
        (if (= color WHITE)
          (fl+ KOMI (->fl (get-field black_dead this)))
          (->fl (get-field white_dead this))))
      (for ((square (in-list (get-field squares this))))
        (define squarecolor (get-field color square))
        (cond
          [(= squarecolor color)
           (set! count (add1 count))]
          [(= squarecolor EMPTY)
           (define surround 0)
           (for ((neighbour (in-list (get-field neighbours square))))
             (when (= (get-field color neighbour) color)
               (set! surround (add1 surround))))
           (when (= surround (length (get-field neighbours square)))
             (set! count (add1 count)))]
          [else (void)]))
      count)

    ;; unused!
    #;(define/public (check)
      (for ((square (in-list (get-field squares this))))
        (unless (= (get-field color square) EMPTY)
          (define members1 (mutable-set square))
          (define changed #true)
          (let loop ()
            (when changed
              (set! changed #f)
              (for ((member (in-list (set->list members1))))
                (for ((neighbour (in-list (get-field neighbours member))))
                  (when (and
                          (= (get-field color neighbour)
                             (get-field color square))
                          (not (set-member? members1 neighbour)))
                    (set! changed #true)
                    (set-add! members1 neighbour))))
              (loop)))
          (define ledges1 0)
          (for ((member (in-set members1)))
            (for ((neighbour (in-list (get-field neighbours member))))
              (when (= (get-field color neighbour) EMPTY)
                (set! ledges1 (add1 ledges1)))))
          (define root (send square find #f))
          (define members2 (mutable-set))
          (for ((square2 (in-list (get-field squares this))))
            (when (and
                    (not (= (get-field color square2) EMPTY))
                    (= (send square2 find #f) root))
              (set-add! members2 square2)))
          (define ledges2 (get-field root ledges))
          (unless (set=? members1 members2)
            (raise-user-error 'assert))
          (unless (= ledges1 ledges2)
            (raise-user-error 'assert (format "ledges differ at ~s: ~s ~s" square ledges1 ledges2)))
          (define empties1 (mutable-set (get-field empties (get-field emptyset this))))
          (define empties2 (mutable-set))
          (for ((square (in-list (get-field squares this))))
            (when (= (get-field color square) EMPTY)
              (set-add! empties2 (get-field pos square)))))))))

(define uctnode% : UCTNode%
  (class object%
    (super-new)
    (field [bestchild : (U #f UCTNode) #f]
           [pos : Integer -1]
           [wins : Natural 0]
           [losses : Natural 0]
           [pos_child : (Listof (U #f UCTNode)) (make-list (* SIZE SIZE) #f)]
           [parent : (U #f UCTNode) #f]
           [unexplored : (Listof Integer) '()])

    (define/public (play board)
      (define color (get-field color board))
      (define node this)
      (define path (list node))
      (let loop ()
        (define pos (send node select board))
        (unless (= pos PASS)
          (send board move pos)
          (define child (list-ref (get-field pos_child node) pos))
          (cond
            [(not child)
             (define new-child (make-object uctnode%))
             (set-field! pos_child node (list-set (get-field pos_child node) pos new-child))
             (set-field! unexplored new-child (send board useful_moves))
             (set-field! pos new-child pos)
             (set-field! parent new-child node)
             (set! path (append path (list new-child)))
             ;; no loop, break
             (void)]
            [else
             (set! path (append path (list child)))
             (set! node child)
             (loop)])))
      (random_playout board)
      (update_path board color path))

    (define/public (select board)
      (cond
        [(not (null? (get-field unexplored this)))
         (define i (random (length (get-field unexplored this))))
         (define pos (list-ref (get-field unexplored this) i))
         (set-field! unexplored this (list-set (get-field unexplored this)
                                               i
                                               (list-ref (get-field unexplored this)
                                                         (sub1 (length (get-field unexplored this))))))
         (set-field! unexplored this (drop-right (get-field unexplored this) 1))
         pos]
        [(get-field bestchild this) => (lambda ((c : UCTNode))
         (get-field pos c))]
        [else
         PASS]))

    (define/public (random_playout board)
      (for ((x (in-range MAXMOVES))
            #:break (get-field finished board))
        (define rm (send board random_move))
        (when (<= 0 rm)
          (send board move rm))))

    (define/public (update_path board color path)
      (define wins (>= (send board score BLACK) (send board score WHITE)))
      (for ((node (in-list path)))
        (define c2
          (if (= color BLACK)
            WHITE
            BLACK))
        (if (eq? wins (= c2 BLACK))
          (set-field! wins node (add1 (get-field wins node)))
          (set-field! losses node (add1 (get-field losses node))))
        (let ((p : (U #f UCTNode) (get-field parent node)))
          (when p
            (set-field! bestchild p (send p best_child))))))

    (define/public (score)
      (define winrate : Flonum
        (fl/ (->fl (get-field wins this))
             (fl+ (->fl (get-field wins this))
                  (->fl (get-field losses this)))))
      (define parentvisits : Flonum
        (fl+ (->fl (get-field wins (or (get-field parent this) (error 'score:parent))))
             (->fl (get-field losses (or (get-field parent this) (error 'score:parent))))))
      (if (fl= 0.0 parentvisits)
        winrate
        (let ((nodevisits (fl+ (->fl (get-field wins this))
                               (->fl (get-field losses this)))))
          (fl+ winrate
               (flsqrt (fl/ (fllog parentvisits)
                            (fl* 5.0 nodevisits)))))))

    (define/public (best_child)
      (define maxscore : Flonum -1.0)
      (define maxchild : (U #f UCTNode) #f)
      (for ((child : (U #f UCTNode) (in-list (get-field pos_child this))))
        (when (and child (> (send child score) maxscore))
          (set! maxchild child)
          (set! maxscore (send child score))))
      (or maxchild
          (raise-user-error 'best_child)))

    (define/public (best_visited)
      (define maxvisits -1)
      (define maxchild : (U #f UCTNode) #f)
      (for ((child : (U #f UCTNode) (in-list (get-field pos_child this))))
        (when (and child (> (+ (get-field wins child) (get-field losses child)) maxvisits))
          (set! maxvisits (+ (get-field wins child) (get-field losses child)))
          (set! maxchild child)))
      (or maxchild
          (raise-user-error 'best_visited)))))

(: computer_move (-> Board Integer))
(define (computer_move board)
  (define pos (send board random_move))
  (cond
    [(= pos PASS)
     PASS]
    [else
      (define tree (make-object uctnode%))
      (set-field! unexplored tree (send board useful_moves))
      (define nboard (make-object board%))
      (for ((game (in-range GAMES)))
        (define node tree)
        (send nboard reset)
        (send nboard replay (get-field history board))
        (send node play nboard))
      (get-field pos (send tree best_visited))]))

(define ITERATIONS 2)

(define (main)
  (random-seed 1)
  (for ((i (in-range ITERATIONS)))
    (define board (make-object board%))
    (define pos (computer_move board))
    pos))

(time (main))

