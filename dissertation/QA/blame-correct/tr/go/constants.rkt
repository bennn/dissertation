#lang typed/racket/base #:transient

(provide
  SIZE
  GAMES
  KOMI
  EMPTY
  WHITE
  BLACK
  SHOW
  PASS
  MAXMOVES
  TIMESTAMP
  MOVES

  Square%
  Square
  EmptySet%
  EmptySet
  ZobristHash%
  ZobristHash
  UCTNode%
  UCTNode
  Board%
  Board)

(require typed/racket/class)

(define SIZE 9)
(define GAMES 200)
(define KOMI 7.5)
(define EMPTY 0)
(define WHITE 1)
(define BLACK 2)
(define SHOW (hash EMPTY "." WHITE "o" BLACK "x"))
(define PASS -1)
(define MAXMOVES (* SIZE SIZE 3))
(define TIMESTAMP 0)
(define MOVES 0)

(define-type Square%
  (Class
    (init [init-board Board]
          [init-pos Integer])
    (field [board Board] ; Dyn in Retic
           [pos Integer]
           [zobrist_strings (Listof Integer)]
           [color Integer]
           [timestamp Integer]
           [removestamp Integer]
           [reference Square] ;; Dyn in Retic
           [ledges Integer]
           [temp_ledges Integer]
           [used Boolean]
           [neighbours (Listof Square)]) ;; Listof Dyn in Retic
    (set_neighbours (-> Void))
    (move (-> Integer Void))
    (remove (-> Square Boolean Void))
    (find (-> Boolean Square))))

(define-type Square
  (Instance Square%))

(define-type EmptySet%
  (Class
    (init [init-board Board])
    (field [board Board]
           [empties (Listof Integer)]
           [empty_pos (Listof Integer)])
    (random_choice (-> Integer))
    (add (-> Integer Void))
    (remove (-> Integer Boolean Void))
    (set (-> Integer Integer Void))))

(define-type EmptySet
  (Instance EmptySet%))

(define-type ZobristHash%
  (Class
    (init [init-board Board])
    (field [board Board]
           [hash_set (Setof Integer)]
           [hash Integer])
    (update (-> Square Integer Void))
    (add (-> Void))
    (dupe (-> Boolean))))

(define-type ZobristHash
  (Instance ZobristHash%))

(define-type Board%
  (Class
    (field [squares (Listof Square)]
           [emptyset EmptySet]
           [zobrist ZobristHash]
           [color Integer]
           [finished Boolean]
           [history (Listof Integer)]
           [white_dead Integer]
           [black_dead Integer]
           [lastmove Integer])
    (reset (-> Void))
    (move (-> Integer Void))
    (random_move (-> Integer))
    (useful_fast (-> Square Boolean))
    (useful (-> Integer Boolean))
    (useful_moves (-> (Listof Integer)))
    (replay (-> (Listof Integer) Void))
    (score (-> Integer Flonum))
    #;(check (-> Void))))

(define-type Board
  (Instance Board%))

(define-type UCTNode%
  (Class
    (init)
    (field [bestchild (U #f UCTNode)]
           [pos Integer]
           [wins Natural]
           [losses Natural]
           [pos_child (Listof (U #f UCTNode))]
           [parent (U #f UCTNode)]
           [unexplored (Listof Integer)])
    (play (-> Board Void))
    (select (-> Board Integer))
    (random_playout (-> Board Void))
    (update_path (-> Board Integer (Listof UCTNode) Void))
    (score (-> Flonum))
    (best_child (-> UCTNode))
    (best_visited (-> UCTNode))))

(define-type UCTNode
  (Instance UCTNode%))

