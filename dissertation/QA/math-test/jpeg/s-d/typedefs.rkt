#lang typed/racket/base

(provide
  PTs
  Bit-Port
  Q-Table
  QT*
  H*
  MCU
  Interp
  Value-Interpreter
  Huffman)

(require (only-in "math/array.rkt" Array))

(define-type Bit-Port (Vector Natural Natural Port))

(define-type Huffman
  (Vector Bytes                ; size-counts
          (Vectorof (U #f Natural))   ; size-offsets
          Bytes                ; values
          (Vectorof (U #f Natural))   ; value-indexes
          Bytes                ; sizes
          (Vectorof (U #f Natural))   ; codes
          (Vectorof Integer))) ; max-codes

(define-type Q-Table (Vectorof (U #f Natural)))

(define-type QT* (Vectorof (U #f Q-Table)))
(define-type H* (Vectorof (U #f Huffman)))
(define-type MCU (Vectorof (Array (Vectorof Integer))))

(define-type Basic-Value (U Bytes (Pairof Natural Natural) Natural Void String Boolean (Vectorof Natural)))
(define-type Interp (U Basic-Value (Listof (Pairof Symbol Basic-Value))))
(define-type Value-Interpreter (-> Natural Interp))

(define-type PTs (Listof (Pairof (U Integer Symbol) Interp)))
