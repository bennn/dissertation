#lang typed/racket

(provide
   Endianness
   endianness)

(require
  (only-in rnrs/bytevectors-6
    endianness))

(define-type Endianness Symbol)

(require/typed/provide rnrs/bytevectors-6
  [bytevector-u16-ref (-> Bytes Natural Endianness Natural)]
  [bytevector-u32-ref (-> Bytes Natural Endianness Natural)]
  [bytevector-s8-ref (-> Bytes Natural Byte)]
  [bytevector-s16-ref (-> Bytes Natural Endianness Natural)]
  [bytevector-s32-ref (-> Bytes Natural Endianness Natural)]
  [bytevector-u8-ref (-> Bytes Natural Byte)]
  [bytevector-ieee-single-ref (-> Bytes Natural Endianness Natural)]
  [bytevector-ieee-double-ref (-> Bytes Natural Endianness Natural)]
  [make-bytevector (-> Natural Bytes)]
  [bytevector-copy! (-> Bytes Natural Bytes Natural Natural Void)]
  [utf8->string (-> Bytes String)])
