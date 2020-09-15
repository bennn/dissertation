#lang typed/racket/base

(require "stream/stream-pred.rkt" "stream/stream-cons.rkt")
(provide stream? stream-cons stream empty-stream)

(require/typed/provide
 racket/stream
 [stream-empty? [(Sequenceof Any) -> Boolean]]
 [stream-first (All (a) [(Sequenceof a) -> a])]
 [stream-rest (All (a) [(Sequenceof a) -> (Sequenceof a)])]
 ;; stream-cons and stream are reimplemented in typed/racket/stream/stream-cons.rkt
 [in-stream (All (a) [(Sequenceof a) -> (Sequenceof a)])]
 ;; empty-stream is also provided from typed/racket/stream/stream-cons.rkt
 [stream->list (All (a) [(Sequenceof a) -> (Listof a)])]
 [stream-length (All (a) [(Sequenceof a) -> Natural])]
 [stream-ref (All (a) [(Sequenceof a) Natural -> a])]
 [stream-tail (All (a) [(Sequenceof a) Natural -> (Sequenceof a)])]
 [stream-append (All (a) [(Sequenceof a) * -> (Sequenceof a)])]
 )

