#lang typed/racket/base

(require/typed/provide
 "stream-cons-thunk-untyped.rkt"
 [stream-cons/thunk (All (a) [(-> a) (-> (Sequenceof a)) -> (Sequenceof a)])]
 [empty-stream (Sequenceof Nothing)]
 )

