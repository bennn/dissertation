#lang typed/racket/base #:transient

(require/typed/provide
 "stream-cons-thunk-untyped.rkt"
 [stream-cons/thunk (All (a) [(-> a) (-> (Sequenceof a)) -> (Sequenceof a)])]
 [empty-stream (Sequenceof Nothing)]
 )

