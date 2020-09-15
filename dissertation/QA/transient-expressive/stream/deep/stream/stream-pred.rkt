#lang typed/racket/base

(provide stream?)

(require typed/racket/unsafe)
(unsafe-require/typed racket/stream
                      [stream? (-> Any Boolean : #:+ (Sequenceof Any))])
