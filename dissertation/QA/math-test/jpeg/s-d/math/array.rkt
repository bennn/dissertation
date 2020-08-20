#lang racket/base

(require "private/array/array-struct.rkt"
         "private/array/array-sequence.rkt"
         "private/array/array-comprehension.rkt")

(provide
  Array
  for/array
  array-shape
  (rename-out [-build-array build-array])
  in-array)
