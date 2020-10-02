#lang info

(define collection "greenman-thesis")
(define deps '(
  "at-exp-lib"
  "base"
  "gtp-plot"
  "gtp-util"
  "pict-lib"
  "reprovide-lang"
  "scribble-abbrevs"
  "scribble-lib"
  "with-cache"
))
(define build-deps '(
  "racket-doc"
  "rackunit-lib"
  "scribble-doc"
))
(define pkg-desc "PhD dissertation, Ben Greenman, 2020")
(define compile-omit-paths '("jfp-2019/benchmarks" "pepm-2018/benchmarks"))
(define version "0.0")
(define pkg-authors '(ben))

