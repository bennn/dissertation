#lang greenman-thesis/include

@(require
   greenman-thesis/jfp-2019/main)

@title{Performance}

@summary{
  A sound gradual type system ensures that untyped components of a program can
   never break the guarantees of statically typed components.
  This assurance relies on runtime checks, which in turn impose performance
   overhead in proportion to the frequency and nature of interaction between
   typed and untyped components.

  This chapter presents the first systematic method for evaluating the
   performance of sound gradual type systems.
  The method quantifies both the absolute performance of a gradual type system
   and the relative performance of two implementations of the same gradual type
   system.
  To validate the method, the chapter reports on its application to
   @integer->word[(*NUM-BENCHMARKS*)] programs and @integer->word[(length (*RKT-VERSIONS*))]
   implementations of Typed Racket.
}

@jointwork[
  #:people* '("Asumu Takikawa"
              "Max S. New"
              "Daniel Feltey"
              "Jan Vitek"
              "Robert Bruce Findler"
              "Sam Tobin-Hochstadt"
              "Zeina Migeed")
  #:paper* '("gtnffvf-jfp-2019" "gm-pepm-2018" "tfgnvf-popl-2016")
]

@; Systematic data collection, what is the space what are the assumptions.
@; Random sampling for large programs.
@; 
@; Visualization, compress 2N dataset to decent figure.
@; 
@; Application to Typed Racket and Reticulated, validate the method,
@;  suggests apples/oranges differences.
@; 
@; Question, can port Transient to Racket and reproduce?

