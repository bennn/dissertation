#lang greenman-thesis/include

@(require
   (only-in file/glob glob)
   (only-in pict text vl-append table bitmap lt-superimpose blank scale)
   (prefix-in sample: greenman-thesis/validate-sample/main))

@appendix

@title[#:tag "appendix"]{Appendix}

@section[#:tag "appendix:sample"]{Sample Validation}

@(define (snoc x x*) (append x* (list x)))

@(define (validate-sample-plot sub-dir)
   (table
     2
     (snoc
       (blank)
       (for/list ((bm (in-list sample:bm*)))
         (vl-append 4
           (text (~a bm) (cons 'bold "Liberation Serif") 9)
           (scale (bitmap (car (glob (build-path "validate-sample" sub-dir (format "~a-*.png" bm))))) 49/100))))
     lt-superimpose lt-superimpose 20 10))

The approximate evaluation method in @chapter-ref{sec:perf:approximate}
 uses simple random sampling to guess the proportion of @ddeliverable{D}
 configurations in a benchmark.
Random sampling is statistically likely to yield an accurate and precise guess, and
 indeed @figure-ref["fig:tr:validate-sample" "fig:tr:validate-sample"]
 present correct and thin intervals.
But sampling can go poorly.
An unlucky guess based on @${r\!=\!@id[NUM-SAMPLE-TRIALS]} samples
 that each contain @${s\!=\!@id[SAMPLE-RATE]\!*\!N} of the absolute-fastest
 configurations is overly optimistic.
The figures in this section double-check our earlier results with additional
 samples and suggest that precise intervals are indeed the norm.

These validation results use new, randomly-generated random samples.
In this section, one @emph{sampling experiment} for a benchmark with
 @${N} modules is a collection of
 @${r\!=\!@id[NUM-SAMPLE-TRIALS]} samples
 that each contain @${s\!=\!@id[SAMPLE-RATE]\!*\!N} configurations
 chosen uniformly at random without replacement.
The question is whether the sampling experiments, as a whole, are typically
 accurate.
One test of accuracy is to measure the average distance from sample conclusions
 to the truth.
More precisely, for 200 evenly-spaced values of @${D} between @${1} and @${20},
 the first test compares the true proportion of @ddeliverable{D} configurations to
 average distance across all upper and lower-bound guesses generated from each
 sample experiment.
A second test is to validate each upper and lower bound individually by taking its
 distance and negating the number if the guess is in the wrong direction.

@Figure-ref{fig:appendix:validate-sample-avg} counts the average distance
 from an approximation to the truth across @~a[sample:num-experiments]
 sampling experiments.
If the samples are accurate, these distances should be close to zero.
And indeed, the worst average distance is 4% away from the true proportion
 of @ddeliverable{D} configurations.

@Figure-ref{fig:appendix:validate-sample-all} presents data on both the
 accuracy and correctness of @~a[sample:num-experiments] different sampling
 experiments.
For each individual guess, the data records both its distance from the truth
 and whether the direction is correct.
Ideally, every guess should end up with a small positive number.
The figure is slightly worse, but still good.
Most guesses fall within 0% and 5% from the truth.
The few bad guesses end up with a worst case of 9% off in the correct direction
 and -4% off in the misleading one.

@figure*[
  "fig:appendix:validate-sample-avg"
  @elem{Average distance from the true proportion of @ddeliverable{D}
   configurations across @~a[sample:num-experiments] approximate intervals.}
  (validate-sample-plot "avg")]

@figure*[
  "fig:appendix:validate-sample-all"
  @elem{Exact difference between the true proportion of @ddeliverable{D}
  configurations and the bounds at each point along @~a[sample:num-experiments]
  approximate intervals. A negative number reflects a misleading upper or lower bound.}
  (validate-sample-plot "all")]


@; -----------------------------------------------------------------------------
@exact|{\bibliography{bg}}|
