#lang greenman-thesis/include

@(require
   (prefix-in tr: greenman-thesis/jfp-2019/main)
   (only-in gtp-plot/plot *OVERHEAD-MAX*)
   (only-in greenman-thesis/shallow/main
     SHALLOW-CURRENT-BENCHMARK*
     get-mixed-path-table
     render-mixed-path-table
     get-mixed-worst-table
     render-mixed-worst-table
     find-lowest-3dpath-D
     get-3d-table
     s:cache-dir)
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


@section[#:tag "appendix:overhead"]{@|sDeep| vs. @|sShallow| Overhead}

@Figures-ref["fig:appendix:overhead" (exact-ceiling (/ (length SHALLOW-CURRENT-BENCHMARK*) overhead-plots-per-page))]
 compare @|sdeep| and @|sshallow| types to the extreme.
Whereas the plots in @chapter-ref{sec:transient:overhead} stop at 20x overhead,
 these go out to the worst-case overhead.


@(parameterize ((*OVERHEAD-MAX* 'infer))
@render-overhead-plot*[
  "fig:appendix:overhead"
  @elem{
  Deep vs. Shallow
  }
  ""
  tr:render-relative-overhead-plot
  (for/list ((bm-name (in-list SHALLOW-CURRENT-BENCHMARK*)))
    (cons bm-name (cons tr:transient-rkt-version stransient)))
  s:cache-dir
])


@exact{\clearpage}
@section[#:tag "appendix:rules"]{Missing Rules}

@Figure-ref{fig:appendix:surface-type} and @figure-ref{fig:appendix:completion}
 present unremarkable typing and completion rules that are omitted from @chapter-ref{sec:both:model}.
Note that the completion rules depend on the full typing derivation of a term
 to uncover, for example, the types of the arguments to a binary operation.
The model in @chapter-ref{chap:design} avoids this technicality by
 asking for annotations on elimination forms.


@figure*[
  "fig:appendix:surface-type"
  @elem{Additional surface typing rules}

@exact|{
\begin{mathpar}
  \inferrule*{
    \stypeenv \sST \ssurface_0 : \stype_0
    \\
    \stypeenv \sST \ssurface_1 : \stype_1
  }{
    \stypeenv \sST \epair{\ssurface_0}{\ssurface_1} : \tpair{\stype_0}{\stype_1}
  }

  \inferrule*{
    \stypeenv \sST \ssurface_0 : \tfloor{\stype_0}
    \\
    \stypeenv \sST \ssurface_1 : \tfloor{\stype_1}
  }{
    \stypeenv \sST \epair{\ssurface_0}{\ssurface_1} : \tfloor{\tpair{\stype_0}{\stype_1}}
  }

  \inferrule*{
    \stypeenv \sST \ssurface_0 : \tdyn
  }{
    \stypeenv \sST \eunop{\ssurface_0} : \tdyn
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfloor{\stype_0}
    \\
    \sDelta(\sunop, \stype_0) = \stype_1
  }{
    \stypeenv \sST \eunop{\sexpr_0} : \tfloor{\stype_1}
  }

  \inferrule*{
    \stypeenv \sST \ssurface_0 : \tdyn
    \\
    \stypeenv \sST \ssurface_1 : \tdyn
  }{
    \stypeenv \sST \ebinop{\ssurface_0}{\ssurface_1} : \tdyn
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \stype_0
    \\
    \stypeenv \sST \sexpr_1 : \stype_1
    \\\\
    \sDelta(\sbinop, \stype_0, \stype_1) = \stype_2
  }{
    \stypeenv \sST \ebinop{\sexpr_0}{\sexpr_1} : \stype_2
  }

  \inferrule*{
    \stypeenv \sST \ssurface_0 : \tdyn
    \\
    \stypeenv \sST \ssurface_1 : \tdyn
  }{
    \stypeenv \sST \eapp{\ssurface_0}{\ssurface_1} : \tdyn
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfun{\stype_0}{\stype_1}
    \\
    \stypeenv \sST \sexpr_1 : \stype_0
  }{
    \stypeenv \sST \eapp{\sexpr_0}{\sexpr_1} : \stype_1
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \stype_0
    \\
    \fsubt{\stype_0}{\stype_1}
  }{
    \stypeenv \sST \sexpr_0 : \stype_1
  }

\end{mathpar}
}|]

@figure*[
  "fig:appendix:completion"
  @elem{Additional surface-to-evaluation completion rules}

@exact|{
\begin{mathpar}
  \inferrule*{
  }{
    \stypeenv \sST \sint_0 : \tdyn \scompile \sint_0
  }

  \inferrule*{
  }{
    \stypeenv \sST \sint_0 : \stype_0 \scompile \sint_0
  }

  \inferrule*{
  }{
    \stypeenv \sST \sint_0 : \tfloor{\stype_0} \scompile \sint_0
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tdyn \scompile \sexpr_2
    \\\\
    \stypeenv \sST \sexpr_1 : \tdyn \scompile \sexpr_3
  }{
    \stypeenv \sST \epair{\sexpr_0}{\sexpr_1} : \tdyn \scompile \epair{\sexpr_2}{\sexpr_3}
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \stype_0 \scompile \sexpr_2
    \\\\
    \stypeenv \sST \sexpr_1 : \stype_1 \scompile \sexpr_3
  }{
    \stypeenv \sST \epair{\sexpr_0}{\sexpr_1} : \tpair{\stype_0}{\stype_1} \scompile \epair{\sexpr_2}{\sexpr_3}
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfloor{\stype_0} \scompile \sexpr_2
    \\\\
    \stypeenv \sST \sexpr_1 : \tfloor{\stype_1} \scompile \sexpr_3
  }{
    \stypeenv \sST \epair{\sexpr_0}{\sexpr_1} : \tfloor{\tpair{\stype_0}{\stype_1}} \scompile \epair{\sexpr_2}{\sexpr_3}
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tdyn \scompile \sexpr_1
  }{
    \stypeenv \sST \eunop{\sexpr_0} : \tdyn \scompile \eunop{\sexpr_1}
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tpair{\stype_0}{\stype_1} \scompile \sexpr_1
  }{
    \stypeenv \sST \eunop{\sexpr_0} : \stype_0 \scompile \eunop{\sexpr_1}
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfloor{\tpair{\stype_0}{\stype_1}} \scompile \sexpr_1
    \\\\
    \fshape{\stype_0} = \sshape_0
  }{
    \stypeenv \sST \eunop{\sexpr_0} : \tfloor{\stype_0} \scompile \escan{\sshape_0}{\eunopfst{\sexpr_1}}
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tdyn \scompile \sexpr_2
    \\\\
    \stypeenv \sST \sexpr_1 : \tdyn \scompile \sexpr_3
  }{
    \stypeenv \sST \ebinop{\sexpr_0}{\sexpr_1} : \tdyn \scompile \ebinop{\sexpr_2}{\sexpr_3}
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \stype_0 \scompile \sexpr_2
    \\\\
    \stypeenv \sST \sexpr_1 : \stype_1 \scompile \sexpr_3
  }{
    \stypeenv \sST \ebinop{\sexpr_0}{\sexpr_1} : \stype_2 \scompile \ebinop{\sexpr_2}{\sexpr_3}
  }

  \inferrule*{
    \stypeenv \sST \sexpr_0 : \tfloor{\stype_0} \scompile \sexpr_2
    \\\\
    \stypeenv \sST \sexpr_1 : \tfloor{\stype_1} \scompile \sexpr_3
  }{
    \stypeenv \sST \ebinop{\sexpr_0}{\sexpr_1} : \tfloor{\stype_2} \scompile \ebinop{\sexpr_2}{\sexpr_3}
  }

\end{mathpar}
}|]


@exact{\clearpage}
@section[#:tag "appendix:paths"]{More Evidence for @|sDeep| and @|sShallow|}

@subsection{Migration Paths}

@(let* ([bm* '(sieve forth fsm fsmoo mbta morsecode
               zombie dungeon jpeg zordoz lnm suffixtree
               kcfa snake take5)]
        [D 3]
        [PT (get-mixed-path-table D bm*)]
        [deep-dead* (for/list ((path-info (in-list PT)) #:when (string=? "0" (caddr path-info))) (car path-info))]
        [shallow-dead* (for/list ((path-info (in-list PT)) #:when (string=? "0" (cadddr path-info))) (car path-info))]
        [mix-dead* (for/list ((path-info (in-list PT)) #:when (string=? "0" (cadddr (cdr path-info)))) (car path-info))]
        [_md (unless (= 1 (length mix-dead*)) (printf "HELP expected one mix-dead row got ~s~n" mix-dead*))]
        [mix-path-bm 'fsm]
        [mix-best-D (find-lowest-3dpath-D mix-path-bm)]
        )
@list[
@figure*[
  "fig:both:mixed-path"
  @elem{Percent of @ddeliverable[D] paths in three lattices:
   the @|sdeep|-typed lattice, the @|sshallow|-typed lattice,
   and a hybrid that chooses the best of @|sdeep| or @|sshallow| types at each point.}
  @render-mixed-path-table[PT]
]
@elem{
@|sShallow| types make step-by-step migration more practical in Typed Racket.
Originally, with @|sdeep| types, a programmer who adds types one module at
 a time is likely to hit a performance wall; that is, a few configurations
 along the migration path are likely to suffer a large overhead.
Adding more @|sdeep| types is a sure way to reduce the overhead,
 especially if the programmer adds the best-possible types (@figure-ref{fig:example-path}),
 but these multi-step pitfalls contradict the promise of migratory typing.
High overhead makes it hard to tell whether the new types are compatible with
 the rest of the codebase.

By choosing @|sdeep| or @|sshallow| types at each point along a path, the
 worst-case overhead along migration paths goes down.
@Figure-ref{fig:both:mixed-path} quantifies the improvement by showing the
 percent of all paths that are @ddeliverable[D] at each step.
With @|sdeep| types alone, all paths in @integer->word[(length deep-dead*)]
 benchmarks hit a point that exceeds the @~a[D]x limit.
With @|sshallow| types alone, all paths in @integer->word[(length shallow-dead*)] benchmarks
 exceed the limit as well.
With the mix, however, only @integer->word[(length mix-dead*)] benchmark (@bm[(car mix-dead*)])
 has zero @ddeliverable[D] paths.
Fine-grained combinations of @|sdeep| and @|sshallow| types can further improve
 the number of viable migration paths.
In @bm[mix-path-bm], for example, every path is @ddeliverable[mix-best-D] if the programmer
 picks the fastest-running mix of @|sdeep| and @|sshallow| types for each configuration.
}])

@subsection{Case Study: GTP Benchmarks}

@(let* ((DDD (get-3d-table '(fsm morsecode jpeg kcfa zombie zordoz)))
        (num-DDD (length DDD))
        (S (tr:benchmark-name->performance-info 'fsm tr:default-rkt-version))
        (fsm-num-modules (performance-info->num-units S))
        (fsm-num-configs (expt 2 fsm-num-modules))
        (fsm-non-mixed (+ 1 fsm-num-modules))
        (fsm-mixed (- fsm-num-configs fsm-non-mixed)))
@list[
@elem{
  For @integer->word[num-DDD] small benchmarks, I measured the full
   space of @${3^N} configurations that can arise by combining @|sdeep|
   and @|sshallow| types.
  Each configuration ran successfully, affirming that @|sdeep| and @|sshallow|
   can interoperate.
  Furthermore, a surprising percent of all @${2^N} mixed-typed configurations
   in each benchmark ran fastest using a mixture of @|sdeep| and @|sshallow|
   types:
}
@(apply itemlist
   (for/list ((d-row (in-list DDD))
              (i (in-naturals 1)))
     (item (format "~a% of " (cadr d-row))
           (bm (car d-row))
           (format " configurations~a"
                   (cond
                    [(= i num-DDD)
                     "."]
                    [(= (+ i 1) num-DDD)
                     "; and"]
                    [else
                     ";"])))))
@elem{
@|noindent|In @bm{fsm}, for example, there are @integer->word[fsm-num-configs] mixed-typed configurations.
@Integer->word[fsm-non-mixed] of these cannot mix @|sdeep| and @|sshallow|
 because they contain at most one typed module.
Of the remaining @~a[fsm-mixed] configurations, over half run fastest with a
 combination of @|sdeep| and @|sshallow| types.
}
])



@; -----------------------------------------------------------------------------
@exact|{\bibliography{bg}}|
