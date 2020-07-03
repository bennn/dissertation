#lang greenman-thesis/include

@; TODO, outline:
@; - [ ] method : space, assumptions, goals, exhaustive, approximate, threats
@;   - [ ] fun, other visualizations from JFP that fell by the wayside
@; - [ ] app TR : benchmarks, protocol, data
@; - [ ] app RP : benchmarks, protocol, data
@;
@; special acknowledgment to Asumu, for visualizations in POPL paper,
@;  because my dissertation has overlap with his
@;
@; do NOT need summary (abstract) and intro paragraph
@;
@; explain that JFP benchmarks are buggy, point to gtp-benchmarks release notes


@(require
   greenman-thesis/jfp-2019/main
   gtp-plot/configuration-info
   gtp-plot/typed-racket-info
   gtp-plot/performance-info)

@title{Performance Analysis Method}
@jointwork[
  #:people* '(
    "Asumu Takikawa"
    "Max S. New"
    "Daniel Feltey"
    "Jan Vitek"
    "Robert Bruce Findler"
    "Sam Tobin-Hochstadt"
    "Zeina Migeed"
  )
  #:paper* '("gtnffvf-jfp-2019" "gm-pepm-2018" "tfgnvf-popl-2016")
]

Sound gradual types come with performance overhead.
A soundness theorem guarantees that static types are valid claims about the
 run-time behavior of a program.
Gradual types can mix with untyped code.
Therefore, a gradual typing system needs run-time checks to protect typed
 code from invalid untyped values.
These run-time checks impose a cost in proportion to the frequency
 of typed/untyped interactions, the nature of the data that crosses
 these type boundaries, and the strength of the soundness guarantee.

Language designers must measure the overhead of gradual types
 to judge the overall usefulness of a gradual typing system.
 @; ... benefits vs costs
Such measuments must systematically explore the mixed-typed space that
 a language offers to language users.
The data tells users what kind of performance to expect.
Furthermore, language designers can employ the data to justify improvements
 and alternative gradual typing approaches.

This chapter presents a systematic and scalable evaluation method that I
 developed with several collaborators---notably Asumu Takikawa and Zeina
 Migeed.
...... ...... ..... explain that sentence


@; This chapter presents the first systematic method for evaluating the
@;  performance of sound gradual type systems.
@; The method quantifies both the absolute performance of a gradual type system
@;  and the relative performance of two implementations of the same gradual type
@;  system.
@; To validate the method, the chapter reports on its application to
@;  @integer->word[(*NUM-BENCHMARKS*)] programs and @integer->word[(length (*RKT-VERSIONS*))]
@;  implementations of Typed Racket.
@; Also many Reticulated programs.


@section{Design Criteria}
@; are there good words, from Asumu?, we can use to talk about criteria/requirements?

@; - space = all configurations, visualize as lattice
@;  > complication, granularity
@; - baseline = no gradual typing (untyped, depends on context)
@; - too much data need to present
@;   what's important = deliverable
@; - ... sample ...
@; - ... relative ...




@section{Evaluation Method}

The method = 2 methods = exhaustive and approximate
how to do it, what parameters
includes benchmark adaptation

@citet{tfgnvf-popl-2016} introduce a three-step method for evaluating the performance of
 a gradual typing system:
 (1) identify a suite of fully-typed programs;
 (2) measure the performance of all gradually-typed @emph{configurations} of the programs;
 (3) count the number of configurations with performance overhead no greater than a certain limit.
They apply this method to Typed Racket, a gradual typing system
 with module-level granularity; in other words, a Typed Racket program with @${M} modules has
 @${2^M} gradually-typed configurations.

Reticulated supports gradual typing at a much finer granularity,
 making it impractical to directly apply the Takikawa method.
A naive application would require @${2^a} measurements for one function with @${a} formal parameters,
 and similarly @${2^f} measurements for one class with @${f} fields.
The following subsections therefore generalize the Takikawa method (@section-ref{sec:method:adapt})
 and describe the protocol we use to evaluate Reticulated (@section-ref{sec:protocol}).


@; -----------------------------------------------------------------------------
@subsection{Exhaustive Evaluation Method}

A performance evaluation of gradual type systems must reflect how programmers
 use such systems.
Since experience with Typed Racket shows that programmers frequently combine
 typed and untyped code within an application, an evaluation method must account
 for the large space of code between untyped and fully-typed programs.
These applications may also undergo gradual transitions that add or remove some
 type annotations.
In a typical evolution, programmers compare the performance of the modified
 program with the previous version.
If type-driven optimizations result in a performance improvement, all is well.
Otherwise, the programmer may need to address the performance overhead.
As they continue to develop the application, programmers repeat this process.
Again, an evaluation method must take into account gradual evolution to reflect
 practice.

The following subsections build an evaluation method from these observations in
 three steps.
First, @secref{sec:method:lattice} describes the space over which a performance
 evaluation must take place.
Second, @secref{sec:measurements} defines metrics relevant to the performance
 of a gradually typed program.
Third, @secref{sec:graphs} introduces a visualization that concisely presents
 the metrics on exponentially large spaces.


@; -----------------------------------------------------------------------------
@subsection[#:tag "sec:method:lattice"]{Performance Lattice}

@(define suffixtree-lattice-version "6.2")
@(define S (make-typed-racket-info (benchmark-rktd suffixtree suffixtree-lattice-version)))
@(define suffixtree-num-modules     (integer->word (performance-info->num-units S)))
@(define suffixtree-num-configs     (performance-info->num-configurations S))
@(define suffixtree-num-configs-str (number->string suffixtree-num-configs))
@(define suffixtree-sample-D        2)
@(define suffixtree-num-D           ((deliverable suffixtree-sample-D) S))
@(define MAX-OVERHEAD 20)
@(define suffixtree-num-D-max       ((deliverable MAX-OVERHEAD) S))
@(define suffixtree-num-D-str       (integer->word suffixtree-num-D))
@; TODO need to talk about L-steps in the gtp-plot package!
@;@(define suffixtree-sample-k        1)
@;@(define suffixtree-num-k           (count-configurations S (lambda (cfg) (<= (configuration-info->mean-runtime cfg) suffixtree-sample-D))))
@;@(define suffixtree-num-k-str       (number->string suffixtree-num-k))
@(define suffixtree-tu-ratio        (format "~ax" (~r (typed/untyped-ratio S) #:precision '(= 1))))
@(define suffixtree-max             MAX-OVERHEAD)
@(define suffixtree-num-max         (- suffixtree-num-configs ((deliverable suffixtree-max) S)))
@(define suffixtree-num-samples 111)

    @figure*["fig:suffixtree-lattice" @elem{Performance overhead in @bm[suffixtree], on Racket v@|suffixtree-lattice-version|.}
      @(parameterize ([*LATTICE-CONFIG-X-MARGIN* 3]
                      [*LATTICE-CONFIG-Y-MARGIN* 8]
                      [*FONT-SIZE* 10]
                      [*LATTICE-UNIT-HEIGHT* 6]
                      [*LATTICE-UNIT-WIDTH* 3]
                      [*LATTICE-UNIT-X-MARGIN* 0]
                      [*LATTICE-UNIT-BORDER-WIDTH* 0]
                      [*LATTICE-LINES?* #true]
                      [*LATTICE-CONFIG-LABEL-MARGIN* 1])
        (performance-lattice S))
    ]

The promise of Typed Racket's macro-level gradual typing is that programmers
 can add types to any subset of the modules in an untyped program.
In principle, this promise extends to third-party libraries and modules from
 the Racket runtime system, but in practice a programmer has no control over
 such modules.
Thus we distinguish between two kinds of modules: the @emph{migratable} modules
 that a programmer may add types to, and the @emph{contextual} modules in the
 software ecosystem, which remain unchanged.
A comprehensive performance evaluation must therefore consider the
 @emph{configurations} a programmer could possibly create given type annotations
 for each migratable module.
These configurations form a lattice, ordered by the subset relation on the set
 of typed modules in a configuration.

@Figure-ref{fig:suffixtree-lattice} demonstrates one such lattice for a program
 with @|suffixtree-num-modules| migratable modules.
The black rectangle at the top of the lattice represents the configuration in
 which all @|suffixtree-num-modules| modules are typed.
The other @id[(sub1 suffixtree-num-configs)] rectangles represent
 configurations with some untyped modules.

A given row in the lattice groups configurations with the same number of typed
 modules (black squares).
For instance, configurations in the second row from the bottom contain two
 typed modules.
These represent all possible ways of converting two modules in the untyped
 configuration to Typed Racket.
Similarly, configurations in the third row represent all possible
 configurations a programmer might encounter after applying three such
 @emph{type conversion steps} to the untyped configuration.
In general, let the notation @${c_1 \rightarrow_k c_2} express the idea
 that a programmer starting from configuration @${c_1} (in row
 @${i}) could reach configuration @${c_2} (in row @${j}) after
 taking at most @${k} type conversion steps (@${j - i \le k}).

Configurations in @figure-ref{fig:suffixtree-lattice} are furthermore labeled
 with their performance overhead relative to the untyped configuration on Racket
 version 6.2.
With these labels, a language implementor can draw several conclusions about
 the performance overhead of gradual typing in this program.
For instance, @|suffixtree-num-D-str| configurations run within a
 @id[suffixtree-sample-D]x overhead.
 @; and @|suffixtree-num-k-str| configurations
 @; are at most @integer->word[suffixtree-sample-k] type conversion step from a
 @; configuration that runs within a @id[suffixtree-sample-D]x overhead.
High overheads are common (@id[suffixtree-num-max] configurations have over
 @id[suffixtree-max]x overhead), but the fully typed configuration runs faster
 (0.7x overhead) than the untyped configuration because Typed Racket uses the
 type annotations to compile efficient bytecode.

A labeled lattice such as @figure-ref{fig:suffixtree-lattice} is a @emph{performance lattice}.
The same lattice without labels is a @emph{configuration lattice}.
Practically speaking, users of a gradual type system explore configuration lattices and maintainers of such systems may use performance lattices to evaluate overall performance.


@; -----------------------------------------------------------------------------
@subsection[#:tag "sec:measurements"]{Performance Metrics}

The most basic question, and least important, about a gradually typed language is
 how fast fully-typed programs are in comparison to their fully untyped relative.
In principle and in Typed Racket, static types enable optimizations and can
 serve in place of runtime tag checks.
The net effect of such improvements may, however, be offset by the runtime cost
 of enforcing type soundness.
Relative performance is therefore best described as a ratio, to capture the
 possibility of speedups and slowdowns.@;

    @definition["typed/untyped ratio"]{
     The typed/untyped ratio of a performance
      lattice is the time needed to run the top (fully typed) configuration divided by the
      time needed to run the bottom (untyped) configuration.
    }

For users of a gradual type system, the important performance
 question is how much overhead their current configuration suffers.
 @; RELATIVE TO previous version (standardized as "untyped program")
If the performance overhead is low enough, programmers can release the
 configuration to clients.
Depending on the nature of the application, some developers might not accept
 any performance overhead.
Others may be willing to tolerate an order-of-magnitude slowdown.
The following parameterized definition of a deliverable configuration accounts
 for these varying requirements.@;

    @definition[@list{@ddeliverable{}}]{
     A configuration
      is @ddeliverable{} if its performance is no worse than a
      factor of @${D} slowdown compared to the untyped configuration.
     @;A program is @ddeliverable{} if all its configurations are @ddeliverable{}.
    }

@; NOTE: really should be using %, but Sec 4 shows why stick to x
@;
If an application is currently in a non-@ddeliverable[] configuration,
 the next question is how much work a team must invest to reach a
 @ddeliverable[] configuration.
One coarse measure of ``work'' is the number of additional modules that must be
 annotated with types before performance improves.@;

    @definition[@list{@kstep{}}]{
     A configuration @exact{$c_1$} is @kstep[] if @exact{$c_1 \rightarrow_k c_2$}
      and @exact{$c_2$} is @ddeliverable{}.
    }

The number of @kstep[] configurations captures the experience of a prescient
 programmer that converts the @exact{$k$} modules that maximize the performance
 improvement.

@(define sample-data
  (let* ([rng (vector->pseudo-random-generator (vector 0 1 2 3 4 5))]
         [approx (lambda (n) (let ([offset (/ n (random 1 10 rng))]) (if (zero? (random 0 1 rng)) (+ n offset) (- n offset))))]
         [mean+std* `#((20 . 0)
                       (50 . 0) (,(approx 50) . 0) (,(approx 50) . 0)
                       (30 . 0) (,(approx 30) . 0) (,(approx 30) . 0)
                       (10 . 0))]
         [mean (lambda (i) (car (vector-ref mean+std* i)))])
    (lambda (tag)
      (case tag
       [(c000) (mean 0)]
       [(c001) (mean 1)]
       [(c010) (mean 2)]
       [(c100) (mean 3)]
       [(c011) (mean 4)]
       [(c101) (mean 5)]
       [(c110) (mean 6)]
       [(c111) (mean 7)]
       [(all) (make-typed-racket-info (for/vector ((ms (in-vector mean+std*))) (list (car ms))))]
       [else (raise-user-error 'sample-data "Invalid configuration '~a'. Use e.g. c000 for untyped." tag)]))))
@(define (sample-overhead cfg)
  (ceiling (/ (sample-data cfg) (sample-data 'c000))))

    @figure["fig:demo-lattice" "Sample performance lattice"
      (parameterize ([*LATTICE-CONFIG-X-MARGIN* 30]
                     [*LATTICE-CONFIG-Y-MARGIN* 8]
                     [*LATTICE-UNIT-X-MARGIN* 0]
                     [*LATTICE-LINE-ALPHA* 0.8])
        (performance-lattice (sample-data 'all))) @; TODO sample data =/= perf-info struct
    ]


Let us illustrate these terms with an example.
Suppose there is a project with
 three modules where the untyped configuration runs in @id[(sample-data 'c000)]
 seconds and the typed configuration runs in @id[(sample-data 'c111)] seconds.
Furthermore, suppose half the mixed configurations run in
  approximately @id[(sample-data 'c001)] seconds and the other half run in
  approximately @id[(sample-data 'c011)] seconds.
@Figure-ref{fig:demo-lattice} is a performance lattice for this hypothetical program.
The label below each configuration is its overhead relative to the untyped configuration.

@(let* ([tu-ratio (/ (sample-data 'c111) (sample-data 'c000))]
        [t-str @id[(sample-overhead 'c111)]]
        [g-overheads (map sample-overhead '(c011 c101 c110 c001 c010 c100))]
        [min-g (inexact->exact (apply max (take g-overheads 3)))]
        [max-g (inexact->exact (apply max g-overheads))])
  @elem{
    The typed/untyped ratio is @id[tu-ratio],
     indicating a performance improvement due to adding types.
    The typed configuration is also
      @ddeliverable[t-str]
      because it runs within a @elem[t-str]x
      slowdown relative to the untyped configuration.
    All mixed configurations are
      @ddeliverable[@id[max-g]], but only three are, e.g.,
      @ddeliverable[@id[min-g]].
    Lastly, the mixed configurations are all @kstep["2" t-str]
     because they can reach the typed configuration in at most two type conversion steps.
  })

The ratio of @ddeliverable{D} configurations in a performance lattice is a
 measure of the overall feasibility of gradual typing.
When this ratio is high, then no matter how the application evolves,
 performance is likely to remain acceptable.
Conversely, a low ratio implies that a team may struggle to recover performance after
 typing a few modules.
Practitioners with a fixed performance requirement @${D} can therefore use the number
 of @ddeliverable[] configurations to extrapolate the performance of a gradual type system.


@; -----------------------------------------------------------------------------
@subsection[#:tag "sec:graphs"]{Overhead Plots}

@; less than half of all @bm[suffixtree] configurations run within a @id[(*MAX-OVERHEAD*)]x slowdown.

Although a performance lattice contains a comprehensive description of
 performance overhead, it does not effectively communicate this information.
It is difficult to tell, at a glance, whether a program has good or bad
 performance relative to its users' requirements.
Comparing the relative performance of two or more lattices is also difficult
 and is in practice limited to programs with an extremely small number of
 modules.

The main lesson to extract from a performance lattice is the proportion of
 @kstep{} configurations for various @${k} and @${D}.
In other words, this proportion describes the number of configurations (out of
 the entire lattice) that are at most @${k} upward steps from a @ddeliverable{D}
 configuration.
One way to plot this information is to fix a value for @${k}, say @${k=0}, and
 consider a set of values @exact{$d_0,\ldots,d_{n-1}$} for @${D}.
The set of proportions of @kstep["0" "d_i"] configurations defines a cumulative
 distribution function with the value of @${D} on the independent axis and the
 proportion of configurations on the dependent axis.

@Figure-ref{fig:suffixtree-plot} demonstrates two such @emph{overhead plots},
 summarizing the data in @figure-ref{fig:suffixtree-lattice}.
@; TODO awkward
Specifically, each plots the @|suffixtree-num-configs-str| configurations of a
 program called @bm[suffixtree] using data measured on Racket
 v@|suffixtree-lattice-version|.
The plot on the left fixes @${k=0} and plots the proportion of @kstep["0" "D"]
 configurations.
The plot on the right fixes @${k=1} and plots the proportion of @kstep["1" "D"]
 configurations.
Both plots consider @${@id[suffixtree-num-samples]} values of @${D} evenly spaced between 1x and
 20x.
The line on each plot represents the cumulative distribution function.
The x-axis is log scaled to focus on low overheads.

The plot on the left, in which @${k=0}, confirms the observation made in
 @secref{sec:method:lattice} that @(id (round (* 100 (/ suffixtree-num-D
 suffixtree-num-configs))))% of the @|suffixtree-num-configs-str| configurations
 (@|suffixtree-num-D-str| configurations) run within a @id[suffixtree-sample-D]x
 overhead.
For values of @${D} larger than 2x, the proportion of @ddeliverable{D}
 configurations is slightly larger, but even at a @id[MAX-OVERHEAD]x overhead,
 this proportion is only @(id (round (* 100 (/ suffixtree-num-D-max
 suffixtree-num-configs))))%.
The plot on the right shows that the proportion of @kstep["1" "D"] is typically
 twice as high as the proportion of @ddeliverable{} configurations for this
 benchmark.

This presentation scales to arbitrarily large programs because the @${y}-axis
 plots the proportion of @ddeliverable{D} configurations; in contrast, a
 performance lattice contains exponentially many nodes.
Furthermore, plotting the overhead for multiple implementations of a gradual
 type system on the same set of axes conveys their relative performance.

@figure*["fig:suffixtree-plot" @elem{Overhead plots for @bm[suffixtree], on Racket v@|suffixtree-lattice-version|.
The unlabeled vertical ticks mark, from left-to-right:
  1.2x, 1.4x, 1.6x, 1.8x, 4x, 6x, 8x, 10x, 12x, 14x, 16x, and 18x.
}
  (parameterize ((*OVERHEAD-SAMPLES* suffixtree-num-samples))
    (overhead-plot S))
]


@subsection{Assumptions and Limitations}

Plots in the style of @figure-ref{fig:suffixtree-plot} rest on two assumptions
 and have two significant limitations, which readers must keep in mind as they
 interpret the results.

@; - assn: log scale
The @emph{first assumption} is that configurations with less than 2x overhead
 are significantly more practical than configurations with a 10x overhead or more.
Hence the plots use a log-scaled x-axis to simultaneously encourage
 fine-grained comparison in the 1.2x to 1.6x overhead range and blur the
 distinction between, e.g., 14x and 18x slowdowns.

@; Zorn: 30% of execution time in storage management "represent the worst-case overhead that that might be expected to be associated with garbage collection."

@; - assn: 20x
The @emph{second assumption} is that configurations with more than 20x
 overhead are completely unusable in practice.
Pathologies like the 100x slowdowns in @figure-ref{fig:suffixtree-lattice}
 represent a challenge for implementors, but if these overheads suddenly
 dropped to 30x, the configurations would still be useless to developers.

@; - limit: no identity, no relation between configs (where is typed?)
The @emph{first limitation} of the overhead plots is that they do not report
 the number of types in a configuration.
The one exception is the fully-typed configuration; its overhead is given
 explicitly through the typed/untyped ratio above the left plot.

@; - limit: angelic choice
The @emph{second limitation} is that the @kstep{1} plot does not show how we
 optimistically chose the best type conversion step.
In a program with @${N} modules, a programmer has at most @${N} type conversion
 steps to choose from, some of which may not lead to a @ddeliverable[] configuration.
For example, there are six configurations with exactly one typed module in
 @figure-ref{fig:suffixtree-lattice} but only one of these is @ddeliverable{1}.





@; -----------------------------------------------------------------------------
@subsection{Definitions}


@; @subsection[#:tag "sec:method:adapt"]{Generalizing the Takikawa Method}

A gradual typing system enriches a dynamically typed language with a notion of static typing;
 that is, some pieces of a program can be statically typed.
The @emph{granularity} of a gradual typing system defines the minimum size of
 such pieces in terms of abstract syntax.
A performance evaluation must define its own granularity to systematically
 explore the ways that a programmer may write type annotations, subject to
 practical constraints.

@definition["granularity"]{
  The @emph{granularity} of an evaluation is the syntactic unit at which
   the evaluation adds or removes type annotations.
}

For example, the evaluation in @citet{tfgnvf-popl-2016} is at the granularity
 of modules.
The evaluation in @citet{vss-popl-2017} is at the granularity
 of whole programs.
@Section-ref{sec:protocol} defines the @emph{function and class-fields} granularity, which we use for this evaluation.

After defining a granularity, a performance evaluation must define a suite of
 programs to measure.
A potential complication is that such programs may depend on external libraries
 or other modules that lie outside the scope of the evaluation.
It is important to distinguish these so-called @emph{contextual modules} from the
 focus of the experiment.

@definition["migratable, contextual"]{
  The @emph{migratable modules} in a program define its configurations.
  The @emph{contextual modules} in a program are common across all configurations.
}

The granularity and migratable modules define the
 @emph{configurations} of a fully-typed program.

@definition["configurations"]{
  Let @${P \tcstep P'}
   if and only if program @${P'} can be obtained from
   @${P} by annotating one syntactic unit in an migratable module.
  Let @${\tcmulti} be the reflexive, transitive closure of the @${\tcstep}
   relation.
  {The @${\tcstep} relation expresses the notion of a
   @emph{type conversion step}@~cite{tfgnvf-popl-2016,gtnffvf-jfp-2019}.
   The @${\tcmulti} relation expresses the notion of @emph{term precision}@~cite{svcb-snapl-2015}.}
  @; note^2: `e0 -->* e1` if and only if `e1 <= e0`
  The @emph{configurations} of a fully-typed program @${P^\tau} are all
   programs @${P} such that @${P\!\tcmulti P^\tau}.
  Furthermore, @${P^\tau} is a so-called @emph{fully-typed configuration};
   an @emph{untyped configuration} @${P^\lambda} has the property @${P^\lambda\!\tcmulti P}
   for all configurations @${P}.
}

An evaluation must measure the performance overhead of these configurations
 relative to some default.
A natural baseline is the performance of the original program, distinct from the
 gradual typing system.

@definition["baseline"]{
 The @emph{baseline performance} of a program is its running time in the absence
  of gradual typing.
}

In Typed Racket, the baseline is the performance of Racket running the
 untyped configuration.
In Reticulated, the baseline is Python running the untyped configuration.
This is not the same as Reticulated running the untyped configuration
 because Reticulated inserts checks in untyped code@~cite{vksb-dls-2014}.

@definition["performance ratio"]{
  A @emph{performance ratio} is the running time of a configuration
   divided by the baseline performance of the untyped configuration.
}

An @emph{exhaustive} performance evaluation measures the performance of every
 configuration.
The natural way to interpret this data is to choose a notion of
 ``good performance'' and count the proportion of ``good'' configurations.
In this spirit, @citet{tfgnvf-popl-2016} ask programmers to consider the
 performance overhead they could deliver to clients.

@definition[@ddeliverable{D}]{
  For @$|{D \in \mathbb{R}^{+}}|, a configuration is @emph{@ddeliverable{D}}
   if its performance ratio is no greater than @${D}.
}

If an exhaustive performance evaluation is infeasible, an alternative is
 to select configurations via simple random sampling and measure the
 proportion of @ddeliverable{D} configurations in the sample.
Repeating this sampling experiment yields a @emph{simple random approximation}
 of the true proportion of @ddeliverable{D} configurations.

@definition[@sraapproximation["r" "s" "95"]]{
  Given @${r} samples each containing @${s} configurations chosen uniformly at random,
   a @emph{@sraapproximation["r" "s" "95"]} is a @${95\%} confidence interval for
   the proportion of @ddeliverable{D} configurations in each sample.
}

The appendix contains mathematical and
 empirical justification for the simple random approximation method.





@;@; -----------------------------------------------------------------------------
@;@section[#:tag "sec:tr-evaluation"]{Application: Typed Racket}
@;
@;This section introduces @integer->word[(count-benchmarks)] @emph{gradual typing performance} (@|GTP|) benchmark programs that are representative of actual user code yet small enough to make exhaustive performance evaluation tractable.
@;The following descriptions of the benchmark programs---arranged by size as measured in number of migratable modules---briefly summarize their relevant characteristics.
@;Each description comes with four fields:
@; @emph{Origin} indicates the benchmark's source,
@; @emph{Purpose} describes what it computes,
@; @emph{Author} credits the original author,
@; and @emph{Depends} lists any libraries of contextual modules that the benchmark depends on.
@;This section concludes with a table summarizing the static characteristics of each benchmark.
@;
@;
@;@parag[]
@;@render-benchmark-descriptions[
@;@(cons sieve
@;  @elem{
@;    Demonstrates a scenario where client code is tightly coupled to higher-order library code.
@;    The library implements a stream data structure; the client builds a stream of prime numbers.
@;    Introducing a type boundary between these modules leads to significant overhead.
@;  })
@;(cons forth
@;  @elem{
@;    Interprets Forth programs.
@;    The interpreter represents calculator commands as a list of first-class objects.
@;    These objects accumulate proxies as they cross type boundaries.
@;  })
@;(cons (list fsm fsmoo)
@;  @elem{
@;    Simulates the interactions of economic agents via finite-state automata@~cite{n-mthesis-2014}.
@;    This benchmark comes in two flavors: @bm[fsm] stores the agents in a mutable vector and whereas @bm[fsmoo] uses a first-class object.
@;  })
@;(cons mbta
@;  @elem{
@;    Builds a map of Boston's subway system and answers reachability queries.
@;    The map encapsulates a boundary to Racket's untyped @library{graph} library; when the map is typed, the (type) boundary to @library{graph} is a performance bottleneck.
@;  })
@;(cons morsecode
@;  @elem{
@;    Computes Levenshtein distances and morse code translations for a fixed sequence of pairs of words.
@;    Every function that crosses a type boundary in @bm[morsecode] operates on strings and integers, thus dynamically type-checking these functions' arguments is relatively cheap.
@;  })
@;(cons zombie
@;  @elem{
@;    Implements a game where players dodge computer-controlled ``zombie'' tokens.
@;    Curried functions over symbols implement game entities and repeatedly cross type boundaries.
@;
@;    @;@racket[
@;    @;  (define-type Point
@;    @;    ((U 'x 'y 'move)
@;    @;     ->
@;    @;     (U (Pairof 'x (-> Real))
@;    @;        (Pairof 'y (-> Real))
@;    @;        (Pairof 'move (Real Real -> Point)))))
@;    @;]
@;
@;  })
@;(cons dungeon
@;  @elem{
@;    Builds a grid of wall and floor objects by choosing first-class classes from a list of ``template'' pieces.
@;    This list accumulates proxies when it crosses a type boundary.
@;
@;    @;Originally, the program imported the Racket @library{math} library
@;    @; for array operations and @library{racket/dict} for a generic dictionary interface.
@;    @;The benchmark uses Racket's vectors instead of the @library{math} library's arrays
@;    @; because Typed Racket v6.2 could not compile the type @racket[(Mutable-Array (Class))] to a contract.
@;    @;The benchmark does not use @library{racket/dict} so that the results for
@;    @; @bm[dungeon] describe internal type boundaries rather than the type
@;    @; boundary to the untyped dict interface.
@;  })
@;(cons zordoz
@;  @elem{
@;    Traverses Racket bytecode (@tt{.zo} files).
@;    The @library{compiler-lib} library defines the bytecode data structures.
@;    Typed code interacting with the library suffers overhead.
@;
@;    @emph{Note}:
@;     the Racket bytecode format changed between versions 6.2 and 6.3 with
@;     the release of the set-of-scopes macro expander@~cite{f-popl-2016}.
@;    This change significantly reduced the overhead of @bm[zordoz].
@;
@;    @;As it turns out, the change from 6.2 to 6.3 improved the typed/untyped ratio
@;    @; from
@;    @; @add-commas[(rnd (typed/untyped-ratio (benchmark-rktd zordoz "6.2")))]x in v6.2 to
@;    @; @add-commas[(rnd (typed/untyped-ratio (benchmark-rktd zordoz "6.3")))]x in v6.3 because
@;    @; the more recent bytecode structures generate less expensive type contracts.
@;    @;The ratio for the newest bytecode format is
@;    @; @add-commas[(rnd (typed/untyped-ratio (benchmark-rktd zordoz "6.5")))]x.
@;  })
@;(cons lnm
@;  @elem{
@;    Renders overhead plots@~cite{tfgnvf-popl-2016}.
@;    Two modules are tightly-coupled to Typed Racket libraries; typing both modules improves performance.
@;  })
@;(cons suffixtree
@;  @elem{
@;    Computes longest common subsequences between strings.
@;    The largest performance overheads are due to a boundary between struct definitions and functions on the structures.
@;  })
@;(cons kcfa
@;  @elem{
@;    Performs 1-CFA on a lambda calculus term that computes @exact|{~$\RktMeta{2*(1+3) = 2*1 + 2*3}$}| via Church numerals.
@;    The (mutable) binding environment flows throughout functions in the benchmark.
@;    When this environment crosses a type boundary, it acquires a new proxy.
@;  })
@;(cons snake
@;  @elem{
@;    Implements the Snake game; the benchmark replays a fixed sequence of moves.
@;    Modules in this benchmark frequently exchange first-order values, such as lists and integers.
@;  })
@;(cons take5
@;  @elem{
@;    Manages a card game between AI players.
@;    These players communicate infrequently, so gradual typing adds relatively little overhead.
@;  })
@;(cons acquire
@;  @elem{
@;    Simulates a board game via message-passing objects.
@;    These objects encapsulate the core data structures; few higher-order values cross type boundaries.
@;  })
@;(cons tetris
@;  @elem{
@;    Replays a pre-recorded game of Tetris.
@;    Frequent type boundary crossings, rather than proxies or expensive runtime checks, are the source of performance overhead.
@;  })
@;(cons synth
@;  @elem{
@;    Converts a description of notes and drum beats to @tt{WAV} format.
@;    Modules in the benchmark come from two sources, a music library and an array library.
@;    The worst overhead occurs when arrays frequently cross type boundaries.
@;  })
@;(cons gregor
@;  @elem{
@;    Provides tools for manipulating calendar dates.
@;    The benchmark builds tens of date values and runs unit tests on these values.
@;  })
@;(cons (list quadBG quadMB)
@;  @elem{
@;    Converts S-expression source code to @tt{PDF} format.
@;    The two versions of this benchmark differ in their type annotations, but have nearly identical source code.
@;
@;    The original version, @bm[quadMB], uses type annotations by the original author.
@;    This version has a high typed/untyped ratio
@;     because it explicitly compiles types to runtime predicates
@;     and uses these predicates to eagerly check data invariants.
@;    In other words, the typed configuration is slower than the untyped configuration because it performs more work.
@;
@;    The second version, @bm[quadBG], uses identical code but weakens types to match the untyped configuration.
@;    This version is therefore suitable for judging the implementation
@;     of Typed Racket rather than the user experience of Typed Racket.
@;    The conference version of this paper included data only for @bm[quadMB].
@;
@;    @; To give a concrete example of different types, here are the definitions
@;    @;  for the core @tt{Quad} datatype from both @bm[quadMB] and @bm[quadBG].
@;    @; @racket[(define-type QuadMB (Pairof Symbol (Listof QuadMB)))]
@;    @; @racket[(define-type QuadBG (Pairof Symbol (Listof Any)))]
@;    @; The former is a homogenous, recursive type.
@;    @; As such, the predicate asserting that a value has type @racket[QuadMB] is a linear-time tree traversal.
@;    @; The predicate for @racket[QuadBG] runs significantly faster.
@;  })
@;]
@;
@;
@;@Figure-ref{fig:bm} tabulates the size and complexity of the benchmark programs.
@;The lines of code (Untyped LOC) and number of migratable modules (# Mod.) approximate program size.
@;The type annotations (Annotation LOC) count additional lines in the typed configuration.
@;These lines are primarily type annotations, but also include type casts and assertions.
@;@;footnote{The benchmarks use more annotations than Typed Racket requires because they give full type signatures for each import. Only imports from untyped modules require annotation.}
@;Adaptor modules (# Adp.) roughly correspond to the number of user-defined datatypes in each benchmark;
@; the next section provides a precise explanation.
@;Lastly, the boundaries (# Bnd.) and exports (# Exp.) distill each benchmark's graph structure.
@;@;footnote{The appendix contains actual module dependence graphs.}
@;Boundaries are import statements from one module to another, excluding imports from runtime or third-party libraries.
@;An identifier named in such an import statement counts as an export.
@;For example, the one import statement in @bm[sieve] names nine identifiers.
@;
@;@figure*["fig:bm" @elem{Static characteristics of the @|GTP| benchmarks}
@;  @render-benchmarks-table{}
@;]
@;
@;
@;@; -----------------------------------------------------------------------------
@;@subsection[#:tag "sec:conversion"]{From Programs to Benchmarks}
@;
@;@(define MIN-ITERS-STR "3")
@;@(define MAX-ITERS-STR "30")
@;@(define FREQ-STR "1.40 GHz")
@;@(define TYPED-BM* (list fsm synth quad))
@;
@;@string-titlecase[@integer->word[(- (*NUM-BENCHMARKS*) (length TYPED-BM*))]] of the benchmark programs are adaptations of untyped programs.
@;The other three benchmarks (@bm[fsm], @bm[synth], and @bm[quad]) use most of the type annotations and code from originally-typed programs.
@;Any differences between the original programs and the benchmarks are due to the following five complications.
@;
@;First, the addition of types to untyped code occasionally requires type casts or small refactorings.
@;For example, the expression @racket[(string->number "42")] has the Typed Racket type @racket[(U Complex #f)].
@;This expression cannot appear in a context expecting an @racket[Integer] without an explicit type cast.
@;As another example, the @bm[quad] programs call a library function to partition a @racket[(Listof (U A B))] into a @racket[(Listof A)] and a @racket[(Listof B)] using a predicate for values of type @racket[A].
@;Typed Racket cannot currently prove that values which fail the predicate have type @racket[B], so the @bm[quad] benchmarks replace the call with two filtering passes.
@;
@;Second, Typed Racket cannot enforce certain types across a type boundary.
@;For example, the core datatypes in the @bm[synth] benchmark are monomorphic because Typed Racket cannot dynamically enforce parametric polymorphism on instances of an untyped structure.
@;
@;Third, any contracts present in the untyped programs are represented as type annotations and in-line assertions in the derived benchmarks.
@;The @bm[acquire] program in particular uses contracts to ensure that certain lists are sorted and have unique elements.
@;The benchmark enforces these conditions with explicit pre and post-conditions on the relevant functions.
@;
@;Fourth, each @emph{static import} of an untyped struct type into typed code generates a unique datatype.
@;Typed modules that share instances of an untyped struct must therefore reference a common static import site.
@;The benchmarks include additional contextual modules, called @emph{adaptor modules}, to provide this canonical import site; for each module @math{M} in the original program that exports a struct, the benchmark includes an adaptor module that provides type annotations for every identifier exported by @math{M}.
@;Adaptor modules add a layer of indirection, but this indirection does not add measurable performance overhead.
@;
@;Fifth, some benchmarks use a different modularization than the original program.
@;The @bm[kcfa] benchmark is modularized according to comments in the original, single-module program.
@;The @bm[suffixtree], @bm[synth], and @bm[gregor] benchmarks each have a single file containing all their data structure definitions; the original programs defined these structures in the same module as the functions on the structures.
@;Lastly, the @bm[quadBG] benchmark has two fewer modules than @bm[quadMB] because it inlines the definitions of two (large) data structures that @bm[quadMB] keeps in separate files.
@;Removing these boundaries has a negligible affect on performance overhead and greatly reduces the number of configurations.
@;
@;
@;@; -----------------------------------------------------------------------------
@;@subsection[#:tag "sec:protocol"]{Experimental Protocol}
@;
@;@(define MIN-ITERS-STR "3")
@;@(define MAX-ITERS-STR "30")
@;@(define FREQ-STR "1.40 GHz")
@;
@;@; @seclink is gross here, but it makes a good PDF
@;Sections @seclink["sec:plots"]{5.2} and @seclink["sec:compare"]{5.3} present the results of an exhaustive performance evaluation of the @integer->word[(*NUM-BENCHMARKS*)] benchmark programs on @integer->word[(length (*RKT-VERSIONS*))] versions of Racket.
@;The data is the result of applying the following protocol for each benchmark and each version of Typed Racket:
@;@itemlist[
@;@item{
@;  Select a random permutation of the configurations in the benchmark.
@;@;footnote{In principle, there is no need to randomize the order, but doing so helps control against possible confounding variables@~cite{mdhs-asplos-2009}.}
@;}
@;@item{
@;  For each configuration: recompile, run once ignoring the result to control against JIT warmup, and run once more and record the running time.
@;  Use the standard Racket bytecode compiler, JIT compiler, and runtime settings.
@;}
@;@item{
@;  Repeat the above steps @exact{$N \ge 10$} times to produce a sequence of @math{N} running times for each configuration.
@;}
@;@item{
@;  Summarize each configuration with the mean of the corresponding running times.
@;}
@;]
@;@; two AMD Opteron 6376 2.3GHz processors and 128GB RAM
@;
@;Specifically, a Racket script implementing the above protocol collected the data in this paper.
@;The script ran on a dedicated Linux machine; this machine has two physical AMD Opteron 6376 processors (with 16 cores each) and 128GB RAM.
@;@;footnote{The Opteron is a NUMA architecture and uses the @tt{x86_64} instruction set.}
@;For the @bm[quadBG] and @bm[quadMB] benchmarks, the script utilized 30 of the machine's physical cores to collect data in parallel.
@;@;footnote{Specifically, the script invoked 30 OS processes, pinned each to a CPU core using the @tt{taskset} Linux command, waited for each process to report a running time, and collected the results.}
@;For all other benchmarks, the script utilized only two physical cores.
@;Each core ran at minimum frequency as determined by the @tt{powersave} CPU governor (approximately @|FREQ-STR|).
@;
@;The online supplement to this paper contains both the experimental scripts and the full datasets.
@;@Secref{sec:threats} reports threats to validity regarding the experimental protocol.
@;The appendix discusses the stability of individual measurements and reports the number of running times for each configuration and version of Racket.
@;
@;
@;@; -----------------------------------------------------------------------------
@;@subsection[#:tag "sec:plots"]{Evaluating Absolute Performance}
@;
@;@(render-lnm-plot
@;  (lambda (pict*)
@;    (define name*
@;      (for/list ([p (in-list pict*)]
@;                 [i (in-naturals)])
@;        (format "fig:lnm:~a" i)))
@;    (define get-caption
@;      (let ([N (length name*)])
@;        (lambda (i) @elem{@|GTP| overhead plots (@id[i]/@id[N])})))
@;    (define NUMV (integer->word (length (*RKT-VERSIONS*))))
@;    (cons
@;      @elem{
@;        @(apply Figure-ref name*) present the results of measuring the benchmark programs in a series of overhead plots.
@;        As in @figure-ref{fig:suffixtree-plot}, the left column of the figures are cumulative distribution functions for @ddeliverable[] configurations and the right column are cumulative distribution functions for @step["1" "D"] configurations.
@;        These plots include data for three versions of Racket released between June 2015 and February 2016.
@;        Data for version 6.2 are thin red curves with short dashes.
@;        Data for version 6.3 are mid-sized green curves with long dashes.
@;        Data for version 6.4 are thick, solid, blue curves.
@;        The typed/untyped ratio for each version appears above each plot in the left column.
@;
@;        @; -- overall, bleak picture
@;        @(let* ([num-bm (length ALL-BENCHMARKS)]
@;                [num-bm-str (integer->word num-bm)]
@;                [num-configs (*TOTAL-NUM-CONFIGURATIONS*)]
@;                [max-str (format "~a" (*MAX-OVERHEAD*))]
@;                [suffixtree-num-modules (integer->word 6)]
@;                [num-max-deliverable 9] ; TODO, use *MAX-OVERHEAD*
@;                [num-max-deliverable-str (integer->word num-max-deliverable)]
@;                [num-mostly-2-deliverable 6]
@;                [num-mostly-2-deliverable-str (integer->word num-mostly-2-deliverable)]
@;                [num-good-slope 8]
@;                [num-good-slope-str (integer->word num-good-slope)]
@;                [v-min (first (*RKT-VERSIONS*))]
@;                [v-max (last (*RKT-VERSIONS*))]
@;                [absolute-min-overhead "1.2"]
@;                [absolute-min-overhead-bm "synth"]
@;                [format-% (lambda (n) (format "~a%" (round (* 100 (/ n num-configs)))))]
@;                [lo (number->string (*LO*))]
@;                [hi (number->string (*HI*))]
@;                [lo-prop (format-% (deliverable* (*LO*) v-max ALL-BENCHMARKS))]
@;                [hi-prop (format-% (- num-configs (deliverable* (*HI*) v-max ALL-BENCHMARKS)))]
@;               )
@;          @elem{
@;            Many curves are quite flat; they demonstrate that gradual typing introduces large and widespread performance overhead in the corresponding benchmarks.
@;            Among benchmarks with fewer than @|suffixtree-num-modules| modules, the most common shape is a flat line near the 50% mark.
@;            Such lines imply that the performance of a group of configurations is dominated by a single type boundary.
@;            @; For instance, there is one type boundary in @bm[fsm] that adds overwhelming slowdown when present; all eight configurations with this boundary have over @|max-str| overhead.
@;            Benchmarks with @|suffixtree-num-modules| or more modules generally have smoother slopes, but five such benchmarks have essentially flat curves.
@;            The overall message is that for many values of @math{D} between 1 and @|max-str|, few configurations are @ddeliverable{}.
@;
@;            For example, in @integer->word[(- num-bm num-mostly-2-deliverable)] of the @|num-bm-str| benchmark programs, at most half the configurations are @ddeliverable{2} on any version.
@;            The situation is worse for lower (more realistic) overheads, and does not improve much for higher overheads.
@;            Similarly, there are ten benchmarks in which at most half the configurations are @ddeliverable{10}.
@;
@;            The curves' endpoints describe the extremes of gradual typing.
@;            The left endpoint gives the percentage of configurations that run at least as quickly as the untyped configuration.
@;            Except for the @bm[lnm] benchmark, such configurations are a low proportion of the total.
@;@;footnote{The @bm[sieve] case is degenerate. Only its untyped and fully-typed configurations are @ddeliverable{1}.}
@;            The right endpoint shows how many configurations suffer over 20x performance overhead.
@;@;footnote{Half the configurations for @bm[dungeon] do not run on versions 6.2 and 6.3 due to a defect in the way these versions proxy first-class classes. The overhead plots report an ``over 20x'' performance overhead for these configurations.}
@;            @string-titlecase[num-max-deliverable-str] benchmarks have at least one such configuration.
@;
@;            Moving from @math{k=0} to @math{k=1} in a fixed version of Racket does little to improve the number of @ddeliverable{} configurations.
@;            Given the slopes of the @math{k=0} plots, this result is not surprising.
@;            One type conversion step can eliminate a pathological boundary, such as those in @bm[fsm] and @bm[zombie], but the overhead in larger benchmarks comes from a variety of type boundaries.
@;            Except in configurations with many typed modules, adding types to one additional module is not likely to improve performance.
@;
@;            In summary, the application of the evaluation method projects a negative image of Typed Racket's sound gradual typing.
@;            Only a small number of configurations in the benchmark suite run with low overhead; a mere @|lo-prop| of all configurations are @ddeliverable[lo] on Racket v@|v-max|.
@;            Many demonstrate extreme overhead; @|hi-prop| of all configurations are not even @ddeliverable[hi] on version @|v-max|.
@;          })
@;      }
@;      (for/list ([p (in-list pict*)]
@;                 [name (in-list name*)]
@;                 [i (in-naturals 1)])
@;        (figure name (get-caption i) p)))))
@;
@;
@;@; -----------------------------------------------------------------------------
@;@subsection[#:tag "sec:compare"]{Evaluating Relative Performance}
@;
@;Although the absolute performance of Racket version 6.4 is underwhelming, it is a significant improvement over versions 6.2 and 6.3.
@;This improvement is manifest in the difference between curves on the overhead plots.
@;For example in @bm[gregor] (third plot in @figure-ref{fig:lnm:3}), version 6.4 has at least as many deliverable configurations as version 6.2 for any overhead on the @math{x}-axis.
@;The difference is greatest near @math{x=2}; in terms of configurations, over 80% of @bm[gregor] configurations are not @ddeliverable{2} on v6.2 but are @ddeliverable{2} on v6.4.
@;The overhead plots for many other benchmarks demonstrate a positive difference between the number of @ddeliverable{D} configurations on version 6.4 relative to version 6.2.
@;
@;The plot of @figure-ref{fig:scale:delta} explicitly shows the improvement of version 6.4 over version 6.2.
@;It consists of @integer->word[(*NUM-BENCHMARKS*)] purple lines, one for each benchmark.
@;These lines plot the difference between the curves for v6.4 and v6.2 on the corresponding overhead plot.
@;For example, the line for @bm[gregor] (labeled @${\mathsf{r}}) demonstrates a large improvement in the number of @ddeliverable{2} configurations.
@;The plot also shows that fifteen of the @integer->word[(*NUM-BENCHMARKS*)] benchmarks significantly benefit from running on version 6.4.
@;Only the line for the @bm[forth] benchmark demonstrates a significant regression;
@; the @bm[zombie] benchmark demonstrates a small regression due to an increase in the cost of type casts.
@;
@;The improved performance of Racket version 6.4 is due to revisions of the contract system and Typed Racket's use of contracts to enforce static types.
@;In particular, the contract system allocates fewer closures to track the labels that Typed Racket uses to report type boundary errors.
@;The regression in the @bm[forth] benchmark is due to a bug in the implementation of class contracts in version 6.2.
@;This bug would suppress the allocation of certain necessary class contracts.
@;With the bug fixed, @bm[forth] generates the contracts but suffers additional performance overhead.
@;
@;@(parameterize ([*RKT-VERSIONS* '("6.2" "6.4")]
@;                [*PLOT-HEIGHT* 180]
@;                [*PLOT-WIDTH* 440]
@;                [*PLOT-FONT-SCALE* 0.02]
@;                [*DELTA-SECTION-ALPHA* 0.5]
@;                [*X-TICK-LINES?* #t])
@; (list
@;  @figure["fig:scale:delta" @elem{Relative performance of v6.4 versus v6.2}
@;    (render-delta ALL-BENCHMARKS)
@;  ]
@; ))
@;
@;@; -----------------------------------------------------------------------------
@;@section[#:tag "sec:scale"]{Evaluation Method, Part II}
@;
@;@(define srs-samples 5)
@;@(define sample-size-factor 10)
@;@(define snake-sample-size (* sample-size-factor (benchmark->num-modules snake)))
@;@(define large-bm* (for/list ([bm (in-list ALL-BENCHMARKS)]
@;                              #:when (< 5 (benchmark->num-modules bm)))
@;                     bm))
@;
@;  @; plot library ~ 80 modules
@;  @; math library ~ 197 modules
@;The evaluation method of @secref{sec:method} does not scale to benchmarks with a large number of migratable modules.
@;Benchmarking a full performance lattice for a program with @math{N} such components requires @exact{$2^N$} measurements.
@;In practice, this limits an exhaustive evaluation of Typed Racket to programs with approximately 20 migratable modules.
@;An evaluation of micro-level gradual typing would be severly limited; depending on the definition of a migratable component, such an evaluation might be limited to programs with 20 functions.
@;
@;Fortunately, simple random sampling can approximate the ground truth presented in @secref{sec:tr}.
@;Instead of measuring every configuration in a benchmark, it suffices to randomly sample a linear number of configurations and plot the overhead apparent in the sample.
@;
@;@Figure-ref{fig:scale:srs-snake} plots the true performance of the @bm[snake] benchmark against confidence intervals@~cite{n-ptrs-1937} generated from random samples.
@;The plot on the left shows the absolute performance of @bm[snake] on version 6.2 (dashed red line) and version 6.4 (solid blue line).
@;The plot on the right shows the improvement of version 6.4  relative to version 6.2 (solid purple line).
@;Each line is surrounded by a thin interval generated from @integer->word[srs-samples] samples of @id[snake-sample-size] configurations each.
@;
@;The plots in @figure-ref{fig:scale:srs-snake} suggest that the intervals provide a reasonable approximation of the performance of the @bm[snake] benchmark.
@;These intervals capture both the absolute performance (left plot) and relative performance (right plot) of @bm[snake].
@;
@;@Figure-ref{fig:scale:delta-interval} provides evidence for the linear sampling suggestion of @figure-ref{fig:scale:srs-snake} using data for the @integer->word[(length large-bm*)] largest benchmarks in the @|GTP| suite.
@;The solid purple lines from @figure-ref{fig:scale:delta} alongside confidence intervals generated from a small number of samples.
@;Specifically, the interval for a benchmark with @math{N} modules is generated from @integer->word[srs-samples] samples of @exact{$@id[sample-size-factor]N$} configurations.
@;Hence the samples for @bm[lnm] use @id[(* 10 (benchmark->num-modules lnm))] configurations and the samples for @bm[quadMB] use @id[(* 10 (benchmark->num-modules quadMB))] configurations.
@;For every benchmark, the true relative performance (solid purple line) lies within the corresponding interval.
@;In conclusion, a language designer can quickly approximate performance by computing a similar interval.
@;
@;
@;@subsection{Statistical Protocol}
@;
@;For readers interested in reproducing the above results, this section describes the protocol that generated @figure-ref{fig:scale:srs-snake}.
@;The details for @figure-ref{fig:scale:delta-interval} are analogous:
@;
@;@itemlist[
@;@item{
@;  To generate one random sample, select @id[snake-sample-size] configurations (10 times the number of modules) without replacement and associate each configuration with its overhead from the exhaustive performance evaluation reported in @secref{sec:tr}.
@;  @; Sampling with replacement yielded similar results.
@;}
@;@item{
@;  To generate a confidence interval for the number of @ddeliverable{D} configurations based on @integer->word[srs-samples] such samples, calculate the proportion of @ddeliverable{D} configurations in each sample and generate a 95% confidence interval from the proportions.
@;  This is the so-called @emph{index method}@~cite{f-arxiv-2006} for computing a confidence interval from a sequence of ratios.
@;  This method is intuitive, but admittedly less precise than a method such as Fieller's@~cite{f-rss-1957}.
@;  The two intervals in the left half of @figure-ref{fig:scale:srs-snake} are a sequence of such confidence intervals.
@;}
@;@item{
@;  To generate an interval for the difference between the number of @ddeliverable{D} configurations on version 6.4 and the number of @ddeliverable{D} configurations on version 6.2, compute two confidence intervals as described in the previous step and plot the largest and smallest difference between these intervals.
@;
@;  In terms of @figure-ref{fig:scale:delta-interval} the upper bound for the number of @ddeliverable{D} configurations on the right half of @figure-ref{fig:scale:srs-snake} is the difference between the upper confidence limit on the number of @ddeliverable{D} configurations in version 6.4 and the lower confidence limit on the number of @ddeliverable{D} configurations in version 6.2.
@;  The corresponding lower bound is the difference between the lower confidence limit on version 6.4 and the upper confidence limit on version 6.2.
@;}
@;]
@;
@;@(parameterize ([*NUM-SIMPLE-RANDOM-SAMPLES* srs-samples]
@;                [*COLOR-OFFSET* 3]
@;                [*RKT-VERSIONS* '("6.2" "6.4")])
@;  @figure["fig:scale:srs-snake" @elem{Approximating absolute performance}
@;    (render-srs-single snake sample-size-factor)
@;  ]
@;)
@;
@;@(parameterize ([*RKT-VERSIONS* '("6.2" "6.4")]
@;                [*PLOT-HEIGHT* 140]
@;                [*PLOT-WIDTH* 440]
@;                [*PLOT-FONT-SCALE* 0.02]
@;                [*DELTA-SECTION-ALPHA* 0.6]
@;                [*NUM-SIMPLE-RANDOM-SAMPLES* srs-samples]
@;                [*TICKS-START-FROM* (- (length ALL-BENCHMARKS) (length large-bm*))])
@; (list
@;  @figure["fig:scale:delta-interval" @elem{Approximating relative performance}
@;      (render-delta large-bm* #:sample-factor sample-size-factor #:sample-style 'interval)
@;  ]
@; ))


@;@; -----------------------------------------------------------------------------
@;@section[#:tag "sec:rp-evaluation"]{Application: Reticulated Python}
@;
@;@subsection[#:tag "sec:protocol"]{Protocol}
@;
@;@parag{Granularity}
@;The evaluation presented in @section-ref{sec:evaluation} is at the granularity
@; of @emph{function and class fields}.
@;One syntactic unit in the experiment is either one function,
@; one method, or the collection of all fields for one class.
@;The class in @figure-ref{fig:cash}, for example, has 3 syntactic units at this granularity.
@;
@;
@;@parag{Benchmark Creation}
@;To convert a Reticulated program into a benchmark, we:
@; (1) build a driver module that runs the program and collects timing information;
@; (2) remove any non-determinism or I/O actions;
@;@;footnote{@Integer->word[(length '(aespython futen http2 slowSHA))] benchmarks inadvertantly perform I/O actions, see @section-ref{sec:threats}.}
@; (3) partition the program into migratable and contextual modules; and
@; (4) add type annotations to the migratable modules.
@;We modify any Python code that Reticulated's type
@; system cannot validate, such as code that requires untagged unions or polymorphism.
@;
@;
@;@parag{Data Collection}
@;For benchmarks with at most @$|{2^{17}}| configurations, we conduct an exhaustive
@; evaluation.
@;For larger benchmarks we conduct a simple random approximation using
@; @integer->word[NUM-SAMPLE-TRIALS] samples each containing @${@id[SAMPLE-RATE]\!*\!(F + C)}
@; configurations, where @${F} is the number of functions in the benchmark and
@; @${C} is the number of classes.
@;@emph{Note} the number @id[SAMPLE-RATE] is arbitrary; our goal was to collect
@; as much data as possible in a reasonable amount of time. @emph{End}
@;
@;
@;All data in this paper was produced by jobs we sent
@; to the @emph{Karst at Indiana University}
@;@;footnote{@url{https://kb.iu.edu/d/bezu}} computing cluster.
@;Each job:
@;@itemlist[#:style 'ordered
@;@item{
@;  reserved all processors on one node;
@;}
@;@item{
@;  downloaded fresh copies of @|PYTHON|
@;  and Reticulated (commit @hyperlink["https://github.com/mvitousek/reticulated/commit/e478343ce7c0f2bc50d897b0ad38055e8fd9487d"]{@tt{e478343}}
@;  on the @hyperlink["https://github.com/mvitousek/reticulated"]{@tt{master}} branch);
@;}
@;@item{
@;  repeatedly:
@;  selected a random configuration from a random benchmark,
@;  ran the configuration's main module @id[NUM-ITERATIONS] times,
@;  and recorded the result of each run.
@;}
@;]
@;Cluster nodes are IBM NeXtScale nx360 M4 servers with two Intel Xeon E5-2650 v2
@; 8-core processors, 32 GB of RAM, and 250 GB of local disk storage.
@;
@;@; -----------------------------------------------------------------------------
@;@section[#:tag "sec:evaluation"]{Performance Evaluation}
@;
@;@(define MAIN-BENCHMARKS (append EXHAUSTIVE-BENCHMARKS SAMPLE-BENCHMARKS))
@;@(define NUM-MAIN-BENCHMARKS (length MAIN-BENCHMARKS))
@;@(define RT (get-ratios-table MAIN-BENCHMARKS))
@;
@;@figure["fig:static-benchmark" "Static summary of benchmarks"
@;  (if CI? (elem) @render-static-information[MAIN-BENCHMARKS])]
@;
@;To assess the run-time cost of gradual typing in Reticulated, we measured
@; the performance of @integer->word[NUM-MAIN-BENCHMARKS] benchmark programs.
@;@(let* ([column-descr*
@;         (list
@;           @elem{lines of code (@bold{SLOC}), }
@;           @elem{number of modules (@bold{M}), }
@;           @elem{number of function and method definitions (@bold{F}), }
@;           @elem{and number of class definitions (@bold{C}).})]
@;        [num-col @integer->word[(length column-descr*)]]
@;       ) @elem{
@;  @Figure-ref{fig:static-benchmark} tabulates information about the size and
@;   structure of the @defn{migratable} portions of these benchmarks.
@;  The @|num-col| columns report the @|column-descr*|
@;  @Section-ref{sec:appendix:benchmarks} of the appendix
@;   describes the benchmarks' origin and purpose.
@;})
@;
@;The following three subsections present the results of the evaluation.
@;@Section-ref{sec:ratio} reports the performance of the untyped
@; and fully-typed configurations.
@;@Section-ref{sec:overhead} plots the proportion of @ddeliverable{D}
@; configurations for @${D} between @${1} and @${@id[MAX-OVERHEAD]}.
@;@Section-ref{sec:exact} compares the number of type annotations in each
@; configuration to its performance.
@;
@;
@;@subsection[#:tag "sec:ratio"]{Performance Ratios}
@;
@;The table in @figure-ref{fig:ratio} lists the extremes of gradual typing in
@; Reticulated.
@;From left to right, these are:
@; the performance of the untyped configuration relative to the Python baseline (the @emph[u/p-ratio]),
@; the performance of the fully-typed configuration relative to the untyped configuration (the @emph[t/u-ratio]),
@; and the overall delta between fully-typed and Python (the @emph[t/p-ratio]).
@;
@;@(unless CI?
@;  (let* ([futen-row (ratios-table-row RT 'futen)]
@;         [futen-u/p (ratios-row-retic/python futen-row)]
@;         [futen-t/u (ratios-row-typed/retic futen-row)]) @elem{
@;  For example, the row for @bm{futen} reports a @|u/p-ratio| of @${@|futen-u/p|}.
@;  This means that the average time to run the untyped configuration of the
@;   @bm{futen} benchmark using Reticulated was @${@|futen-u/p|} times slower than the
@;   average time of running the same code using Python.
@;  Similarly, the @|t/u-ratio| for @bm{futen} states that the fully-typed configuration
@;   is @${@|futen-t/u|} times slower than the untyped configuration.
@;}))
@;
@;
@;@parag{Conclusions}
@;Migrating a benchmark to
@; Reticulated, or from untyped to fully-typed, always adds performance overhead.
@;The migration never improves performance.
@;The overhead is always within an order-of-magnitude.
@;@(let* ([rp* (map ratios-row-retic/python RT)]
@;        [tr* (map ratios-row-typed/retic RT)]
@;        [count-< (λ (x* n) (length (filter (λ (str) (< (string->number str) n)) x*)))]
@;        [rp-<2 (count-< rp* 2)]
@;        [rp-<3 (count-< rp* 3)]
@;        [rp-<4.5 (count-< rp* 4.5)]
@;        [tr-<2 (count-< tr* 2)]
@;        [tr-<3 (count-< tr* 3)]
@;        [tr-<3.5 (count-< tr* 3.5)]
@;        [num-> (for/sum ([rp (in-list rp*)]
@;                         [tr (in-list tr*)]
@;                         #:when (> (string->number rp) (string->number tr)))
@;                 1)])
@;  (unless (= (length rp*) rp-<4.5)
@;    (raise-user-error 'performance-ratios
@;      "expected all retic/python ratios to be < 4.5, but only ~a of ~a are" rp-<4.5 (length rp*)))
@;  @elem{
@;    Regarding the @|u/p-ratio|s:
@;     @integer->word[rp-<2] are below @${2}x,
@;     @integer->word[(- rp-<3 rp-<2)] are between @${2}x and @${3}x, and
@;     the remaining @integer->word[(- rp-<4.5 rp-<3)] are below @${4.5}x.
@;    The @|t/u-ratio|s are typically lower:
@;      @integer->word[tr-<2] are below @${2}x,
@;      @integer->word[(- tr-<3 tr-<2)] are between @${2}x and @${3}x,
@;      and the final @integer->word[(- tr-<3.5 tr-<3)] are below @${3.5}x.
@;
@;    @Integer->word[num->] benchmarks have larger @|u/p-ratio|s than @|t/u-ratio|s.
@;    Given that an untyped Reticulated program offers the same safety guarantees
@;     as Python, it is surprising that the @|u/p-ratio|s are so large.
@;})
@;
@;@figure["fig:ratio" "Performance ratios"
@;  (if CI? (elem) @render-ratios-table[RT])
@;]
@;
@;
@;@subsection[#:tag "sec:overhead"]{Overhead Plots}
@;
@;@figure*["fig:overhead" "Overhead plots"
@;  (if CI? (elem) @render-overhead-plot*[MAIN-BENCHMARKS])
@;]
@;
@;@Figure-ref{fig:overhead} summarizes the overhead of gradual typing in the
@; benchmark programs.
@;Each plot reports the percent of @ddeliverable[] configurations (@|y-axis|)
@; for values of @${D} between @${1}x overhead and @${@id[MAX-OVERHEAD]}x overhead (@|x-axis|).
@;The @|x-axes| are log-scaled to focus on low overheads;
@; vertical tick marks appear at @${1.2}x, @${1.4}x, @${1.6}x, @${1.8}x, @${4}x, @${6}x, and @${8}x overhead.
@;
@;The heading above the plot for a given benchmark states the benchmark's name
@; and indicate whether the data is exhaustive or approximate.
@;If the data is exhaustive, this heading lists the number of configurations
@; in the benchmark.
@;If the data is approximate, the heading lists the number of samples
@; and the number of randomly-selected configurations in each sample.
@;
@;@emph{Note} the curves for the approximate data
@; (i.e., the curves for @bm{sample_fsm}, @bm{aespython}, and @bm{stats}) are intervals.
@;For instance, the height of an interval at @${x\!=\!4} is the range of the
@; @sraapproximation[NUM-SAMPLE-TRIALS (format "[~a(F+C)]" SAMPLE-RATE) "95"]
@; for the number of @ddeliverable[4] configurations.
@;These intervals are thin because there is little variance in the proportion
@; of @ddeliverable{D} configurations across the @integer->word[NUM-SAMPLE-TRIALS]
@; samples.
@;@emph{End}
@;
@;
@;@parag{How to Read the Plots}
@;Overhead plots are cumulative distribution functions.
@;As the value of @${D} increases along the @|x-axis|, the number of
@; @ddeliverable{D} configurations is monotonically increasing.
@;The important question is how many configurations are @ddeliverable{D}
@; for low values of @${D}.
@;If this number is large, then a developer who applies gradual typing to a
@; similar program has a large chance that the configuration they arrive at
@; is a @ddeliverable{D} configuration.
@;The area under the curve is the answer to this question.
@;A curve with a large shaded area below it implies that a large number
@; of configurations have low performance overhead.
@;
@;@(let ([d0 "d_0"]
@;       [d1 "d_1"]) @elem{
@;  The second most important aspects of an overhead plot are the two values of @${D}
@;   where the curve starts and ends.
@;  More precisely, if @${h\!:\!\mathbb{R}^+\!\rightarrow\!\mathbb{N}} is a function
@;   that counts the percent of @ddeliverable{D}
@;   configurations in a benchmark, the critical points are the smallest
@;   overheads @${@|d0|, @|d1|} such
@;   that @${h(@|d0|)\!>\!0\%} and @${h(@|d1|)\!=\!100\%}.
@;  An ideal start-value would lie between zero and one; if @${@|d0|\!<\!1} then
@;   at least one configuration runs faster than the Python baseline.
@;  The end-value @${@|d1|} is the overhead of the slowest-running configuration.
@;})
@;
@;Lastly, the slope of a curve corresponds to the likelihood that
@; accepting a small increase in performance overhead increases the number
@; of deliverable configurations.
@;A flat curve (zero slope) suggests that the performance of a group of
@; configurations is dominated by a common set of type annotations.
@;Such observations are no help to programmers facing performance issues,
@; but may help language designers find inefficiencies in their implementation
@; of gradual typing.
@;
@;
@;@parag{Conclusions}
@;Curves in @figure-ref{fig:overhead} typically cover a large area and reach the
@; top of the @|y-axis| at a low value of @${D}.
@;This value is always less than @${@id[MAX-OVERHEAD]}.
@;In other words, every configuration in the
@; experiment is @ddeliverable[MAX-OVERHEAD].
@;For many benchmarks, the maximum overhead is significantly lower.
@;@(let ([num-2-deliv (length '(futen slowSHA fannkuch nbody nqueens pidigits
@;                              take5 stats))]) @elem{
@;  Indeed, @integer->word[num-2-deliv] benchmarks are @ddeliverable[2].})
@;@; TODO 'indeed' is awkward
@;
@;None of the configurations in the experiment run faster than the Python baseline.
@;This is to be expected, given the @|u/p-ratio|s in @figure-ref{fig:ratio} and the
@; fact that Reticulated translates type annotations into run-time checks.
@;
@;@(let ([smooth '(futen http2 slowSHA chaos fannkuch float nbody pidigits
@;                 pystone PythonFlow take5 sample_fsm aespython stats)])
@;  @elem{
@;    @Integer->word[(length smooth)] benchmarks have relatively smooth slopes.
@;    The plots for the other @integer->word[(- NUM-EXHAUSTIVE-BENCHMARKS (length smooth))]
@;     benchmarks have wide, flat segments.
@;    These flat segments are due to functions that are frequently executed
@;     in the benchmarks' traces; all configurations in which one of these functions
@;     is typed incur a significant performance overhead.
@;})
@;
@;@(let* ([NOT-tp '(http2 call_method spectralnorm)]
@;        [num-tp (- NUM-MAIN-BENCHMARKS (length NOT-tp))]
@;        [S-SLOWER (if CI? "" (percent-slower-than-typed "spectralnorm"))]) @elem{
@;  @Integer->word[num-tp] benchmarks are roughly @ddeliverable{T}, where @${T} is
@;   the @|t/p-ratio| listed in @figure-ref{fig:ratio}.
@;In these benchmarks, the fully-typed configuration is one of the slowest configurations.
@;@;Note that these ratios are typically larger than Typed Racket's typed/untyped ratios@~cite{tfgnvf-popl-2016}.
@;The notable exception is @bm{spectralnorm}, in which the fully-typed configuration
@; runs faster than @${@id[S-SLOWER]\%} of all configurations.
@;Unfortunately, this speedup is due to a soundness bug;
#;@;footnote{Bug report: @url{https://github.com/mvitousek/reticulated/issues/36}}
@; in short, the implementation of Reticulated does not type-check the contents of tuples.
@;})
@;
@;
@;@subsection[#:tag "sec:exact"]{Absolute Running Times}
@;
@;@figure*["fig:exact" "Running time (in seconds) vs. Number of typed components"
@;  (if CI? (elem) @render-exact-runtime-plot*[MAIN-BENCHMARKS])
@;]
@;
@;Since changing the type annotations in a Reticulated program changes its
@; performance, the language should provide a cost model to help developers
@; predict the performance of a given configuration.
@;The plots in @figure-ref{fig:exact} demonstrate that a simple heuristic
@; works well for these benchmarks: the performance of a configuration is
@; proportional to the number of type annotations in the configuration.
@;
@;@parag{How to Read the Plots}
@;@Figure-ref{fig:exact} contains one point for every run of every
@; configuration in the experiment.
@;(Recall from @section-ref{sec:protocol},
@; the data for each configuration is @id[NUM-ITERATIONS] runs.)
@;Each point compares the number of type annotations in a
@; configuration (@|x-axis|) against its running time, measured in seconds (@|y-axis|).
@;
@;The plots contain many points with both the same number of typed components
@; and similar performance.
@;To reduce the visual overlap between such points, the points for a given
@; configuration are spread across the @|x-axis|; in particular,
@; the @id[NUM-ITERATIONS] points for a configuration with @math{N}
@; typed components lie within the interval @${N\!\pm\!@id[EXACT-RUNTIME-XSPACE]}
@; on the @|x-axis|.
@;
@;For example, @bm{fannkuch} has two configurations: the untyped configuration
@; and the fully-typed configuration.
@;To determine whether a point @${(x,y)} in the plot for @bm{fannkuch} represents
@; the untyped or fully-typed configuration, round @${x} to the nearest integer.
@;
@;
@;@parag{Conclusions}
@;
@;Suppose a programmer starts at an arbitrary configuration and adds some
@; type annotations.
@;The plots in @figure-ref{fig:exact} suggest that this action will affect
@; performance in one of four possible ways, based on trends among the plots.
@;
@;@exact-runtime-category["types make things slow"
@;  '(futen slowSHA chaos float pystone PythonFlow take5 sample_fsm aespython stats)
@;  (λ (num-in-category) @elem{
@;    The plots for @|num-in-category| benchmarks show a gradual increase in
@;     performance overhead as the number of typed components increases.
@;    Typing any function, class, or method adds a small performance overhead.
@;})]
@;
@;@exact-runtime-category[@elem{types make things very slow}
@;  '(call_method call_simple go http2 meteor nqueens spectralnorm Espionage PythonFlow)
@;  (λ (num-in-category) @elem{
@;    @string-titlecase[num-in-category] plots have visible gaps between
@;     clusters of configurations with the same number of types.
@;    Configurations below the gap contain type annotations that impose relatively little
@;     run-time cost.
@;    Configurations above the gap have some common type annotations that
@;     add significant overhead.
@;    Each such gap corresponds to a flat slope in @figure-ref{fig:overhead}.
@;})]
@;
@;@exact-runtime-category[@elem{types are free}
@;  '(fannkuch nbody pidigits)
@;  (λ (num-in-category) @elem{
@;    In @|num-in-category| benchmarks, all configurations have similar performance.
@;    The dynamic checks that enforce tag soundness add insignificant overhead.
@;})]
@;
@;@exact-runtime-category[@elem{types make things fast}
@;  '(call_method spectralnorm)
@;  (λ (num-in-category) @elem{
@;    In @|num-in-category| benchmarks, some configurations
@;     run faster than similar configurations with fewer typed components.
@;    These speedups are the result of two implementation bugs:
@;     (1) Reticulated does not dynamically check the contents of statically-typed tuples,
@;     and (2) for method calls to dynamically-typed objects, Reticulated performs
@;     a run-time check that overlaps with Python's dynamic typing@~cite{vksb-dls-2014}.
@;})]
@;
@;@exact{\smallskip}
@;
@;Overall, there is a clear trend that adding type annotations adds performance
@; overhead.
@;The increase is typically linear.
@;On one hand, this observation may help programmers predict performance issues.
@;On the other hand, the linear increase demonstrates that Reticulated does
@; not use type information to optimize programs.
@;In principle a JIT compiler could generate check-free code if it could infer
@; the run-time type of a variable, but it remains to be seen whether this
@; approach would improve performance in practice.
@;
@;
@;@subsection[#:tag "sec:threats"]{Threats to Validity}
@;
@;We have identified five sources of systematic
@; bias.
@;@(let* ( @; See `src/PyPI-ranking/README.md` to reproduce these claims
@;        [lib-data* '((simplejson 50 "https://github.com/simplejson/simplejson")
@;                     (requests 200 "https://github.com/kennethreitz/requests")
@;                     (Jinja2 600 "https://github.com/pallets/jinja/tree/master/jinja2"))]
@;        [rank-info @elem{PyPI Ranking
@;@;footnote{@url{http://pypi-ranking.info/alltime}}}]
@;        [lib-info (authors*
@;                    (for/list ([ld (in-list lib-data*)]
@;                               [long-style? (in-sequences '(#t)
@;                                                          (in-cycle '(#f)))])
@;                      @elem{
@;                        @(if long-style? "The" "the")
@;                        @hyperlink[(caddr ld)]{@tt[@symbol->string[(car ld)]]}
@;                        library contains over @${@id[(cadr ld)]}@;
@;                        @(if long-style? " functions and methods" "")}))]
@;       ) @elem{
@;  First, the experiment consists of a small suite of benchmarks, and these
@;   benchmarks are rather small.
@;  For example, an ad-hoc sample of the @|rank-info| reveals that even small
@;   Python packages have far more functions and methods than our benchmarks.
@;  @|lib-info|.
@;})
@;
@;Second, the experiment considers one fully-typed configuration per benchmark;
@; however, there are many ways of typing a given program.
@;The types in this experiment may differ from types ascribed by another Python
@; programmer, which, in turn, may lead to different performance overhead.
@;
@;@(let ([missing-types '(take5)]
@;       [retic-limited '(pystone stats)]
@;       [format-bm* (lambda (bm*) (authors* (map bm bm*)))]
@;       @; see also https://github.com/nuprl/retic_performance/issues/55
@;       @;
@;       @; - futen is missing some type annotation(s)
@;       @;   - LazyFqdn missing @fields
@;       @; - call_method is missing some type annotation(s)
@;       @;   - missing @fields, but actually empty
@;       @; - call_method_slots is missing some type annotation(s)
@;       @;   - missing @fields, but actually empty
@;       @; - go uses the Dyn type
@;       @;   - to avoid circular reference
@;       @; - pystone uses the Dyn type
@;       @;   - union type, (U PSRecord None)
@;       @; - take5 is missing some type annotation(s)
@;       @;   - `create_deck`, argument 'deck_size' is unannotated
@;       @;   - same function has optional arguments, so the types ignored
@;       @; - stats is missing some type annotation(s)
@;       @;   - only on the print function
@;       @; - stats uses the Dyn type
@;       @;   - for polymorphism, "rank polymorphism", and union types
@;      ) @elem{
@;  Third, some benchmarks use dynamic typing.
@;  The @bm{take5} benchmark contains one function that accepts optional arguments,
@;   and is therefore dynamically typed.
@;@;footnote{Bug report: @url{https://github.com/mvitousek/reticulated/issues/32}.}
@;  The @bm{go} benchmark uses dynamic typing because Reticulated cannot validate
@;   its use of a recursive class definition.
@;  The @format-bm*[retic-limited] benchmarks use dynamic typing
@;   to overcome Reticulated's lack of untagged union types.
@;})
@;
@;@(let ([use-io* '(aespython futen http2 slowSHA)]) @elem{
@;  Fourth, the @(authors* (map bm use-io*)) benchmarks read from a file
@;   within their timed computation.
@;  We nevertheless consider our results representative.
@;})
@;
@;Fifth, Reticulated supports a finer granularity of type annotations than the
@; experiment considers.
@;Function signatures can leave some arguments untyped, and class field
@; declaractions can omit types for some members.
@;We believe that a fine-grained evaluation would support the
@; conclusions presented in this paper.
@;
@;@subsection[#:tag "sec:appendix:validating"]{Validating the Approximation Method}
@;
@;@Section-ref{sec:method} proposes a so-called @emph{simple random approximation}
@; method for guessing the number of @ddeliverable{D} configurations in a benchmark:
@;
@;@|DEF-APPROX|@;
@;@;
@;@Section-ref{sec:evaluation} instantiates this method using @${r\!=\!@id[NUM-SAMPLE-TRIALS]}
@; samples each containing @${@id[SAMPLE-RATE]\!*\!(F + C)} configurations,
@; where @${F} is the number of functions and methods in the benchmark and
@; @${C} is the number of class definitions.
@;The intervals produced by this method
@; (for the @bm*[SAMPLE-BENCHMARKS] benchmarks) are thin,
@; but the paper does not argue that the intervals are very likely to be accurate.
@;This appendix provides the missing argument.
@;
@;@subsection{Statistical Argument}
@;
@;Let @${d} be a predicate that checks whether a configuration from
@; a fixed program is @ddeliverable{D}.
@;Since @${d} is either true or false for every configuration,
@; this predicate defines a Bernoulli random variable @${X_d} with parameter
@; @${p}, where @${p} is the true proportion of @ddeliverable{D} configurations.
@;Consequently, the expected value of this random variable is @${p}.
@;The law of large numbers therefore states that the average of infinitely
@; many samples of @${X_d} converges to @${p}, the true proportion
@; of deliverable configurations.
@;Convergence suggests that the average of ``enough'' samples is ``close to'' @${p}.
@;The central limit theorem provides a similar guarantee---any sequence of
@; such averages is normally distributed around the true proportion.
@;A @${95\%} confidence interval generated from sample averages is therefore
@; likely to contain the true proportion.
@;
@;
@;@;@subsection{Back-of-the-Envelope Argument}
@;@;
@;@;Suppose a few developers independently apply gradual typing to a program.
@;@;For a fixed overhead tolerance @${D}, some proportion of the developers have
@;@; @ddeliverable{D} configurations.
@;@;There is a remote chance that this proportion coincides with the true proportion
@;@; of @ddeliverable{D} configurations.
@;@;Intuitively, the chance is less remote if the number of developers is large.
@;@;But even for a small number of developers, if they repeat this experiment
@;@; multiple times, then the average proportion of @ddeliverable{D} configurations
@;@; should tend towards the true proportion.
@;@;After all, if the true proportion of @ddeliverable{D} configurations is
@;@; @${10\%} then approximately @${1} in @${10} randomly sampled configurations is
@;@; @ddeliverable{D}.
@;
@;
@;@subsection{Empirical Illustration}
@;
@;@Figure-ref{fig:sample:validate} superimposes the results of simple random
@; sampling upon the exhaustive data for three benchmarks.
@;Specifically, these plots are the result of a two-step recipe:
@;@itemlist[
@;@item{
@;  First, we plot the true proportion of
@;   @ddeliverable{D} configurations for @${D} between @${1}x and @${10}x.
@;  This data is represented by a blue curve; the area under the curve is shaded
@;   blue.
@;}
@;@item{
@;  Second, we plot a
@;   @sraapproximation[NUM-SAMPLE-TRIALS (format "[~a(F+C)]" SAMPLE-RATE) "95"]
@;   as a brown interval.
@;  This is a 95% confidence interval generated from @integer->word[NUM-SAMPLE-TRIALS]
@;   samples each containing @$[(format "~a(F+C)" SAMPLE-RATE)] configurations
@;   chosen uniformly at random.
@;}
@;]
@;
@;@;The intervals accurately enclose the true proportions of @ddeliverable{D} configurations.
@;
@;@figure["fig:sample:validate" @elem{Simple random approximations}
@;  (parameterize ([*PLOT-HEIGHT* 100]
@;                 [*SINGLE-COLUMN?* #true])
@;    (if CI? (elem) @render-validate-samples-plot*[VALIDATE-BENCHMARKS]))
@;]
@;
@;@section[#:tag "sec:appendix:benchmarks"]{Benchmark Descriptions}
@;
@;@(let ([total @integer->word[NUM-EXHAUSTIVE-BENCHMARKS]]
@;       [num1 @Integer->word[(length (list* 'aespython 'stats DLS-2014-BENCHMARK-NAMES))]]
@;       [num2 @Integer->word[(length POPL-2017-BENCHMARK-NAMES)]]
@;       [num3 @integer->word[(length '(Espionage PythonFlow take5 sample_fsm))]]
@;      ) @elem{
@;   @|num1| benchmarks originate from case studies by @citet{vksb-dls-2014}.
@;   @;@;footnote{@|dls-names|.}
@;   @|num2| are from the evaluation by @citet{vss-popl-2017} on programs from
@;   the Python Performance Benchmark Suite.
@;   The remaining @|num3| originate from open-source programs.
@;})
@;
@;The following descriptions credit each benchmark's original author,
@; state whether the benchmark depends on any contextual modules,
@; and briefly summarize its purpose.
@;
@;@; -----------------------------------------------------------------------------
@;@; --- WARNING: the order of benchmarks matters!
@;@; ---  Do not re-order without checking ALL PROSE in this file
@;@; -----------------------------------------------------------------------------
@;
@;@bm-desc["futen"
@;@hyperlink["http://blog.amedama.jp/"]{@tt{momijiame}}
@;@url{https://github.com/momijiame/futen}
@;@list[
@;  @lib-desc["fnmatch"]{Filename matching}
@;  @lib-desc["os.path"]{Path split, path join, path expand, getenv}
@;  @lib-desc["re"]{One regular expression match}
@;  @lib-desc["shlex"]{Split host names from an input string}
@;  @lib-desc["socket"]{Basic socket operations}
@;]]{
@;  Converts an @hyperlink["https://www.openssh.com/"]{OpenSSH} configuration
@;  file to an inventory file for the
@;  @hyperlink["https://www.ansible.com/"]{@emph{Ansiable}} automation framework.
@;  @; 1900 iterations
@;}
@;
@;@bm-desc["http2"
@;@authors[@hyperlink["https://github.com/httplib2/httplib2"]{Joe Gregorio}]
@;@url{https://github.com/httplib2/httplib2}
@;@list[
@;  @lib-desc["urllib"]{To split an IRI into components}
@;]]{
@;  Converts a collection of @hyperlink["https://en.wikipedia.org/wiki/Internationalized_Resource_Identifier"]{Internationalized Resource Identifiers}
@;  to equivalent @hyperlink["http://www.asciitable.com/"]{ASCII} resource
@;  identifiers.
@;  @; 10 iterations
@;}
@;
@;@bm-desc["slowSHA"
@;@authors["Stefano Palazzo"]
@;@url{http://github.com/sfstpala/SlowSHA}
@;@list[
@;  @lib-desc["os"]{path split}
@;]]{
@;  Applies the SHA-1 and SHA-512 algorithms to English words.
@;  @; 1 iteration
@;}
@;
@;@bm-desc["call_method"
@;@authors["The Python Benchmark Suite"]
@;@url{https://github.com/python/performance}
@;@list[]]{
@;  Microbenchmarks simple method calls;
@;  the calls do not use argument lists,
@;  keyword arguments, or tuple unpacking.
@;  @; Consists of @${32*10^5} calls to trivial functions.
@;  @; 1 iteration
@;}
@;
@;@bm-desc["call_simple"
@;@authors["The Python Benchmark Suite"]
@;@url{https://github.com/python/performance}
@;@list[]]{
@;  Same as @bm{call_method}, using functions rather than methods.
@;}
@;
@;@bm-desc["chaos"
@;@authors["The Python Benchmark Suite"]
@;@url{https://github.com/python/performance}
@;@list[
@;  @lib-desc["math"]{Square root}
@;  @lib-desc["random"]{randrange}
@;]]{
@;  Creates fractals using the @hyperlink["https://en.wikipedia.org/wiki/Chaos_game"]{@emph{chaos game}} method.
@;  @; 1 iteration
@;}
@;
@;@bm-desc["fannkuch"
@;@authors["The Python Benchmark Suite"]
@;@url{https://github.com/python/performance}
@;@list[]]{
@;  Implements Anderson and Rettig's microbenchmark.
@;  @;@~cite{ar-lp-1994}.
@;  @; 1 iteration
@;}
@;
@;@bm-desc["float"
@;@authors["The Python Benchmark Suite"]
@;@url{https://github.com/python/performance}
@;@list[
@;  @lib-desc["math"]{Sin, Cos, Sqrt}
@;]]{
@;  Microbenchmarks floating-point operations.
@;  @; 1 iteration (200,000 points)
@;}
@;
@;@bm-desc["go"
@;@authors["The Python Benchmark Suite"]
@;@url{https://github.com/python/performance}
@;@list[
@;  @lib-desc["math"]{sqrt log}
@;  @lib-desc["random"]{randrange random}
@;  "two untyped modules"
@;]]{
@;  Implements the game @hyperlink["https://en.wikipedia.org/wiki/Go_(game)"]{Go}.
@;  This benchmark is split across three files: a @defn{migratable} module that implements
@;  the game board, a @defn{contextual} module that defines constants, and a @defn{contextual} module
@;  that implements an AI and drives the benchmark.
@;  @; 2 iterations
@;}
@;
@;@bm-desc["meteor"
@;@authors["The Python Benchmark Suite"]
@;@url{https://github.com/python/performance}
@;@list[]]{
@;  Solves the Shootout benchmarks meteor puzzle.
@;  @;footnote{@url{http://benchmarksgame.alioth.debian.org/u32/meteor-description.html}}
@;  @; 1 iterations (finds at most 6,000 solutions)
@;}
@;
@;@bm-desc["nbody"
@;@authors["The Python Benchmark Suite"]
@;@url{https://github.com/python/performance}
@;@list[]]{
@;  Models the orbits of Jupiter, Saturn, Uranus, and Neptune.
@;  @; 1 iteration
@;}
@;
@;@bm-desc["nqueens"
@;@authors["The Python Benchmark Suite"]
@;@url{https://github.com/python/performance}
@;@list[]]{
@;  Solves the @hyperlink["https://developers.google.com/optimization/puzzles/queens"]{@math{8}-queens} problem by a brute-force algorithm.
@;  @; 10 iterations
@;}
@;
@;@bm-desc["pidigits"
@;@authors["The Python Benchmark Suite"]
@;@url{https://github.com/python/performance}
@;@list[]]{
@;  Microbenchmarks big-integer arithmetic.
@;  @; 1 iteration (5,000 digits)
@;}
@;
@;@bm-desc["pystone"
@;@authors["The Python Benchmark Suite"]
@;@url{https://github.com/python/performance}
@;@list[]]{
@;  Implements Weicker's @emph{Dhrystone} benchmark.
@;  @;footnote{@url{http://www.eembc.org/techlit/datasheets/ECLDhrystoneWhitePaper2.pdf}}
@;  @; 50,000 iterations
@;}
@;
@;@bm-desc["spectralnorm"
@;@authors["The Python Benchmark Suite"]
@;@url{https://github.com/python/performance}
@;@list[]]{
@;  Computes the largest singular value of an infinite matrix.
@;  @; 10 iterations
@;}
@;
@;@bm-desc["Espionage"
@;@authors["Zeina Migeed"]
@;""
@;@list[
@;  @lib-desc["operator"]{itemgetter}
@;]]{
@;  Implements Kruskal's spanning-tree algorithm.
@;  @; 1 iteration
@;}
@;
@;@bm-desc["PythonFlow"
@;@authors["Alfian Ramadhan"]
@;@url{https://github.com/masphei/PythonFlow}
@;@list[
@;  @lib-desc["os"]{path join}
@;]]{
@;  Implements the Ford-Fulkerson max flow algorithm. 
@;  @; no longer needs citation
@;  @;@~cite{ff-cjm-1956}.
@;  @; 1 iteration
@;}
@;
@;@bm-desc["take5"
@;@authors["Maha Alkhairy" "Zeina Migeed"]
@;""
@;@list[
@;  @lib-desc["random"]{randrange shuffle random seed}
@;  @lib-desc["copy"]{deepcopy}
@;]]{
@;  Implements a card game and a simple player AI.
@;  @; 500 iterations
@;}
@;
@;@bm-desc["sample_fsm"
@;@authors["Linh Chi Nguyen"]
@;@url{https://github.com/ayaderaghul/sample-fsm}
@;@list[
@;  @lib-desc["itertools"]{cycles}
@;  @lib-desc["os"]{path split}
@;  @lib-desc["random"]{random randrange}
@;]]{
@;  Simulates the interactions of economic agents modeled as finite-state automata.
@;  @;@~cite{n-mthesis-2014}.
@;  @; 100 iterations
@;}
@;
@;@bm-desc["aespython"
@;@authors[@hyperlink["http://caller9.com/"]{Adam Newman}
@;         @hyperlink["https://github.com/serprex"]{Demur Remud}]
@;@url{https://github.com/serprex/pythonaes}
@;@list[
@;  @lib-desc["os"]{random stat}
@;  @lib-desc["struct"]{pack unpack calcsize}
@;]]{
@;  @; Second sentence is a little awkward. I just want to say, "this is really
@;  @;  a Python implementation of AES, not just a wrapper to some UNIX implementation"
@;  Implements the @hyperlink["http://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197.pdf"]{Advanced Encryption Standard}.
@;  @;Uses the @tt{os} library only to generate random bytes and invoke the
@;  @; @hyperlink["http://man7.org/linux/man-pages/man2/stat.2.html"]{@tt{stat()}}
@;  @; system call.
@;  @; 1 iteration, encrypts the book of Leviticus (2800 lines)
@;}
@;
@;@bm-desc["stats"
@;@authors[@hyperlink["https://connects.catalyst.harvard.edu/Profiles/display/Person/12467"]{Gary Strangman}]
@;@url{https://github.com/seperman/python-statlib/blob/master/statlib/pstat.py}
@;@list[
@;  @lib-desc["copy"]{deepcopy}
@;  @lib-desc["math"]{pow abs etc.}
@;]]{
@;  Implements first-order statistics functions; in other words, transformations
@;   on either floats or (possibly-nested) lists of floats.
@;  The original program consists of two modules.
@;  The benchmark is modularized according to comments in the program's source
@;   code to reduce the size of each module's configuration space.
@;  @; 1 iteration
@;}

