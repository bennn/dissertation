#lang greenman-thesis/include

@; TODO, outline:
@; - [X] method : space, assumptions, goals, exhaustive, approximate, threats
@;   - [ ] fun, other visualizations from JFP that fell by the wayside
@; - [X] app TR : benchmarks, protocol, data
@; - [X] app RP : benchmarks, protocol, data
@;
@; TODO sort retic benchmarks by N, acknowledge origin somewhere else! re-cache
@;
@; extra viz:
@;  JFP appendix, heartbeat plots, ....


@(require
   (prefix-in tr: greenman-thesis/jfp-2019/main)
   (prefix-in rp: greenman-thesis/pepm-2018/main)
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
    "Matthias Felleisen"
  )
  #:paper* '("gtnffvf-jfp-2019" "gm-pepm-2018" "tfgnvf-popl-2016")
]

@disclaimer{
  The Typed Racket data in this chapter is based on an early version
   of the GTP benchmarks@~cite{gtnffvf-jfp-2019}.
  These benchmarks have severe limitations that were later fixed,
   thanks to work by: Sam Tobin-Hochstadt, Lukas Lazarek, Sam Sundar, and
   Cameron Moy.

  Up-to-date benchmarks with release notes are here:

  @centered{@format-url{https://docs.racket-lang.org/gtp-benchmarks/index.html}}
}

@; sound GT has cost
Sound gradual types come with performance overhead.
A soundness theorem guarantees that static types are valid claims about the
 run-time behavior of a program.
Gradual types can mix with untyped code.
Therefore, a gradual typing system needs run-time checks to protect typed
 code from invalid untyped values.
These run-time checks impose a cost in proportion to the frequency
 of typed/untyped interactions, the nature of the data that crosses
 these type boundaries, and the strength of the soundness guarantee.

@; must measure
Language designers must measure the overhead of gradual types
 to judge the overall usefulness of a gradual typing system.
 @; ... benefits vs costs
Users need to know what kind of performance to expect.
Implementors need a baseline to quantify improvements and
 justify alternative gradual typing approaches.

This chapter presents a systematic and scalable evaluation method that I
 developed with several collaborators---notably Asumu Takikawa and Zeina
 Migeed.
The method summarizes performance for the exponentially-many ways that a programmer
 can mix typed and untyped code by focusing on a binary "goodness" measure.
Informally, a mixture is good if it is fast enough to use.
Random sampling can approximate the proportion of good mixtures for programs
 in which exhaustive evaluation is not practical.


@section{Design Criteria}
@; are there good words, from Asumu?, we can use to talk about criteria/requirements?
@; "background" assumptions

@; - benchmarks, "representative"
@; - space = 2^N configurations
@; - goal = % deliverable
@;   -- baseline = no gradual typing

The goal of performance evaluation is to predict the experiences of future
 users.
Such users likely agree with the migration plan listed in @chapter-ref{chap:why}
 and are eager to test new typed/untyped combinations.
Experience with Typed Racket indicates that this is exactly what happens;
 programmers combine typed and untyped code any way that the system allows.
Applications undergo such transitions in a gradual, step-by-step manner.
In a typical evolution, programmers compare the performance of the modified
 program with the previous version.
If type-driven optimizations result in a performance improvement, all is well.
Otherwise, the programmer may need to address the overhead with additional
 modifications.

These observations and assumptions about users suggest three basic criteria
 for an evaluation method.

@parag{Begin with representative benchmarks}
An evaluation method must measure programs, and is in turn limited by the
 chosen benchmarks.
We need a variety of benchmarks from which to draw conclusions.
Where possible, benchmarks should be adapted from real programs.
Otherwise, benchmarks should stress-test specific parts of the language.


@parag{Summarize an exponentially-large space}
@(let ((num-example-configs 5)) (list @elem{
Gradual typing promises to support exponentially-many combinations of typed
 and untyped code.
Using Typed Racket, a programmer can add types to any module; thus a program
 with @id[num-example-configs] modules leads to @${2^@id[num-example-configs]}
 possible combinations (@figure-ref{fig:example-lattice-0}).
Languages that offer types at a finer granularity have even more possibilities.
In general, an evaluation must treat each combination as equally-likely.
}

@figure["fig:example-lattice-0" @elem{
  A Racket program with @id[num-example-configs] modules supports @id[(expt 2 num-example-configs)]
  mixed-typed configurations, including the fully-untyped and fully-typed versions.
  }
  (parameterize ([*LATTICE-CONFIG-X-MARGIN* 6]
                 [*LATTICE-CONFIG-Y-MARGIN* 10]
                 [*FONT-SIZE* 12]
                 [*LATTICE-UNIT-HEIGHT* 6]
                 [*LATTICE-UNIT-WIDTH* 7]
                 [*LATTICE-UNIT-X-MARGIN* 0]
                 [*LATTICE-UNIT-BORDER-WIDTH* 0]
                 [*LATTICE-LINES?* #true]
                 [*LATTICE-CONFIG-LABEL-MARGIN* 1])
    (configuration-lattice num-example-configs))
]
))


@parag{Focus on overhead}
@(let ((example-overhead 8)) @elem{
Because a gradual program starts from an original program, the first question
 is how adding types changes performance.
Hence we can always assume two versions for any program: the current
 typed/untyped configuration and the same code without any gradual types.
The latter @emph{baseline} program helps anchor an evaluation.
If a programmer can tolerate a certain overhead, say @id[example-overhead]x,
 then a single number can summarize the good parts of the exponentially-large
 configuation space; namely, the percent of configurations that run fast enough.
})



@; -----------------------------------------------------------------------------
@section{Exhaustive Evaluation Method}

We have developed two methods to measure the performance of a gradually-typed
 program: an exhaustive method that considers the entire configuration space,
 and an approximation thereof.
This section presents the exhaustive method---first by example and then more
 formally.


@subsection{By Example}

A Typed Racket program is a collection of modules (@figure-ref{fig:example-program}).
Some modules are under the direct control of the programmer;
 these migratable modules are often identified with the program.
Other modules are controlled by different programmers.
These contextual modules may come from the standard library or
 a package registry.

@figure*["fig:example-program" @elem{
  Example Typed Racket program.
  The four @emph{migratable} modules on the left are under the programmer's control.
  The two @emph{contextual} modules on the right represent libraries and core
   language APIs.
  }
  @; TODO white = untyped, black = typed, NON-GRAY = contextual ... pick white or black!
  (make-example-program-pict)
]

Gradual typing in Typed Racket lets a programmer choose the language
 for each migratable module.
Thus a program with @${N} migratable modules opens a space of @${2^N}
 possibilites (@figure-ref{fig:example-lattice-0})---and no more, because the
 contextual modules are not easily open to modification.

@(let* ((lattice-version "6.4")
        (relative-version "6.2")
        (S (tr:benchmark-name->performance-info 'fsm lattice-version))
        (num-modules (tr:benchmark->num-modules tr:fsm))
       ) @list[
@elem{
An exhaustive performance evaluation must consider all @${2^N} possibilities.
After all, a programmer might choose any one; there is no reason why
 not.
To give one example, the performance lattice in @figure-ref{fig:example-lattice}
 shows this full dataset for a program named @bm{fsm} that has @id[num-modules]
 migratable modules.
The @integer->word[(expt 2 num-modules)] configurations are arranged in a
 lattice with the fully-untyped configuration on the bottom and the fully-typed
 configuration on top.
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
}
@figure*["fig:example-lattice" @elem{
Performance overhead in @bm{fsm}, on Racket v@|lattice-version|.}
  (parameterize ([*LATTICE-CONFIG-X-MARGIN* 10]
                 [*LATTICE-CONFIG-Y-MARGIN* 10]
                 [*FONT-SIZE* 12]
                 [*LATTICE-UNIT-HEIGHT* 8]
                 [*LATTICE-UNIT-WIDTH* 12]
                 [*LATTICE-UNIT-X-MARGIN* 0]
                 [*LATTICE-UNIT-BORDER-WIDTH* 0]
                 [*LATTICE-LINES?* #true]
                 [*LATTICE-CONFIG-LABEL-MARGIN* 1])
    (performance-lattice S))
]
@elem{
Configurations in @figure-ref{fig:example-lattice} are furthermore labeled
 with their performance overhead relative to the untyped configuration on Racket
 version @|lattice-version|.
With these labels, a language implementor can draw several conclusions about
 the performance overhead of gradual typing in this program.
A first observation is that the fully-typed code runs the same speed as untyped.
This 1x overhead is also the overall best point in the lattice.
Six other configurations run within a 2x overhead, but the rest suffer
 from orders-of-magnitude slowdowns.
Gradual typing in @bm{fsm} can come at a huge cost.
}
@elem{
Drawing such conclusions is not easy, however, even for this small program.
Analyzing a lattice for programs with eight or more modules is clearly infeasible.
@Figure-ref{fig:overhead-plot-example} presents a graphical alternative,
 an @emph{overhead plot},
 that anonymizes configurations and focuses on overhead relative to untyped.
@; TODO awkward, haven't introduced ddeliv yet!
Overhead plots are cumulative distribution functions.
As the value of @${D} increases along the @|x-axis|, the number of
 @ddeliverable{D} configurations is monotonically increasing.
The important question is how many configurations are @ddeliverable{D}
 for low values of @${D}.
If this number is large, then a developer who applies gradual typing to a
 similar program has a large chance that the configuration they arrive at
 is a @ddeliverable{D} configuration.
The area under the curve is the answer to this question.
A curve with a large shaded area below it implies that a large number
 of configurations have low performance overhead.

@(let ([d0 "d_0"]
       [d1 "d_1"]) @elem{
  The second most important aspects of an overhead plot are the two values of @${D}
   where the curve starts and ends.
  More precisely, if @${h\!:\!\mathbb{R}^+\!\rightarrow\!\mathbb{N}} is a function
   that counts the percent of @ddeliverable{D}
   configurations in a benchmark, the critical points are the smallest
   overheads @${@|d0|, @|d1|} such
   that @${h(@|d0|)\!>\!0\%} and @${h(@|d1|)\!=\!100\%}.
  An ideal start-value would lie between zero and one; if @${@|d0|\!<\!1} then
   at least one configuration runs faster than the Python baseline.
  The end-value @${@|d1|} is the overhead of the slowest-running configuration.
})

Lastly, the slope of a curve corresponds to the likelihood that
 accepting a small increase in performance overhead increases the number
 of deliverable configurations.
A flat curve (zero slope) suggests that the performance of a group of
 configurations is dominated by a common set of type annotations.
Such observations are no help to programmers facing performance issues,
 but may help language designers find inefficiencies in their implementation
 of gradual typing.
}
@render-overhead-plot*[
  "fig:overhead-plot-example"
  @elem{
Overhead plot for @bm{fsm}, on Racket v@|lattice-version|.
The unlabeled vertical ticks mark, from left-to-right:
1.2x, 1.4x, 1.6x, 1.8x, 4x, 6x, 8x, 10x, 12x, 14x, 16x, and 18x.
  }
  ""
  tr:render-overhead-plot
  '(fsm)
  #f
]
@render-overhead-plot*[
  "fig:relative-overhead-plot-example"
  @elem{
Overhead plots for @bm{fsm}, on Racket v@|lattice-version|
 and v@|relative-version|.
The blue curve for v@|lattice-version| is higher, showing a relative improvement.
  }
  ""
  tr:render-relative-overhead-plot
  (list (cons 'fsm (cons lattice-version relative-version)))
  #f
]
@elem{
An overhead plot in the style of @figure-ref{fig:overhead-plot-example}
 scales to arbitrarily large programs because the @|y-axis|
 plots the proportion of @ddeliverable{D} configurations; in contrast, a
 performance lattice contains exponentially many nodes.
Furthermore, plotting the overhead for multiple implementations of a gradual
 type system on the same set of axes conveys their relative performance
 (@figure-ref{fig:relative-overhead-plot-example}).
}
])

@; TODO more words here?

To summarize, the exhaustive method has three steps:
@itemlist[#:style 'ordered
 @item{
   identify a suite of fully-typed programs;
 }
 @item{
   measure the performance of all gradually-typed @emph{configurations} of the programs;
 }
 @item{
   count the number of configurations with performance overhead no greater than a certain limit.
 }
]


@subsection{By Definition}

The following definitions capture the intuitions above and generalize
 beyond Typed Racket.
As a running example, this section will use Reticulated Python to demonstrate
 the generalization.
Reticulated permits optional types for every parameter to a function,
 every function return type, and every class field.

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


@subsection{Threats and Limitations}

Fixed types.
@; TODO

Granularity doesn't go inside type, to allow List(Dyn) etc.
@; TODO

Plots in the style of @figure-ref{fig:overhead-plot-example} rest on two
 assumptions.
Readers must keep these in mind as they
 interpret the results.

@; - assn: log scale
The first assumption is that configurations with less than 2x overhead
 are significantly more practical than configurations with a 10x overhead or more.
Hence the plots use a log-scaled x-axis to simultaneously encourage
 fine-grained comparison in the 1.2x to 1.6x overhead range and blur the
 distinction between, e.g., 14x and 18x slowdowns.

@; - assn: 20x
The second assumption is that configurations with more than @id[tr:MAX-OVERHEAD]x
 overhead are completely unusable in practice.
Pathologies like the 100x slowdowns in @figure-ref{fig:example-lattice}
 represent a challenge for implementors, but if these overheads suddenly
 dropped to 30x, the configurations would still be useless to developers.


@section{Approximate Evaluation Method}

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

Intuitively, this sampling method should lead to good results because
 it randomly samples a stable population.
If the true proportion of @ddeliverable{D} configurations in a program
 happens to be 10%, then one random selection has a 1 in 10 chance of
 drawing a @ddeliverable{D} configuration.

A statistical justification depends on the law of large numbers the
 central limit theorem.
Let @${d} be a predicate that checks whether a configuration
 is @ddeliverable{D}.
Since @${d} is either true or false for every configuration,
 this predicate defines a Bernoulli random variable @${X_d} with parameter
 @${p}, where @${p} is the true proportion of @ddeliverable{D} configurations.
Consequently, the expected value of this random variable is @${p}.
The law of large numbers states that the average of @emph{infinitely}
 many samples of @${X_d} converges to @${p}, the true proportion
 of deliverable configurations.
We cannot draw infinitely many samples, but perhaps this convergence
 property means that the average of ``enough'' samples is ``close'' to @${p}.
Indeed, the central limit theorem guarantees that any sequence of
 such averages is normally distributed around the true proportion.
A @${95\%} confidence interval generated from sample averages is therefore
 likely to contain the true proportion.

The statistical argument reveals two weaknesses in the sampling method.

@itemlist[
@item{
  First, there is no guarantee that every confidence interval based on sampling
   contains the true proportion of @ddeliverable{D} configurations.
  The results can mislead.
}
@item{
  Second, the confidence intervals could be huge.
  A wide interval offers little insight, even if it happens to contain the
   true proportion.
  (In the extreme, a useless interval says that 0% to 100% of configurations
   are @ddeliverable{D}.)
}
]

The argument does say, however, that an interval is more likely to be useful
 if based on a huge number of samples, each sample containing a huge number
 of configurations.
Question is, how well does a manageable experiment size work in practice?

@Figure-ref["fig:tr:validate-sample" "fig:rp:validate-sample"] test the
 @sraapproximation["r" "s" "95"] method using 
 @${r\!=\!@id[NUM-SAMPLE-TRIALS]} samples each containing a linear
 number of configurations.
More precisely, each sample contains @${@id[SAMPLE-RATE]\!*\!N} configurations
 out of the @${2^N} possibilities in the benchmark at hand.
These plots show both the true data and the result of sampling:
 a solid blue line shows the true proportion of @ddeliverable{D} configurations,
 and an orange interval shows the result of approximation.
Don't worry about the interpretation of the blue lines for now,
 the application sections go into great detail.

@render-overhead-plot*[
  "fig:tr:validate-sample"
  @elem{Typed Racket sample validation.}
  ""
  tr:render-validate-plot
  (map tr:benchmark-name (take-right tr:ALL-BENCHMARKS overhead-plots-per-page))
  tr:cache-dir
]

@render-overhead-plot*[
  "fig:rp:validate-sample"
  @elem{Reticulated sample validation.}
  ""
  rp:render-validate-plot
  (take rp:VALIDATE-BENCHMARKS overhead-plots-per-page)
  rp:cache-dir
]

In each plot, the sample intervals are both thin and capture the true
 proportion.
Thus a linear number of samples appears sufficient.


@subsection{Statistical Protocol}

For readers interested in reproducing the above results, this section describes
 the protocol that generated @figure-ref["fig:tr:validate-sample" "fig:rp:validate-sample"].

To generate one random sample, select @${@id[SAMPLE-RATE]\!*\!N} configurations
 without replacement and compute their overhead.
We have tried sampling with replacement and found similar results.

To generate a confidence interval for the number of @ddeliverable{D}
 configurations based on a group of samples, calculate
 the proportion of @ddeliverable{D} configurations in each sample and generate
 a 95% confidence interval from the proportions.
This is the simple @emph{index method} for computing a
 confidence interval from a sequence of ratios (@format-url{https://arxiv.org/pdf/0710.2024v1.pdf}).
We have not experimented with a more precise method such as
 Fieller's@~cite{f-rss-1957}.


@section{Benchmark Selection}

Our approach to benchmarks.
As large as possible.
As real as possible.
To this end, "origin" below and credits to original programs author.
Some however from benchmark suite chosen because prior Python evaluation.


@subsection[#:tag "sec:tr:conversion"]{From Programs to Benchmarks}

To convert a Reticulated program into a benchmark, we:
 (1) build a driver module that runs the program and collects timing information;
 (2) remove any non-determinism or I/O actions;
 (3) partition the program into migratable and contextual modules; and
 (4) add type annotations to the migratable modules.
That said, @integer->word[(length '(aespython futen http2 slowSHA))]
 benchmarks inadvertantly perform I/O actions, see @section-ref{sec:rp:threats}.
We modify any Python code that Reticulated's type
 system cannot validate, such as code that requires untagged unions or polymorphism.


@(let* ((TYPED-BM (map bm '(fsm synth quadMB)))
        (num-typed-bm (length TYPED-BM))) @elem{
@string-titlecase[@integer->word[(- (tr:*NUM-BENCHMARKS*) num-typed-bm)]] of
 the benchmark programs are adaptations of untyped programs.
The other @integer->word[num-typed-bm] benchmarks
 (@oxfordize[TYPED-BM]) use most of
 the type annotations and code from originally-typed programs.
Any differences between the original programs and the benchmarks are due to the
 following five complications.
})

First, the addition of types to untyped code occasionally requires type casts or small refactorings.
For example, the expression @tt{(string->number "42")} has the Typed Racket
 type @tt{(U Complex #f)}.
This expression cannot appear in a context expecting an @tt{Integer} without an
 explicit type cast.
As another example, the @bm{quad} programs call a library function to partition
 a @tt{(Listof (U A B))} into a @tt{(Listof A)} and a
 @tt{(Listof B)} using a predicate for values of type @tt{A}.
Typed Racket cannot currently prove that values which fail the predicate have
 type @tt{B}, so the @bm{quad} benchmarks replace the call with two
 filtering passes.

Second, Typed Racket cannot enforce certain types across a type boundary.
For example, the core datatypes in the @bm{synth} benchmark are monomorphic
 because Typed Racket cannot dynamically enforce parametric polymorphism on
 instances of an untyped structure.

Third, any contracts present in the untyped programs are represented as type
 annotations and in-line assertions in the derived benchmarks.
The @bm{acquire} program in particular uses contracts to ensure that certain
 lists are sorted and have unique elements.
The benchmark enforces these conditions with explicit pre and post-conditions
 on the relevant functions.

Fourth, each @emph{static import} of an untyped struct type into typed code
 generates a unique datatype.
Typed modules that share instances of an untyped struct must therefore
 reference a common static import site.
The benchmarks include additional contextual modules, called @emph{adaptor
 modules}, to provide this canonical import site; for each module @${M} in the
 original program that exports a struct, the benchmark includes an adaptor
 module that provides type annotations for every identifier exported by @${M}.
Adaptor modules add a layer of indirection, but this indirection does not add
 measurable performance overhead.

Fifth, some benchmarks use a different modularization than the original program.
The @bm{kcfa} benchmark is modularized according to comments in the original,
 single-module program.
The @bm{suffixtree}, @bm{synth}, and @bm{gregor} benchmarks each have a single
 file containing all their data structure definitions; the original programs
 defined these structures in the same module as the functions on the structures.
Lastly, the @bm{quadBG} benchmark has two fewer modules than @bm{quadMB}
 because it inlines the definitions of two (large) data structures that
 @bm{quadMB} keeps in separate files.
Removing these boundaries has a negligible affect on performance overhead and
 greatly reduces the number of configurations.



@; -----------------------------------------------------------------------------
@section[#:tag "sec:tr:evaluation"]{Application 1: Typed Racket}

As a first validation of the method, we measured Typed Racket.
This evaluation proved that the method can give useful summaries and
 helped us refine our visualizations.
It also revealed significant challenges for Typed Racket's @|sguarded|
 approach to gradual typing.
Overheads of several orders of magnitude were common.


@subsection[#:tag "sec:tr:protocol"]{Protocol}

@parag{Granularity}
The granularity of this evaluation is @emph{modules}, same as the granularity
 of Typed Racket.
One syntactic unit in the experiment is one entire module.
Converting one unit requires changing the language to Typed Racket and
 adding type annotations throughout.


@parag{Data Collection}

For each benchmark and each version of Typed Racket, we apply the following protocol to collect data:
@itemlist[
@item{
  Select a random permutation of the configurations in the benchmark.
  In principle, there is no need to randomize the order, but doing so helps
   control against possible confounding variables@~cite{mdhs-asplos-2009}.
}
@item{
  For each configuration: recompile, run once ignoring the result to control
   against JIT warmup, and run once more and record the running time.
  Use the standard Racket bytecode compiler, JIT compiler, and runtime settings.
}
@item{
  Repeat the above steps @${N \ge 10} times to produce a sequence of
   @${N} running times for each configuration.
}
@item{
  Summarize each configuration with the mean of the corresponding running times.
}
]

@; two AMD Opteron 6376 2.3GHz processors and 128GB RAM
@(let ((FREQ-STR "1.40 GHz")
       (num-processors 2)
       (num-proc-cores 16)
       (processor-name "AMD Opteron 6376")
       (proc-speed "2.3GHz")
       (proc-ram "128 GB")) @elem{
Specifically, @hyperlink["https://github.com/nuprl/gradual-typing-performance/blob/master/tools/benchmark-run/run.rkt"]{a Racket script}
 implementing the above protocol collected the data in this paper.
The script ran on a dedicated Linux machine with two physical
 @id[processor-name] processors (@id[num-proc-cores] cores each) and @id[proc-ram] RAM.
The Opteron is a NUMA architecture and uses the @tt{x86_64} instruction set.
For the @bm{quadBG} and @bm{quadMB} benchmarks, the script utilized 30 of the
 machine's physical cores to collect data in parallel.
Specifically, the script invoked 30 OS processes, pinned each to a CPU core
 using the @tt{taskset} Linux command, waited for each process to report a
 running time, and collected the results.
For all other benchmarks, the script utilized two physical cores.
Each core ran at minimum frequency as determined by the @tt{powersave} CPU
 governor (approximately @|FREQ-STR|).
})


@subsection[#:tag "sec:tr:benchmarks"]{Benchmarks}

This section introduces @integer->word[tr:num-benchmarks] @emph{gradual typing
 performance} (@|GTP|) benchmark programs that are representative of actual user
 code yet small enough to make exhaustive performance evaluation tractable.
The following descriptions of the benchmark programs---arranged by size as
 measured in number of migratable modules---briefly summarize their relevant
 characteristics.
Each description comes with four fields:
 @emph{Origin} indicates the benchmark's source,
 @emph{Purpose} describes what it computes,
 @emph{Author} credits the original author,
 and @emph{Depends} lists any libraries of contextual modules that the benchmark depends on.
This section concludes with a table summarizing the static characteristics of each benchmark.

@; WARNING: benchmark order matters
@; TODO wrapper function, to sort and check for missing?
@exact{\bigskip}
@bm-desc[
  @bm{sieve}
  #:author "Ben Greenman"
  #:origin "Synthetic"
  #:purpose "Generate prime numbers"]{
    Demonstrates a scenario where client code is tightly coupled to higher-order library code.
    The library implements a stream data structure; the client builds a stream of prime numbers.
    Introducing a type boundary between these modules leads to significant overhead.
}
@bm-desc[
  @bm{forth}
  #:author "Ben Greenman"
  #:origin "Library"
  #:purpose "Forth interpreter"
  #:url "http://docs.racket-lang.org/forth/index.html"]{
  Interprets Forth programs.
  The interpreter represents calculator commands as a list of first-class objects.
  These objects accumulate proxies as they cross type boundaries.
}
@bm-desc[
  (list @bm{fsm} @bm{fsmoo})
  #:author "Linh Chi Nguyen"
  #:origin "Economics research"
  #:purpose "Economy simulator"
  #:url "https://github.com/mfelleisen/sample-fsm"]{
  Simulates the interactions of economic agents via finite-state automata@~cite{na-eai-2016}.
  This benchmark comes in two flavors: @bm{fsm} stores the agents in a mutable vector and @bm{fsmoo} uses a first-class object.
}
@bm-desc[
  @bm{mbta}
  #:author "Matthias Felleisen"
  #:origin "Educational"
  #:purpose "Interactive map"
  #:depends (list (make-lib "graph" "http://github.com/stchang/graph"))]{
  Builds a map of Boston's subway system and answers reachability queries.
  The map encapsulates a boundary to Racket's untyped @library{graph} library;
   when the map is typed, the (type) boundary to @library{graph} is a
   performance bottleneck.
}
@bm-desc[
  @bm{morsecode}
  #:author "John Clements and Neil Van Dyke"
  #:origin "Library"
  #:purpose "Morse code trainer"
  #:url "https://github.com/jbclements/morse-code-trainer/tree/master/morse-code-trainer"]{
  Computes Levenshtein distances and morse code translations for a fixed sequence of pairs of words.
  Every function that crosses a type boundary in @bm{morsecode} operates on strings and integers, thus dynamically type-checking these functions' arguments is relatively cheap.
}
@bm-desc[
  @bm{zombie}
  #:author "David Van Horn"
  #:origin "Research"
  #:purpose "Game"
  #:url "https://github.com/philnguyen/soft-contract"]{
  Implements a game where players dodge computer-controlled ``zombie'' tokens.
  Curried functions over symbols implement game entities and repeatedly cross type boundaries.
    @;@racket[
    @;  (define-type Point
    @;    ((U 'x 'y 'move)
    @;     ->
    @;     (U (Pairof 'x (-> Real))
    @;        (Pairof 'y (-> Real))
    @;        (Pairof 'move (Real Real -> Point)))))
    @;]
}
@bm-desc[
  @bm{dungeon}
  #:author "Vincent St. Amour"
  #:origin "Application"
  #:purpose "Maze generator"]{
  Builds a grid of wall and floor objects by choosing first-class classes from a list of ``template'' pieces.
  This list accumulates proxies when it crosses a type boundary.

  Originally, the program imported the Racket @library{math} library
   for array operations.
  The benchmark uses Racket's vectors instead of the @library{math} library's arrays
   because Typed Racket v6.2 cannot compile the type @rkt{(Mutable-Array (Class))} to a contract.
  @;and @library{racket/dict} for a generic dictionary interface.  
  @; TODO why remove dict?
  @;The benchmark avoids @library{racket/dict} to avoid a type boundary.
}
@bm-desc[
  @bm{zordoz}
  #:author "Ben Greenman"
  #:origin "Library"
  #:purpose "Explore bytecode"
  #:url "http://github.com/bennn/zordoz"
  #:depends (list (make-lib "compiler-lib" "http://docs.racket-lang.org/raco/decompile.html#%28mod-path._compiler%2Fdecompile%29"))]{
  Traverses Racket bytecode (@tt{.zo} files).
  The @library{compiler-lib} library defines the bytecode data structures.
  Typed code interacting with the library suffers overhead.
}
@bm-desc[
  @bm{lnm}
  #:author "Ben Greenman"
  #:origin "Synthetic"
  #:purpose "Data visualization"
  #:depends (list
              (make-lib "plot" "https://docs.racket-lang.org/plot/")
              (make-lib "math/statistics" "https://docs.racket-lang.org/math/stats.html"))]{
  Renders overhead plots@~cite{tfgnvf-popl-2016}.
  Two modules are tightly-coupled to Typed Racket libraries; typing both modules improves performance.
}
@bm-desc[
  @bm{suffixtree}
  #:author "Danny Yoo"
  #:origin "Library"
  #:purpose "String tools"
  #:url "https://github.com/dyoo/suffixtree"]{
  Implements Ukkonen's suffix tree algorithm and computes
  longest common subsequences between strings.
  The largest performance overheads are due to a boundary between struct definitions and functions on the structures.
}
@bm-desc[
  @bm{kcfa}
  #:author "Matt Might"
  #:origin "Educational"
  #:purpose "Explanation of k-CFA"
  #:url "http://matt.might.net/articles/implementation-of-kcfa-and-0cfa/"]{
  Performs 1-CFA on a lambda calculus term that computes @$|{~2*(1+3) = 2*1 + 2*3}| via Church numerals.
  The (mutable) binding environment flows throughout functions in the benchmark.
  When this environment crosses a type boundary, it acquires a new proxy.
}
@bm-desc[
  @bm{snake}
  #:author "David Van Horn"
  #:origin "Research"
  #:purpose "Game"
  #:url "https://github.com/philnguyen/soft-contract"]{
  Implements the Snake game; the benchmark replays a fixed sequence of moves.
  Modules in this benchmark frequently exchange first-order values, such as lists and integers.
}
@bm-desc[
  @bm{take5}
  #:author "Matthias Felleisen"
  #:origin "Educational"
  #:purpose "Game"]{
  Manages a card game between AI players.
  These players communicate infrequently, so gradual typing adds relatively little overhead.
}
@bm-desc[
  @bm{acquire}
  #:author "Matthias Felleisen"
  #:origin "Educational"
  #:purpose "Game"
  #:url "https://github.com/mfelleisen/Acquire"]{
  Simulates a board game via message-passing objects.
  These objects encapsulate the core data structures; few higher-order values cross type boundaries.
}
@bm-desc[
  @bm{tetris}
  #:author "David Van Horn"
  #:origin "Research"
  #:purpose "Game"
  #:url "https://github.com/philnguyen/soft-contract"]{
  Replays a pre-recorded game of Tetris.
  Frequent type boundary crossings, rather than proxies or expensive runtime checks, are the source of performance overhead.
}
@bm-desc[
  @bm{synth}
  #:author "Vincent St. Amour and Neil Toronto"
  #:origin "Application"
  #:purpose "Music synthesis DSL"
  #:url "http://github.com/stamourv/synth"]{
  Converts a description of notes and drum beats to @tt{WAV} format.
  Modules in the benchmark come from two sources, a music library and an array library.
  The worst overhead occurs when arrays frequently cross type boundaries.
}
@bm-desc[
  @bm{gregor}
  #:author "Jon Zeppieri"
  #:origin "Library"
  #:purpose "Date and time library"
  #:url "https://docs.racket-lang.org/gregor/index.html"
  #:depends
    (list (make-lib "cldr" "https://docs.racket-lang.org/cldr-core/index.html")
          (make-lib "tzinfo" "https://docs.racket-lang.org/tzinfo/index.html"))]{

  Provides tools for manipulating calendar dates.
  The benchmark builds tens of date values and runs unit tests on these values.
}
@bm-desc[
  (list @bm{quadBG} @bm{quadMB})
  #:author "Matthew Butterick"
  #:origin "Application"
  #:purpose "Typesetting"
  #:url "https://github.com/mbutterick/quad"
  #:depends (list (make-lib "csp" "https://github.com/mbutterick/csp"))]{
  Converts S-expression source code to @tt{PDF} format.
  The two versions of this benchmark differ in their type annotations, but have nearly identical source code.

  The original version, @bm{quadMB}, uses type annotations by the original author.
  This version has a high typed/untyped ratio
   because it explicitly compiles types to runtime predicates
   and uses these predicates to eagerly check data invariants.
  In other words, the typed configuration is slower than the untyped configuration because it performs more work.

  The second version, @bm{quadBG}, uses identical code but weakens types to match the untyped configuration.
  This version is therefore suitable for judging the implementation
   of Typed Racket rather than the user experience of Typed Racket.

  @; To give a concrete example of different types, here are the definitions
  @;  for the core @tt{Quad} datatype from both @bm{quadMB} and @bm{quadBG}.
  @; @racket[(define-type QuadMB (Pairof Symbol (Listof QuadMB)))]
  @; @racket[(define-type QuadBG (Pairof Symbol (Listof Any)))]
  @; The former is a homogenous, recursive type.
  @; As such, the predicate asserting that a value has type @racket[QuadMB] is a linear-time tree traversal.
  @; The predicate for @racket[QuadBG] runs significantly faster.
}

@figure*["fig:tr:static-benchmark" @elem{
  Static characteristics of the @|GTP| benchmarks.
  @bold{N} = number of components = number of modules.
  SLOC = source lines of fully-typed code as reported by @|SLOCCOUNT|.}
  @tr:render-static-information[tr:ALL-BENCHMARKS]
]

@Figure-ref{fig:tr:static-benchmark} tabulates the size of the migratable
 code in the benchmark programs.
The column labeled @bold{N} reports the number of migratable modules;
 the configuration space for each program has @${2^N} points.
The SLOC column reports lines of code in the fully-typed configuration.
Type annotations add 10 to 300 lines of code, but the count approximates size.


@subsection[#:tag "sec:tr:ratio"]{Performance Ratios}

@figure["fig:tr:ratio" @elem{Performance ratios for the @|GTP| benchmarks.}
  @tr:render-ratios-table[(tr:get-ratios-table tr:ALL-BENCHMARKS)]
]

@Figure-ref{fig:tr:ratio} lists the overhead of static types in the benchmarks.
In @bm{sieve}, for example, the fully-typed configuration runs slightly
 faster than untyped.
In @bm{forth}, the typed configuration is almost 10x slower.

@parag{Performance Ratios, Conclusions}

Overall, the typed configurations run slightly slower than the untyped code.
These small slowdowns are due to type casts on input data and on
 contextual modules.

A few benchmarks, notably @bm{lnm} and @bm{suffixtree}, run faster when
 typed.
For @bm{lnm}, the speedup is due to a typed contextual module than slows
 down the untyped configuration.
For @bm{suffixtree}, we have a positive speedup thanks to the Typed Racket
 optimizer.

Three benchmarks suffer tremendously when typed: @bm{forth}, @bm{zombie},
 and @bm{quadMB}.
Type casts play a large role.
The newest version of the benchmarks should avoid these pathologies.


@; -----------------------------------------------------------------------------
@subsection[#:tag "sec:tr:overhead"]{Overhead Plots}

@render-overhead-plot*[
  "fig:tr:overhead"
  "Typed Racket overhead plots"
  overhead-long-caption
  tr:render-overhead-plot
  (map tr:benchmark-name tr:ALL-BENCHMARKS)
  tr:cache-dir]

@Figures-ref["fig:rp:overhead" (exact-ceiling (/ (length tr:ALL-BENCHMARKS) overhead-plots-per-page))]
 present the results of measuring the benchmark programs in a series of overhead plots.
As in @figure-ref{fig:overhead-plot-example}, the plots are
 cumulative distribution functions for @ddeliverable[] configurations.

@; -- overall, bleak picture
@parag{Conclusions}
@(let* ([num-bm tr:num-benchmarks]
        [num-bm-str (integer->word num-bm)]
        [num-configs (tr:*TOTAL-NUM-CONFIGURATIONS*)]
        [max-str (format "~a" tr:MAX-OVERHEAD)]
        [suffixtree-num-modules (integer->word 6)]
        [num-max-deliverable 9] ; TODO, use *MAX-OVERHEAD*
        [num-max-deliverable-str (integer->word num-max-deliverable)]
        [num-mostly-2-deliverable 6]
        [num-mostly-2-deliverable-str (integer->word num-mostly-2-deliverable)]
        [num-good-slope 8]
        [num-good-slope-str (integer->word num-good-slope)]
        [v-max (last (tr:*RKT-VERSIONS*))]
        [format-% (lambda (n) (format "~a%" (round (* 100 (/ n num-configs)))))]
        [lo (number->string (tr:*LO*))]
        [hi (number->string (tr:*HI*))]
        [lo-prop (format-% (tr:deliverable* (tr:*LO*) v-max tr:ALL-BENCHMARKS))]
        [hi-prop (format-% (- num-configs (tr:deliverable* (tr:*HI*) v-max tr:ALL-BENCHMARKS)))]
       ) @elem{
Many curves are quite flat; they demonstrate that gradual typing introduces
 large and widespread performance overhead in the corresponding benchmarks.
Among benchmarks with fewer than @|suffixtree-num-modules| modules, the
 most common shape is a flat line near the 50% mark.
Such lines imply that the performance of a group of configurations is
 dominated by a single type boundary.
@; For instance, there is one type boundary in @bm{fsm} that adds overwhelming slowdown when present; all eight configurations with this boundary have over @|max-str| overhead.
Benchmarks with @|suffixtree-num-modules| or more modules generally have
 smoother slopes, but five such benchmarks have essentially flat curves.
The overall message is that for many values of @${D} between 1 and
 @|max-str|, few configurations are @ddeliverable{}.

For example, in @integer->word[(- num-bm num-mostly-2-deliverable)] of the
 @|num-bm-str| benchmark programs, at most half the configurations are
 @ddeliverable{2} on any version.
The situation is worse for lower (more realistic) overheads, and does not
 improve much for higher overheads.
Similarly, there are ten benchmarks in which at most half the
 configurations are @ddeliverable{10}.

The curves' endpoints describe the extremes of gradual typing.
The left endpoint gives the percentage of configurations that run at least
 as quickly as the untyped configuration.
Except for the @bm{lnm} benchmark, such configurations are a low proportion of the total.
The right endpoint shows how many configurations suffer over 20x performance overhead.
@string-titlecase[num-max-deliverable-str] benchmarks have at least one such configuration.

In summary, the application of the evaluation method projects a negative
 image of Typed Racket's sound gradual typing.
Only a small number of configurations in the benchmark suite run with low
 overhead; a mere @|lo-prop| of all configurations are @ddeliverable[lo] on
 Racket v@|v-max|.
Many demonstrate extreme overhead; @|hi-prop| of all configurations are not
 even @ddeliverable[hi] on version @|v-max|.
})


@; -----------------------------------------------------------------------------
@section[#:tag "sec:rp:evaluation"]{Application 2: Reticulated Python}

Together with Zeina Migeed, I conducted a performance evaluation for Reticulated
 Python.
This evaluation led to three broad benefits.

In terms of the method, this evaluation required generalizing and scaling.
It was an opportunity to validate random sampling, and in turn random sampling
 was an essential part of our success.

For gradual typing, the evaluation served as the first comprehensive test
 of the @|stransient| approach.
Whereas Typed Racket's @|snatural| approach suffers from slowdowns of over
 two orders of magnitude, @|stransient| never exceeded 10x overhead.
The catch, however, is that the fully-typed configuration is slowest in
 @|stransient| but fastest in @|sguarded|.
Instead of proving @|stransient| the clear winner, the evaluation revealed
 a tradeoff.

Third, the evaluation revealed issues with the implementation of @|stransient|
 in Reticulated Python where benchmarks had anomolous results.
We found two soundness holes: one with optional function parameters and one
 with tuples.


@subsection[#:tag "sec:rp:protocol"]{Protocol}

@parag{Granularity}
The granularity of this evaluation is @emph{function and class fields}.
One syntactic unit in the experiment is either one function,
 one method, or the collection of all fields for one class.
@; TODO example
@; TODO connect to static figure ... use N everywhere? ... say "units"?


@parag{Data Collection}
For benchmarks with at most @$|{2^{17}}| configurations, we conduct an exhaustive
 evaluation.
For larger benchmarks we conduct a simple random approximation using
 @integer->word[NUM-SAMPLE-TRIALS] samples each containing @${@id[SAMPLE-RATE]\!*\!(F + C)}
 configurations, where @${F} is the number of functions in the benchmark and
 @${C} is the number of classes.
Note that the number @id[SAMPLE-RATE] was chosen simply to collect
 as much data as possible in a reasonable amount of time.

All data in this paper was produced by jobs we sent
 to the @hyperlink["https://kb.iu.edu/d/bezu"]{@emph{Karst at Indiana University}}
 computing cluster.
Each job:
@itemlist[#:style 'ordered
@item{
  reserved all processors on one node;
}
@item{
  downloaded fresh copies of @|rp:PYTHON|
  and Reticulated commit @github-commit["mvitousek" "reticulated" "e478343ce7c0f2bc50d897b0ad38055e8fd9487d"];
}
@item{
  repeatedly:
  selected a random configuration from a random benchmark,
  ran the configuration's main module @id[rp:NUM-ITERATIONS] times,
  and recorded the result of each run.
}
]
Cluster nodes are IBM NeXtScale nx360 M4 servers with two Intel Xeon E5-2650 v2
 8-core processors, 32 GB of RAM, and 250 GB of local disk storage.

Data collection scripts are online:
 @format-url{https://github.com/nuprl/retic_performance}

@; -----------------------------------------------------------------------------
@subsection[#:tag "sec:rp:benchmarks"]{Benchmarks}

@(let ([num1 @Integer->word[(length (list* 'aespython 'stats rp:DLS-2014-BENCHMARK-NAMES))]]
       [num2 @Integer->word[(length rp:POPL-2017-BENCHMARK-NAMES)]]
       [num3 @integer->word[(length '(Espionage PythonFlow take5 sample_fsm))]]) @elem{
   @|num1| benchmarks originate from case studies by @citet{vksb-dls-2014}.
   @;footnote{@|dls-names|.}
   @|num2| are from the evaluation by @citet{vss-popl-2017} on programs from
   the Python Performance Benchmark Suite.
   The remaining @|num3| originate from open-source programs.
})

The following descriptions credit each benchmark's original author,
 state whether the benchmark depends on any contextual modules,
 and briefly summarize its purpose.

@; WARNING: benchmark order matters
@exact{\bigskip}
@bm-desc[
  @bm{futen}
  #:author (hyperlink "http://blog.amedama.jp/" @tt{momijiame})
  #:origin "Library"
  #:purpose "SSH configuration"
  #:url "https://github.com/momijiame/futen"
  #:depends
  (list
    @rp:lib-desc["fnmatch"]{Filename matching}
    @rp:lib-desc["os.path"]{Path split, path join, path expand, getenv}
    @rp:lib-desc["re"]{One regular expression match}
    @rp:lib-desc["shlex"]{Split host names from an input string}
    @rp:lib-desc["socket"]{Basic socket operations})]{
  Converts an @hyperlink["https://www.openssh.com/"]{OpenSSH} configuration
  file to an inventory file for the
  @hyperlink["https://www.ansible.com/"]{Ansiable} automation framework.
  @; 1900 iterations
}
@bm-desc[
  @bm{http2}
  #:author @hyperlink["https://github.com/httplib2/httplib2"]{Joe Gregorio}
  #:origin "Library"
  #:purpose "HTTP utilities"
  #:url "https://github.com/httplib2/httplib2"
  #:depends (list @rp:lib-desc["urllib"]{To split an IRI into components})]{
  Converts a collection of @hyperlink["https://en.wikipedia.org/wiki/Internationalized_Resource_Identifier"]{Internationalized Resource Identifiers}
  to equivalent @hyperlink["http://www.asciitable.com/"]{ASCII} resource
  identifiers.
  @; 10 iterations
}
@bm-desc[
  @bm{slowSHA}
  #:author "Stefano Palazzo"
  #:origin "Library"
  #:purpose "Hashing"
  #:url "http://github.com/sfstpala/SlowSHA"
  #:depends (list @rp:lib-desc["os"]{path split})]{
  Applies the SHA-1 and SHA-512 algorithms to English words.
  @; 1 iteration
}
@bm-desc[
  @bm{call_method}
  #:author unknown-author
  #:origin PYBENCH
  #:purpose "Test method calls"
  #:url "https://github.com/python/performance"]{
  Microbenchmarks simple method calls;
  the calls do not use argument lists,
  keyword arguments, or tuple unpacking.
  @; Consists of @${32*10^5} calls to trivial functions.
  @; 1 iteration
}
@bm-desc[
  @bm{call_simple}
  #:author unknown-author
  #:origin PYBENCH
  #:purpose "Test function calls"
  #:url "https://github.com/python/performance"]{
  Same as @bm{call_method}, using functions rather than methods.
}
@bm-desc[
  @bm{chaos}
  #:author "Carl Friedrich Bolz"
  #:origin PYBENCH
  #:purpose "Create fractals"
  #:url "https://github.com/python/performance"
  #:depends @list[
    @rp:lib-desc["math"]{Square root}
    @rp:lib-desc["random"]{randrange}]]{
  Creates fractals using the @hyperlink["https://en.wikipedia.org/wiki/Chaos_game"]{@emph{chaos game}} method.
  @; 1 iteration
}
@bm-desc[
  @bm{fannkuch}
  #:author "Sokolov Yura"
  #:origin PYBENCH
  #:purpose "Test integers, vectors"
  #:url "https://github.com/python/performance"]{
  Implements a classic LISP microbenchmark@~cite{ar-lp-1994}.
  @; 1 iteration
}
@bm-desc[
  @bm{float}
  #:author "Factor"
  #:origin PYBENCH
  #:purpose "Test float ops"
  #:url "https://github.com/python/performance"
  #:depends @list[@rp:lib-desc["math"]{Sin, Cos, Sqrt}]]{
  Microbenchmarks floating-point operations.
  @; 1 iteration (200,000 points)
}
@bm-desc[
  @bm{go}
  #:author unknown-author
  #:origin PYBENCH
  #:purpose "Game"
  #:url "https://github.com/python/performance"
  #:depends @list[ @rp:lib-desc["math"]{sqrt log} @rp:lib-desc["random"]{randrange random}]]{
  Implements the game @hyperlink["https://en.wikipedia.org/wiki/Go_(game)"]{Go}.
  This benchmark is split across three files: a @defn{migratable} module that implements
  the game board, a @defn{contextual} module that defines constants, and a @defn{contextual} module
  that implements an AI and drives the benchmark.
  @; 2 iterations
}
@bm-desc[
  @bm{meteor}
  #:author "Daniel Nanz"
  #:origin PYBENCH
  #:purpose "Puzzle"
  #:url "https://github.com/python/performance"]{
  Solves the Shootout benchmarks meteor puzzle.
  @;footnote{@url{http://benchmarksgame.alioth.debian.org/u32/meteor-description.html}}
  @; 1 iterations (finds at most 6,000 solutions)
}
@bm-desc[
  @bm{nbody}
  #:author "Kevin Carson"
  #:origin PYBENCH
  #:purpose "Test float ops"
  #:url "https://github.com/python/performance"]{
  Models the orbits of Jupiter, Saturn, Uranus, and Neptune.
  @; 1 iteration
}
@bm-desc[
  @bm{nqueens}
  #:author unknown-author
  #:origin PYBENCH
  #:purpose "Puzzle"
  #:url "https://github.com/python/performance"]{
  Solves the @hyperlink["https://developers.google.com/optimization/puzzles/queens"]{@${8}-queens} problem by a brute-force algorithm 10 times in a row.
}
@bm-desc[
  @bm{pidigits}
  #:author unknown-author
  #:origin PYBENCH
  #:purpose "Test big integer ops"
  #:url "https://github.com/python/performance"]{
  Microbenchmarks big-integer arithmetic.
  @; 1 iteration (5,000 digits)
}
@bm-desc[
  @bm{pystone}
  #:author "Chris Arndt"
  #:origin PYBENCH
  #:purpose "Test integer ops"
  #:url "https://github.com/python/performance"]{
  Implements Weicker's @emph{Dhrystone} benchmark.
  @;footnote{@url{http://www.eembc.org/techlit/datasheets/ECLDhrystoneWhitePaper2.pdf}}
  @; 50,000 iterations
}
@bm-desc[
  @bm{spectralnorm}
  #:author "Sebastien Loisel"
  #:origin PYBENCH
  #:purpose "Test arithmetic"
  #:url "https://github.com/python/performance"]{
  Computes the largest singular value of an infinite matrix.
  @; 10 iterations
}
@bm-desc[
  @bm{Espionage}
  #:author "Zeina Migeed"
  #:origin "Synthetic"
  #:purpose "Graph algorithm"
  #:depends @list[@rp:lib-desc["operator"]{itemgetter}]]{
  Implements Kruskal's spanning-tree algorithm.
  @; 1 iteration
}
@bm-desc[
  @bm{PythonFlow}
  #:author "Alfian Ramadhan"
  #:origin "Synthetic"
  #:purpose "Flow algorithm"
  #:url "https://github.com/masphei/PythonFlow"
  #:depends @list[@rp:lib-desc["os"]{path join}]]{
  Implements the Ford-Fulkerson algorithm.
  @; no longer needs citation
  @;@~cite{ff-cjm-1956}.
  @; 1 iteration
}
@bm-desc[
  @bm{take5}
  #:author "Maha Alkhairy and Zeina Migeed"
  #:origin "Educational"
  #:purpose "Game"
  #:depends @list[ @rp:lib-desc["random"]{randrange shuffle random seed} @rp:lib-desc["copy"]{deepcopy}]]{
  Implements a card game and a simple player AI.
  @; 500 iterations
}
@bm-desc[
  @bm{sample_fsm}
  #:author "Zeina Migeed"
  #:origin "Economics research"
  #:purpose "Economy simulator"
  #:url "https://github.com/ayaderaghul/sample-fsm"
  #:depends
  @list[
    @rp:lib-desc["itertools"]{cycles}
    @rp:lib-desc["os"]{path split}
    @rp:lib-desc["random"]{random randrange}
  ]]{
  Adapted from the Typed Racket @bm{fsm} benchmark.
  @; 100 iterations
}
@bm-desc[
  @bm{aespython}
  #:author (list
    @hyperlink["http://caller9.com/"]{Adam Newman}
    @hyperlink["https://github.com/serprex"]{Demur Remud})
  #:origin "Library"
  #:purpose "Encryption"
  #:url "https://github.com/serprex/pythonaes"
  #:depends @list[ @rp:lib-desc["os"]{random stat} @rp:lib-desc["struct"]{pack unpack calcsize} ]]{
  @; Second sentence is a little awkward. I just want to say, "this is really
  @;  a Python implementation of AES, not just a wrapper to some UNIX implementation"
  Implements the @hyperlink["http://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197.pdf"]{Advanced Encryption Standard}.
  @;Uses the @tt{os} library only to generate random bytes and invoke the
  @; @hyperlink["http://man7.org/linux/man-pages/man2/stat.2.html"]{@tt{stat()}}
  @; system call.
  @; 1 iteration, encrypts the book of Leviticus (2800 lines)
}
@bm-desc[
  @bm{stats}
  #:author @hyperlink["https://connects.catalyst.harvard.edu/Profiles/display/Person/12467"]{Gary Strangman}
  #:origin "Library"
  #:purpose "Statistics"
  #:url "https://github.com/seperman/python-statlib/blob/master/statlib/pstat.py"
  #:depends @list[ @rp:lib-desc["copy"]{deepcopy} @rp:lib-desc["math"]{pow abs etc.}]]{
  Implements first-order statistics functions; in other words, transformations
   on either floats or (possibly-nested) lists of floats.
  The original program consists of two modules.
  The benchmark is modularized according to comments in the program's source
   code to reduce the size of each module's configuration space.
  @; 1 iteration
}

@figure["fig:rp:static-benchmark" @elem{
  Static summary of the Reticulated benchmarks.
  @bold{N} = number of components = functions + classes + methods.
  SLOC = source lines of code as reported by @|SLOCCOUNT|.}
  @rp:render-static-information[rp:MAIN-BENCHMARKS]]
@; TODO TR has extra columns
@;  annotation LOC = meaningful but probably less interesting for retic
@;  #adp = n/a
@;  #bnd #export = meaningful, but do not have

To assess the run-time cost of gradual typing in Reticulated, we measured
 the performance of @integer->word[rp:NUM-MAIN-BENCHMARKS] benchmark programs.
@(let* ([column-descr*
         (list
           @elem{number of migratable units (@bold{N} = num. functions + methods + classes)}
           @elem{lines of code (SLOC)}
           @elem{number of modules}
           @elem{number of function definitions}
           @elem{number of classes}
           @elem{number of method definitions})]
        [num-col @integer->word[(length column-descr*)]]
       )
  (unless (= (length column-descr*) (- (length rp:STATIC-INFO-TITLE*) 1))
    (printf "WARNING: missing column description~n actual: ~s~n descr: ~s~n" 
            rp:STATIC-INFO-TITLE*
            column-descr*))
@elem{
  @Figure-ref{fig:rp:static-benchmark} tabulates information about the size and
   structure of the @defn{migratable} portions of these benchmarks.
  The @|num-col| columns report the @oxfordize[column-descr*].
  Most benchmarks are small, with 1--3 modules and fewer than 200 lines of code.
  That said, the number of gradually-typed configurations in the experiment is
   prohibitively large.
  The relatively small @bm{sample_fsm} describes half a million configurations.
  For the largest two benchmarks, @bm{aespython} and @bm{stats}, exhaustive
   measurement is out of the question.
})


@subsection[#:tag "sec:rp:ratio"]{Performance Ratios}

@(let* ([rp:RT (rp:get-ratios-table rp:MAIN-BENCHMARKS)]
        [futen-row (rp:ratios-table-row rp:RT 'futen)]
        [futen-u/p (rp:ratios-row-retic/python futen-row)]
        [futen-t/u (rp:ratios-row-typed/retic futen-row)]
        [rp* (map rp:ratios-row-retic/python rp:RT)]
        [tr* (map rp:ratios-row-typed/retic rp:RT)]
        [count-< (λ (x* n) (length (filter (λ (str) (< (string->number str) n)) x*)))]
        [rp-<2 (count-< rp* 2)]
        [rp-<3 (count-< rp* 3)]
        [rp-<4.5 (count-< rp* 4.5)]
        [tr-<2 (count-< tr* 2)]
        [tr-<3 (count-< tr* 3)]
        [tr-<3.5 (count-< tr* 3.5)]
        [num-> (for/sum ([rp (in-list rp*)]
                         [tr (in-list tr*)]
                         #:when (> (string->number rp) (string->number tr)))
                 1)])
  (unless (= (length rp*) rp-<4.5)
    (printf "WARNING: performance-ratios expected all retic/python ratios to be < 4.5, but only ~a of ~a are"
            rp-<4.5 (length rp*)))
  (list
@elem{
The table in @figure-ref{fig:rp:ratio} lists the extremes of gradual typing in
 Reticulated.
From left to right, these are:
 the performance of the untyped configuration relative to the Python baseline (the @id[rp:u/p-ratio]),
 the performance of the fully-typed configuration relative to the untyped configuration (the @id[rp:t/u-ratio]),
 and the overall delta between fully-typed and Python (the @id[rp:t/p-ratio]).
}

@figure["fig:rp:ratio" @elem{Performance ratios for three important points in
 a configuration space: fully-typed code (typed), untyped code run through
 Reticulated (retic), and untyped code run via Python (python).}
  @rp:render-ratios-table[rp:RT]
]

@elem{
  For example, the row for @bm{futen} reports a @|rp:u/p-ratio| of @${@|futen-u/p|}.
  This means that the average time to run the untyped configuration of the
   @bm{futen} benchmark using Reticulated was @${@|futen-u/p|} times slower than the
   average time of running the same code using Python.
  Similarly, the @|rp:t/u-ratio| for @bm{futen} states that the fully-typed configuration
   is @${@|futen-t/u|} times slower than the untyped configuration.
}

@parag{Performance Ratios, Conclusions}
@elem{
  Migrating a benchmark to
   Reticulated, or from untyped to fully-typed, always adds performance overhead.
  The migration never improves performance.
  The overhead is always within an order-of-magnitude.

  Regarding the @|rp:u/p-ratio|s:
   @integer->word[rp-<2] are below @${2}x,
   @integer->word[(- rp-<3 rp-<2)] are between @${2}x and @${3}x, and
   the remaining @integer->word[(- rp-<4.5 rp-<3)] are below @${4.5}x.
  The @|rp:t/u-ratio|s are typically lower:
   @integer->word[tr-<2] are below @${2}x,
   @integer->word[(- tr-<3 tr-<2)] are between @${2}x and @${3}x,
   and the final @integer->word[(- tr-<3.5 tr-<3)] are below @${3.5}x.

  @Integer->word[num->] benchmarks have larger @|rp:u/p-ratio|s than @|rp:t/u-ratio|s.
  Given that an untyped Reticulated program offers the same safety guarantees
   as Python, it is surprising that the @|rp:u/p-ratio|s are so large.
}))


@subsection[#:tag "sec:rp:overhead"]{Overhead Plots}

@render-overhead-plot*[
  "fig:rp:overhead"
  "Reticulated overhead plots"
  overhead-long-caption
  rp:render-overhead-plot
  rp:MAIN-BENCHMARKS
  rp:cache-dir
]

@Figures-ref["fig:rp:overhead" (exact-ceiling (/ (length rp:MAIN-BENCHMARKS) overhead-plots-per-page))] summarizes the overhead of gradual typing in the
 benchmark programs.
Each plot reports the percent of @ddeliverable[] configurations (@|y-axis|)
 for values of @${D} between @${1}x overhead and @${@id[rp:MAX-OVERHEAD]}x overhead (@|x-axis|).
The @|x-axes| are log-scaled to focus on low overheads;
 vertical tick marks appear at @${1.2}x, @${1.4}x, @${1.6}x, @${1.8}x, @${4}x, @${6}x, and @${8}x overhead.

The heading above the plot for a given benchmark states the benchmark's name
 and indicate whether the data is exhaustive or approximate.
If the data is exhaustive, this heading lists the number of configurations
 in the benchmark.
If the data is approximate, the heading lists the number of samples
 and the number of randomly-selected configurations in each sample.

The curves for the approximate data
 (i.e., the curves for @bm{sample_fsm}, @bm{aespython}, and @bm{stats}) are intervals
 rather than fixed-width lines.
For instance, the height of an interval at @${x\!=\!4} is the range of the
 @sraapproximation[NUM-SAMPLE-TRIALS (format "[~a(F+C)]" SAMPLE-RATE) "95"]
 for the number of @ddeliverable[4] configurations.
These intervals are thin because there is little variance in the proportion
 of @ddeliverable{D} configurations across the @integer->word[NUM-SAMPLE-TRIALS]
 samples; that said, the @bm{sample_fsm} curve is visibly thicker than the
 @bm{aespython} curve.


@parag{Overhead Plot, Conclusions}
Curves in @figures-ref["fig:rp:overhead" (exact-ceiling (/ (length rp:MAIN-BENCHMARKS) overhead-plots-per-page))] typically cover a large area and reach the
 top of the @|y-axis| at a low value of @${D}.
This value is always less than @${@id[rp:MAX-OVERHEAD]}.
In other words, every configuration in the
 experiment is @ddeliverable[rp:MAX-OVERHEAD].
For many benchmarks, the maximum overhead is significantly lower.
@(let ([num-2-deliv (length '(futen slowSHA fannkuch nbody nqueens pidigits
                              take5 stats))]) @elem{
  Indeed, @integer->word[num-2-deliv] benchmarks are nearly @ddeliverable[2].})

None of the configurations in the experiment run faster than the Python baseline.
This is to be expected, given the @|rp:u/p-ratio|s in @figure-ref{fig:rp:ratio} and the
 fact that Reticulated translates type annotations into run-time checks.

@(let ([smooth '(futen http2 slowSHA chaos fannkuch float nbody pidigits
                 pystone PythonFlow take5 sample_fsm aespython stats)])
  @elem{
    @Integer->word[(length smooth)] benchmarks have relatively smooth slopes.
    The plots for the other @integer->word[(- rp:NUM-EXHAUSTIVE-BENCHMARKS (length smooth))]
     benchmarks have wide, flat segments.
    These flat segments are due to functions that are frequently executed
     in the benchmarks' traces; all configurations in which one of these functions
     is typed incur a significant performance overhead.
})

@(let* ([NOT-tp '(http2 call_method spectralnorm)]
        [num-tp (- rp:NUM-MAIN-BENCHMARKS (length NOT-tp))]
        [S-SLOWER (rp:percent-slower-than-typed 'spectralnorm)]) @elem{
@Integer->word[num-tp] benchmarks are roughly @ddeliverable{T}, where @${T} is
 the @|rp:t/p-ratio| listed in @figure-ref{fig:rp:ratio}.
In these benchmarks, the fully-typed configuration is one of the slowest configurations.
The notable exception is @bm{spectralnorm}, in which the fully-typed configuration
 runs faster than @${@id[S-SLOWER]\%} of all configurations.
Unfortunately, this speedup is due to a soundness bug
 (@github-issue["mvitousek" "reticulated" 36]);
 in short, the implementation of Reticulated does not type-check the contents of tuples.
})



@subsection[#:tag "sec:rp:threats"]{Threats to Validity}

We have identified five sources of systematic
 bias.
@(let* ( @; See `src/PyPI-ranking/README.md` to reproduce these claims
        [lib-data* '((simplejson 50 "https://github.com/simplejson/simplejson")
                     (requests 200 "https://github.com/kennethreitz/requests")
                     (Jinja2 600 "https://github.com/pallets/jinja/tree/master/jinja2"))]
        [rank-info @elem{PyPI Ranking (@format-url{http://pypi-ranking.info/alltime})}]
        [lib-info (authors*
                    (for/list ([ld (in-list lib-data*)]
                               [long-style? (in-sequences '(#t)
                                                          (in-cycle '(#f)))])
                      @elem{
                        @(if long-style? "The" "the")
                        @hyperlink[(caddr ld)]{@tt[@symbol->string[(car ld)]]}
                        library contains over @${@id[(cadr ld)]}@;
                        @(if long-style? " functions and methods" "")}))]
       ) @elem{
  First, the experiment consists of a small suite of benchmarks, and these
   benchmarks are rather small.
  For example, an ad-hoc sample of the @|rank-info| reveals that even small
   Python packages have far more functions and methods than our benchmarks.
  @|lib-info|.
})

Second, the experiment considers one fully-typed configuration per benchmark;
 however, there are many ways of typing a given program.
The types in this experiment may differ from types ascribed by another Python
 programmer, which, in turn, may lead to different performance overhead.

@(let ([missing-types '(take5)]
       [retic-limited '(pystone stats)]
       [format-bm* (lambda (bm*) (authors* (map bm bm*)))]
       @; see also https://github.com/nuprl/retic_performance/issues/55
       @;
       @; - futen is missing some type annotation(s)
       @;   - LazyFqdn missing @fields
       @; - call_method is missing some type annotation(s)
       @;   - missing @fields, but actually empty
       @; - call_method_slots is missing some type annotation(s)
       @;   - missing @fields, but actually empty
       @; - go uses the Dyn type
       @;   - to avoid circular reference
       @; - pystone uses the Dyn type
       @;   - union type, (U PSRecord None)
       @; - take5 is missing some type annotation(s)
       @;   - `create_deck`, argument 'deck_size' is unannotated
       @;   - same function has optional arguments, so the types ignored
       @; - stats is missing some type annotation(s)
       @;   - only on the print function
       @; - stats uses the Dyn type
       @;   - for polymorphism, "rank polymorphism", and union types
      ) @elem{
  Third, some benchmarks use dynamic typing.
  The @bm{take5} benchmark contains one function that accepts optional arguments,
   and is therefore dynamically typed (@github-issue["mvitousek" "reticulated" 32]).
  The @bm{go} benchmark uses dynamic typing because Reticulated cannot validate
   its use of a recursive class definition.
  Two other benchmarks use dynamic typing to overcome Reticulated's lack of
   untagged union types; namely, @format-bm*[retic-limited]
})

@(let ([use-io* '(aespython futen http2 slowSHA)]) @elem{
  Fourth, the @(authors* (map bm use-io*)) benchmarks read from a file
   within their timed computation.
  We nevertheless consider our results representative.
})

Fifth, Reticulated supports a finer granularity of type annotations than the
 experiment considers.
Function signatures can leave some arguments untyped, and class field
 declaractions can omit types for some members.
We believe that a fine-grained evaluation would support the
 conclusions presented in this paper.


@; -----------------------------------------------------------------------------
@section{Digression: Additional Visualizations}

The methods presented in this chapter are our most effective answer
 to the question of how to evaluate the performance of a gradual typing system.
In particular, the notion of @ddeliverable{} configurations is a
 clear and scalable way to summarize performance.

That said, a gradual typing system has other interesting properties besides
 the number of @ddeliverable{} configurations.
This section presents other visualizations that help answer extra
 questions.


@subsection{Exact Runtime Plots}

@(let* ((bm-name 'nqueens)
        (S (rp:benchmark-name->performance-info bm-name))
        (num-units (performance-info->num-units S))
       ) @list[
@render-overhead-plot*[
  "fig:example-exact-plot"
  @elem{Number of type annotations vs. Running time on the Reticulated @bm[@~a[bm-name]] benchmark.}
  exact-long-caption
  rp:render-exact-plot
  (list bm-name)
  #f]
@elem{
The raw data behind an overhead plot is a sequence of running times for
 every configuration.
An overhead plot summarizes the running times into an average, and uses
 these averages to group configurations into buckets.
Unfortunately, this method hides outliers in the data and syntactic similarities among
 configurations.

@Figure-ref{fig:example-exact-plot} addresses both concerns.
Instead of summarizing one configuration with its average runtime,
 the plot contains one point for every running time in the dataset.
These points are sorted left-to-right in one of the @integer->word[num-units]
 columns of the figure; if a plot like this does not consist of distinct,
 horizontal lines, the underlying dataset may have irregular running times.
Each column contains all configurations that have the same number of types.
In terms of the configuration lattice (@figure-ref{fig:example-lattice-0}),
 the left-most column contains the bottom level and successive columns
 present successive levels.
At a glance, @figure-ref{fig:example-exact-plot} therefore shows the overall
 effect of adding types.
This high-level view can be useful for comparing different approaches to
 gradual typing.
}])


@subsection{Relative Scatterplots}


@; @subsection{Best-Path Plots}
@; TODO make new gtp-plot function, draw lines on same plot ... or change pi struct?
@;  right now, trouble is no way to tell config<



@;  @subsection[#:tag "sec:rp:exact"]{Absolute Running Times}
@;  
@;  @figure*["fig:rp:exact" "Running time (in seconds) vs. Number of typed components"
@;    @rp:render-exact-runtime-plot*[rp:MAIN-BENCHMARKS]
@;  ]
@;  
@;  Since changing the type annotations in a Reticulated program changes its
@;   performance, the language should provide a cost model to help developers
@;   predict the performance of a given configuration.
@;  The plots in @figure-ref{fig:rp:exact} demonstrate that a simple heuristic
@;   works well for these benchmarks: the performance of a configuration is
@;   proportional to the number of type annotations in the configuration.
@;  
@;  @parag{How to Read the Plots}
@;  @Figure-ref{fig:rp:exact} contains one point for every run of every
@;   configuration in the experiment.
@;  (Recall from @section-ref{sec:rp:protocol},
@;   the data for each configuration is @id[rp:NUM-ITERATIONS] runs.)
@;  Each point compares the number of type annotations in a
@;   configuration (@|x-axis|) against its running time, measured in seconds (@|y-axis|).
@;  
@;  The plots contain many points with both the same number of typed components
@;   and similar performance.
@;  To reduce the visual overlap between such points, the points for a given
@;   configuration are spread across the @|x-axis|; in particular,
@;   the @id[rp:NUM-ITERATIONS] points for a configuration with @${N}
@;   typed components lie within the interval @${N\!\pm\!@id[rp:EXACT-RUNTIME-XSPACE]}
@;   on the @|x-axis|.
@;  
@;  For example, @bm{fannkuch} has two configurations: the untyped configuration
@;   and the fully-typed configuration.
@;  To determine whether a point @${(x,y)} in the plot for @bm{fannkuch} represents
@;   the untyped or fully-typed configuration, round @${x} to the nearest integer.
@;  
@;  
@;  @parag{Exact Runtime Conclusions}
@;  
@;  Suppose a programmer starts at an arbitrary configuration and adds some
@;   type annotations.
@;  The plots in @figure-ref{fig:rp:exact} suggest that this action will affect
@;   performance in one of four possible ways, based on trends among the plots.
@;  
@;  @exact-runtime-category["types make things slow"
@;    '(futen slowSHA chaos float pystone PythonFlow take5 sample_fsm aespython stats)
@;    (λ (num-in-category) @elem{
@;      The plots for @|num-in-category| benchmarks show a gradual increase in
@;       performance overhead as the number of typed components increases.
@;      Typing any function, class, or method adds a small performance overhead.
@;  })]
@;  
@;  @rp:exact-runtime-category[@elem{types make things very slow}
@;    '(call_method call_simple go http2 meteor nqueens spectralnorm Espionage PythonFlow)
@;    (λ (num-in-category) @elem{
@;      @string-titlecase[num-in-category] plots have visible gaps between
@;       clusters of configurations with the same number of types.
@;      Configurations below the gap contain type annotations that impose relatively little
@;       run-time cost.
@;      Configurations above the gap have some common type annotations that
@;       add significant overhead.
@;      Each such gap corresponds to a flat slope in @figures-ref["fig:rp:overhead" (exact-ceiling (/ (length rp:MAIN-BENCHMARKS) overhead-plots-per-page))].
@;  })]
@;  
@;  @rp:exact-runtime-category[@elem{types are free}
@;    '(fannkuch nbody pidigits)
@;    (λ (num-in-category) @elem{
@;      In @|num-in-category| benchmarks, all configurations have similar performance.
@;      The dynamic checks that enforce tag soundness add insignificant overhead.
@;  })]
@;  
@;  @rp:exact-runtime-category[@elem{types make things fast}
@;    '(call_method spectralnorm)
@;    (λ (num-in-category) @elem{
@;      In @|num-in-category| benchmarks, some configurations
@;       run faster than similar configurations with fewer typed components.
@;      These speedups are the result of two implementation bugs:
@;       (1) Reticulated does not dynamically check the contents of statically-typed tuples,
@;       and (2) for method calls to dynamically-typed objects, Reticulated performs
@;       a run-time check that overlaps with Python's dynamic typing@~cite{vksb-dls-2014}.
@;  })]
@;  
@;  @exact{\smallskip}
@;  
@;  Overall, there is a clear trend that adding type annotations adds performance
@;   overhead.
@;  The increase is typically linear.
@;  On one hand, this observation may help programmers predict performance issues.
@;  On the other hand, the linear increase demonstrates that Reticulated does
@;   not use type information to optimize programs.
@;  In principle a JIT compiler could generate check-free code if it could infer
@;   the run-time type of a variable, but it remains to be seen whether this
@;   approach would improve performance in practice.
