#lang greenman-thesis/include

@(require
   (prefix-in tr: greenman-thesis/jfp-2019/main)
   (prefix-in rp: greenman-thesis/pepm-2018/main)
   (only-in greenman-thesis/shallow/main
     NSA-num-cores
     NSA-core-name
     NSA-core-speed
     NSA-RAM)
   (only-in greenman-thesis/oopsla-2019/pict typed-codeblock untyped-codeblock)
   (only-in racket/math exact-floor)
   gtp-plot/configuration-info
   gtp-plot/typed-racket-info
   gtp-plot/performance-info)

@title[#:tag "chap:performance"]{Performance Analysis Method}
@jointwork[
  #:people* '(
    "Matthias Felleisen"
    "Daniel Feltey"
    "Robert Bruce Findler"
    "Zeina Migeed"
    "Max S. New"
    "Asumu Takikawa"
    "Sam Tobin-Hochstadt"
    "Jan Vitek"
  )
  #:paper* '("gtnffvf-jfp-2019" "gm-pepm-2018" "tfgnvf-popl-2016")
  #:extra @elem{ The Typed Racket benchmarks presented in this chapter
   have been improved over the years by:
   Spenser Bauman, Lukas Lazarek, Cameron Moy, and Sam Sundar.}
]


@; sound GT has cost
@|noindent|Sound types come with performance overhead in a mixed-typed language
 because soundness is a claim about behavior and the only way to control
 the behavior of untyped code is via run-time checks.
These checks impose a cost in proportion to
 the frequency of mixed-typed interactions,
 the complexity of the type specifications that govern boundaries,
 and the strength of the soundness guarantee.

Language designers must measure the performance of a mixed-typed language
 to judge its overall usefulness in light of its guarantees.
Type-sound code that runs too slowly is worthless.
At a finer grain, users need an idea of what overhead to expect when they begin
 experimenting with types.
Implementors need a comprehensive performance summary to
 measure improvements to a language and to compare alternative mixed-typed
 designs.
Despite these realities, early reports on mixed-typed languages
 typically lack performance evaluation.
A few acknowledge performance issues in passing@~cite{tf-popl-2008,acftd-scp-2013,vksb-dls-2014}.
Others show only the performance of fully-typed code relative to fully-untyped
 code, skipping the novel configurations in between@~cite{rsfbv-popl-2015,vss-popl-2017}.
But in their defense, the development of a performance method is a challenge
 in itself.

This chapter presents a systematic and scalable method to assess the performance
 of a mixed-typed language.
The method summarizes performance for the exponentially-many ways that a programmer
 can mix typed and untyped code by focusing on a binary quality measure.
Informally, a mixture is good if runs within a user-supplied overhead limit.
Random sampling can approximate the proportion of good mixtures for programs
 in which exhaustive evaluation is not practical.
As validation, this chapter evaluates the performance of two
 mixed-typed languages: Typed Racket and Reticulated Python.


@section{Design Criteria}

The goal of performance evaluation is to predict the experiences of future
 users.
Intuition suggests that the users of a mixed-typed language will begin
 with an untyped codebase and add types step-by-step.
Experience with Typed Racket supports the intuition.
Programmers add types in an incremental fashion and experiment with all sorts of combinations.
When typed libraries enter the picture, untyped programmers unknowingly
 create mixed-typed applications.
In a typical evolution, programmers compare the performance of the modified
 program with the previous version.
If the current performance is on par with the previous, then all is well.
Otherwise, the easy solutions are: adding more types, and rewinding to a
 less-typed version.
These observations and assumptions about users suggest three basic criteria
 for an evaluation method.

@subsection{Representative Benchmarks}

An evaluation method has to measure programs, and the results of
 a particular evaluation are limited by the chosen benchmarks.
Benchmark programs that stem from realistic code and exercise a variety
 of features are an important step toward generalizable results.

@; TODO say more, please


@subsection{Exponential Compression}

@(let ((num-example-configs 5)) (list @elem{
A mixed-typed language promises to support exponentially-many combinations of
 typed and untyped code.
In Typed Racket, for example, a programmer can add types to any module of a program.
Thus a program with @id[num-example-configs] modules leads to
 @${2^@id[num-example-configs]} possible combinations (@figure-ref{fig:example-lattice-0}).
Languages that can mix at a finer granularity support @${2^k} configurations,
 where @${k} is the number of potentially-typed blocks.

Without evidence against certain mixtures, an evaluation
 must collect data for every mixed-typed configuration.
These huge datasets call for a way to compress exponentially-many
 observations into a compact summary.
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


@subsection{Report Overheads}

@(let ((example-overhead 13)) @elem{
Because migratory typing starts from an untyped language, programmers
 can always revert to an untyped version of their codebase if types
 prove too expensive.
The existence of this fully-untyped baseline helps anchor an evaluation.
If a programmer can tolerate a certain overhead, say @id[example-overhead]x,
 then a single number can summarize the good parts of the exponentially-large
 configuation space; namely, the percent of configurations that run fast enough.
A second benefit is that overheads make it easy to find points where types
 improve upon the baseline; look for overheads under 1x.
})


@; -----------------------------------------------------------------------------
@section{Exhaustive Evaluation Method}

An exhaustive evaluation considers all ways that a programmer might
 toggle type annotations.
The method begins with a fully-typed codebase,
 measures all possible mixed-typed configurations,
 and introduces a compact visualization to summarize the results.


@subsection{By Example}

@;@figure*["fig:example-program" @elem{
@;  Example Typed Racket program.
@;  The four @emph{migratable} modules on the left are under the programmer's control.
@;  The two @emph{contextual} modules on the right represent libraries and core
@;   language APIs.
@;  }
@;  @; TODO white = untyped, black = typed, NON-GRAY = contextual ... pick white or black!
@;  (make-example-program-pict)
@;]

@(let* ((lattice-version "6.4")
        (relative-version "7.7")
        (S (tr:benchmark-name->performance-info 'fsm lattice-version))
        (num-modules (tr:benchmark->num-modules tr:fsm))
       ) @list[
@para{
A Racket program is a collection of modules.
Technically, there are two kinds of modules in such a collection:
 the @emph{migratable} modules that the program's author has direct
 control over, and the @emph{contextual} modules that come from an
 external library.
A programmer can add types to any migratable module.
Thus a program with @${N} migratable modules opens a space of @${2^N}
 mixed-typed configurations, and each configuration depends on
 the same contextual modules.

For example, @bm{fsm} is a small Racket program that simulates an economy (@section-ref{sec:tr:benchmarks}).
The main functionality is split across @integer->word[num-modules] modules;
 with migratory typing, this leads to @integer->word[(expt 2 num-modules)]
 mixed-typed configurations.
@Figure-ref{fig:example-lattice} shows all these configurations in a lattice,
 with the untyped configuration on the bottom and the fully-typed configuration
 on top.
Nodes in the middle mix typed and untyped code; each row groups all
 configurations with the same number of typed modules.
Lines between nodes represent the addition (or removal) of types from one
 module.

The label below a configuration node reports its overhead relative to
 the untyped configuration on Racket version @|lattice-version|.
With these labels, a language implementor can draw several conclusions about
 performance overhead in @bm{fsm}.
A first observation is that the fully-typed code runs equally fast as
 the untyped baseline.
This 1x overhead is also the overall best point in the lattice.
Six other configurations run within a 2x overhead, but the rest suffer
 from orders-of-magnitude slowdowns.
Types in @bm{fsm} can come at a huge cost.
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

@para{

Drawing such conclusions is not easy, however, even for this small program.
Manually analyzing a lattice for programs with eight or more modules is clearly infeasible.
@Figure-ref{fig:overhead-plot-example} presents a graphical alternative,
 an @emph{overhead plot}, that reports configurations' overhead relative
 to the untyped baseline.
Overhead plots are cumulative distribution functions.
As the function proceeds left-to-right for numbers @${D} along the @|x-axis|,
 the curve shows the proportion of all configurations that run at most
 @${D} times slower than the untyped configuration.
For short, these are @ddeliverable{D} configurations.
On the left, there is always at least one @ddeliverable{1} configuration;
 namely, the fully-untyped configuration itself.
The question is whether other configurations run fast as well.
To read such a plot quickly, focus on the area under the curve.
A large shaded area implies that a large number of configurations
 have low overhead.

@(let ([d0 "d_0"]
       [d1 "d_1"]) @elem{
  The second most important aspects of an overhead plot are the two values of @${D}
   where the curve starts and ends.
  More precisely, if @${h\!:\!\mathbb{R}^+\!\rightarrow\!\mathbb{N}} is the CDF
   that counts the proportion of @ddeliverable{D}
   configurations in a benchmark, the critical points are the smallest
   overheads @${@|d0|, @|d1|} such
   that @${h(@|d0|)\!>\!0\%} and @${h(@|d1|)\!=\!100\%}.
  An ideal start-value would lie between zero and one; if @${@|d0|\!<\!1} then
   at least one configuration runs faster than the baseline.
  The end-value @${@|d1|} is the overhead of the slowest-running configuration.
})

Lastly, the slope of a curve corresponds to the likelihood that
 accepting a small increase in performance overhead increases the number
 of deliverable configurations.
A flat curve (zero slope) suggests that the performance of a group of
 configurations is dominated by a common set of type annotations.
Such observations are no help to programmers facing performance issues,
 but may help language implementors fix inefficiencies.

Overhead plots scale to arbitrarily large datasets by compressing
 exponentially-many points into a proportion.
Furthermore, plotting two curves on one axis compares relative performance.
@Figure-ref{fig:relative-overhead-plot-example} demonstrates two curves
 for @bm{fsm}: on Racket v@|lattice-version| and v@|relative-version|.
The latter curve shows a huge improvement thanks to collapsible contracts@~cite{fgsfs-oopsla-2018}.
Indeed, every @bm{fsm} configuration is @ddeliverable{4} on Racket v@|relative-version|.
}
@render-overhead-plot*[
  "fig:overhead-plot-example"
  @elem{
Overhead plot for @bm{fsm}, on Racket v@|lattice-version|.
The unlabeled vertical ticks mark, from left-to-right:
1.2x, 1.4x, 1.6x, 1.8x, 4x, 6x, 8x, 10x, 12x, 14x, 16x, and 18x.
  }
  ""
  (tr:make-render-overhead-plot lattice-version)
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
])


@subsection[#:tag "chap:performance:definition"]{By Definition}

The exhaustive evaluation method applies to other mixed-typed languages
 as well as Typed Racket.
To encourage adaptations, the following definitions highlight key concepts.
The prose uses Reticulated Python as a running example.

In Reticulated, every parameter to a function, every function return position,
 and every class field can be typed or untyped.
This is a much finer @emph{granularity} than Typed Racket's, in which entire
 modules must be typed as a unit.
The added flexibility means that an experimenter must choose whether to explore:
 coarse, module-grained mixes;
 fine, function-parameter mixes;
 or something in between.

@definition["granularity"]{
  The @emph{granularity} of an experiment is the syntactic unit at which
   it adds/removes type annotations.
}

For example, @citet{tfgnvf-popl-2016} evaluate Typed Racket at the granularity
 of modules.
@citet{vss-popl-2017} evaluate Reticulated at the granularity of whole programs,
 and @citet{gm-pepm-2018} evaluate Reticulated at the granularity of whole
 functions and whole class field sets (@section-ref{sec:rp:protocol}).

After choosing a granularity, an experimenter must pick a suite of
 programs to measure.
A potential complication is that programs may depend on external libraries
 or other modules that lie outside the realistic scope of the evaluation.

@definition["migratable, contextual"]{
  The @emph{migratable} code in a program defines its configurations.
  The @emph{contextual} code in a program is common across all configurations.
}

The granularity and the migratable code define the
 @emph{configurations} of a fully-typed program.

@definition["configurations"]{
  Let @${C \tcstep C'}
   if and only if program @${C'} can be obtained from program
   @${C} by annotating one migratable syntactic unit.
  Let @${\tcmulti} be the reflexive, transitive closure of the @${\tcstep}
   relation.
  The @emph{configurations} of a fully-typed program @${C_\tau} are all
   programs @${C} such that @${C\!\tcmulti C_\tau}.
  Furthermore, @${C_\tau} is a @emph{fully-typed configuration}.
  An @emph{untyped configuration} @${C_\lambda} has the property @${C_\lambda\!\tcmulti C}
   for all configurations @${C}.
}

@|noindent|In terms of prior work, the @${\tcstep} relation includes all
 possible @emph{type conversion steps}@~cite{tfgnvf-popl-2016,gtnffvf-jfp-2019}.
The @${\tcmulti} relation corresponds to @emph{term precision}@~cite{svcb-snapl-2015} as follows:
 @${e_0 \tcmulti\, e_1 \mbox{ only if } e_1 \sqle e_0}.

An evaluation must measure overhead relative to a useful baseline.
For migratory typing, the correct baseline is the original host-language program.

@definition["baseline"]{
 The @emph{baseline performance} of a program is its running time in the absence
  of migratory typing.
}

In Typed Racket, the baseline is the performance of Racket running the
 untyped configuration.
In Reticulated, the baseline is Python running the untyped configuration.
Be advised, Python-running-untyped differs from Reticulated-running-untyped
 because Reticulated inserts checks in every migratable module that
 it sees@~cite{vksb-dls-2014}.

@definition["performance ratio"]{
  A @emph{performance ratio} is the running time of a configuration
   divided by the baseline performance.
}

An exhaustive performance evaluation measures the performance of every
 configuration.
To summarize the data, choose a notion of 
 ``good performance'' and count the proportion of ``good'' configurations.
In this spirit, @citet{tfgnvf-popl-2016} ask programmers to consider the
 performance overhead they could deliver to clients.

@definition[@ddeliverable{D}]{
  A configuration is @emph{@ddeliverable{D}},
   for some @$|{D \in \mathbb{R}^{+}}|,
   if its performance ratio is no greater than @${D}.
}


@subsection[#:tag "sec:perf:limits"]{Known Limitations}

@; TODO overhead plot anonymize

Evaluation begins with a fixed set of types, but there are usually many
 ways to type a piece of code.
Consider the application of an identity function to a number:

@exact{\medskip}
@untyped-codeblock['(
  "((λ(x) x) 61)"
)]

@|noindent|In Typed Racket, the parameter @tt{x} can be given infinitely many
 correct types.
The obvious choices are @tt{Integer} and @tt{Number},
 but other base types work, including @tt{Real} and @tt{Natural}.
Untagged unions bring many options: @tt{(U Real String)}, @tt{(U Real String (-> Boolean))},
 and so on.
Different choices entail different run-time checks, but the method lacks
 a systematic way to explore equally-valid typings.

Along the same lines, the definition of granularity does not talk about
 imprecise types.
In Reticulated, the type @tt{Function([Str], Int)} has three
 less-precise variants that incorporate the dynamic type.
The method only looks at one fully-typed variant, but the others may
 have notable performance implications.

@; TODO 3rd issue = anonymous configs
Overhead plots (@figure-ref{fig:overhead-plot-example}) rest on two assumptions.
@; - assn: log scale
First is that configurations with less than 2x overhead
 are significantly more practical than configurations with a 10x overhead or more.
Hence the plots use a log-scaled x-axis to encourage
 fine-grained comparison in the 1.2x to 1.6x overhead range and to blur the
 distinction among larger numbers.
@; - assn: 20x
Second is that configurations with more than @id[tr:MAX-OVERHEAD]x
 overhead are completely unusable in practice.
Pathologies like the 1000x slowdowns in @figure-ref{fig:example-lattice}
 represent a challenge for implementors, but if these overheads suddenly
 dropped to 30x, the configurations would still be useless to developers.

The main limitation of exhaustive evaluation, however, is its exhaustiveness.
With 20 migratable units, an experiment requires over 1 million measurements.
At a module-level granularity, this limit is somewhat reasonable because each
 module can be arbitrarily large.
But at function-level granularity and finer, the practical limit quickly rules
 out interesting programs.


@section{Approximate Evaluation Method}

The proportion of @ddeliverable{D} configurations in a program can be
 approximated using random sampling.
First, choose several configurations and measure the proportion of
 @ddeliverable{D} configurations in the sample.
Next, repeat the experiment several times.
Combining the proportions in a confidence interval provides an estimate
 for the true proportion.

@definition[@sraapproximation["r" "s" "95"]]{
  Given @${r} samples each containing @${s} configurations chosen uniformly at random,
   a simple random approximation is a @${95\%} confidence interval for
   the proportion of @ddeliverable{D} configurations in each sample.
}

Intuitively, this method should lead to good results because
 it randomly samples a stable population.
If the true proportion of @ddeliverable{D} configurations in a program
 happens to be 10%, then a random configuration has a 1 in 10
 chance of being @ddeliverable{D}.

A statistical justification depends on the law of large numbers
 and the central limit theorem.
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
The central limit theorem implies that any sequence of
 such averages is normally distributed around the true proportion.
A @${95\%} confidence interval generated from sample averages is therefore
 likely to contain the true proportion.

The statistical argument reveals two weaknesses:

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
  In the extreme, a totally useless interval says that 0% to 100% of configurations
   are @ddeliverable{D}.
}
]

@|noindent|The argument does say, however, that an interval is very likely to
 be useful if it is based on a huge number of samples each with a huge number
 of configurations.
The challenge is to find parameters that engineer a compromise between
 size and precision.

@render-overhead-plot*[
  "fig:tr:validate-sample"
  @elem{Typed Racket sample validation.}
  ""
  (tr:make-render-validate-plot "7.7")
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

By comparing sample data to the ground-truth from an exhaustive evaluation,
 I have found that linear sampling gives small and accurate intervals.
@Figure-ref["fig:tr:validate-sample" "fig:rp:validate-sample"] demonstrate
 on a few Typed Racket and Reticulated programs.
The blue curve and shaded area on each plot is the exhaustive data.
The orange interval is a 95% confidence interval based on
 @${r\!=\!@id[NUM-SAMPLE-TRIALS]} each containing
 @${s\!=\!@id[SAMPLE-RATE]\!*\!N} configurations,
 where @${N} is the number of typed units in the benchmark program.
The sample intervals all tightly cover the true proportion
 of @ddeliverable{D} configurations.


@subsection{Statistical Protocol}

For readers interested in reproducing the above results, here are
 additional details about 
 the protocol behind @figure-ref["fig:tr:validate-sample" "fig:rp:validate-sample"].

To generate one random sample, select @${@id[SAMPLE-RATE]\!*\!N} configurations
 uniformly at random and compute their overhead.
Sampling with replacement gives the same theoretical results as sampling
 without replacement.
The figures employ sampling without replacement in the hope of finding
 new configurations with exceptional overhead.

To generate a confidence interval for the number of @ddeliverable{D}
 configurations based on a group of samples, calculate
 the proportion of @ddeliverable{D} configurations in each sample and generate
 a 95% confidence interval from the proportions.
This is the simple index method for computing a
 confidence interval from a sequence of ratios (@format-url{https://arxiv.org/pdf/0710.2024v1.pdf}).
A more advanced method may give tighter intervals, if extra precision is needed@~cite{f-rss-1957}.


@section{Benchmark Selection}

Representative benchmarks are difficult to come by.
My best-effort approach is to seek out programs that serve a realistic
 purpose.
Several implement games, and re-play a game round.
Others adapt library code with an example use.
All of the forthcoming Typed Racket benchmarks follow this approach (@section-ref{sec:tr:benchmarks}).
Many of the Reticulated benchmarks (@section-ref{sec:rp:benchmarks}), however, come from prior work and
 are smaller scripts in the spirit of the Gabriel benchmarks@~cite{g-1985}.


@subsection[#:tag "sec:conversion"]{From Programs to Benchmarks}

To convert a program into a benchmark, we:
@itemlist[#:style 'ordered
@item{
  partition the program into migratable and contextual code;
}
@item{
  build a migratable driver module that runs the program and collects timing information;
}
@item{
  remove any non-determinism or I/O actions;
}
@item{
  find types for the migratable code.
}]

The final step, finding types for untyped code, can be difficult.
First, the type checker may require casts or refactorings to deal with
 untyped code.
For example, untyped Racket code may assume that the
 application @tt{(string->number "42")} returns an integer.
The assumption is correct, but the type checker cannot follow the reasoning
 and needs a run-time check.
Reticulated does not have union types, and therefore falls back to @tt{Dyn}
 for many common untyped patterns.
An experimenter must choose whether to rewrite the pattern or accept the
 trivial typing.

Second, some @|sdeep| type boundaries may lack run-time support.
Typed Racket cannot enforce the type @tt{(U (-> Real) (-> Integer))}
 at a boundary because its contracts lack unions for higher-order wrappers.
The work-around is to rewrite the boundaries or, if possible, simplify
 the types.
For the above, @tt{(-> Real)} is a viable choice.

Third, each static import of a struct type into Typed Racket code
 generates a unique datatype.
Typed modules that share instances of an untyped struct must therefore
 reference a common definition.
Typed Racket benchmarks with this issue include additional contextual
 modules, called @emph{adaptor modules}, to provide a canonical import.



@; -----------------------------------------------------------------------------
@section[#:tag "sec:tr:evaluation"]{Application 1: Typed Racket}

This section presents an exhaustive evaluation of Typed Racket v7.7
 on a set of @integer->word[tr:num-benchmarks] benchmark programs;
 namely, the GTP suite v@|tr:gtp-version| (@|gtp-url|).
The main purpose of this evaluation is to confirm that the
 exhaustive method provides a useful summary of a mixed-typed language.
A secondary result is that it reveals performance challenges
 that Typed Racket must overcome.


@subsection[#:tag "sec:tr:protocol"]{Protocol}

@parag{Granularity}
The granularity of this evaluation is @emph{modules}, same as the granularity
 of Typed Racket.
One syntactic unit in the experiment is one entire module.


@parag{Data Collection}

For each configuration in each benchmark, a control script compiled
 the whole program, ran once ignoring performance, and ran four more times
 collecting data.
These actions used the standard Racket 7.7 BC bytecode compiler,
 JIT compiler, and runtime settings.
@(let ((num-processors 1)
       (num-proc-cores NSA-num-cores)
       (processor-name NSA-core-name)
       (proc-speed NSA-core-speed)
       (proc-ram NSA-RAM)) @elem{
The control script ran on a dedicated Linux machine with a @id[processor-name]
 processor.
The processor has @id[proc-ram] RAM and @integer->word[num-proc-cores] cores,
 and ran at @|NSA-core-speed|.
})


@subsection[#:tag "sec:tr:benchmarks"]{Benchmarks}

This section summarizes @integer->word[tr:num-benchmarks] benchmark programs.
These benchmarks are sorted in order of increasing size, as measured by the
 number of migratable modules.
Each summary comes with four fields:
 @emph{Origin} indicates the benchmark's source,
 @emph{Purpose} describes what it computes,
 @emph{Author} credits the original author,
 and @emph{Depends} lists significant contextual libraries that the benchmark depends on.

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
}
@bm-desc[
  @bm{forth}
  #:author "Ben Greenman"
  #:origin "Library"
  #:purpose "Forth interpreter"
  #:url "http://docs.racket-lang.org/forth/index.html"]{
  Interprets Forth programs.
  The interpreter represents calculator commands as a list of first-class objects.
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
  The map interacts with Racket's untyped @library{graph} library.
}
@bm-desc[
  @bm{morsecode}
  #:author "John Clements and Neil Van Dyke"
  #:origin "Library"
  #:purpose "Morse code trainer"
  #:url "https://github.com/jbclements/morse-code-trainer/tree/master/morse-code-trainer"]{
  Computes Levenshtein distances@~cite{l-spd-1966} and morse code translations for a fixed sequence of pairs of words.
}
@bm-desc[
  @bm{zombie}
  #:author "David Van Horn"
  #:origin "Research"
  #:purpose "Game"
  #:url "https://github.com/philnguyen/soft-contract"]{
  Implements a game where players dodge ``zombie'' tokens.
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
  Originally, the program imported the Racket @library{math} library
   for array operations.
  The benchmark uses Racket's vectors instead of the @library{math} library's arrays
   because Typed Racket v6.2 cannot compile the type @rkt{(Mutable-Array (Class))} to a contract.
  @;and @library{racket/dict} for a generic dictionary interface.  
  @; TODO why remove dict?
  @;The benchmark avoids @library{racket/dict} to avoid a type boundary.
}
@bm-desc[
  @bm{jpeg}
  #:author "Andy Wingo"
  #:origin "Library"
  #:purpose "JPEG toolkit"
  #:url "http://github.com/wingo/racket-jpeg"
  #:depends (list (make-lib "math/array" "https://docs.racket-lang.org/math/array.html")
                  (make-lib "rnrs/bytevectors-6" "https://docs.racket-lang.org/r6rs/R6RS_Libraries.html#(mod-path._rnrs%2Fbytevectors-6)"))]{
  Parses a bytestream of JPEG data to an internal representation, then serializes the result.
}
@bm-desc[
  @bm{zordoz}
  #:author "Ben Greenman"
  #:origin "Library"
  #:purpose "Explore bytecode"
  #:url "http://github.com/bennn/zordoz"
  #:depends (list (make-lib "compiler-lib" "http://docs.racket-lang.org/raco/decompile.html#%28mod-path._compiler%2Fdecompile%29"))]{
  Traverses Racket bytecode (@tt{.zo} files).
  The untyped @library{compiler-lib} library defines the bytecode data structures.
}
@bm-desc[
  @bm{lnm}
  #:author "Ben Greenman"
  #:origin "Synthetic"
  #:purpose "Data visualization"
  #:depends (list
              (make-lib "plot" "https://docs.racket-lang.org/plot/")
              (make-lib "math/statistics" "https://docs.racket-lang.org/math/stats.html"))]{
  Renders overhead plots.
  Two modules are tightly-coupled to Typed Racket libraries.
}
@bm-desc[
  @bm{suffixtree}
  #:author "Danny Yoo"
  #:origin "Library"
  #:purpose "String tools"
  #:url "https://github.com/dyoo/suffixtree"]{
  Implements Ukkonen's suffix tree algorithm@~cite{u-a-1995}
   and computes longest common subsequences between strings.
}
@bm-desc[
  @bm{kcfa}
  #:author "Matt Might"
  #:origin "Educational"
  #:purpose "Explanation of k-CFA"
  #:url "http://matt.might.net/articles/implementation-of-kcfa-and-0cfa/"]{
  Performs 1-CFA on a lambda calculus term that computes @$|{~2*(1+3) = 2*1 + 2*3}| via Church numerals.
  The (mutable) binding environment flows throughout functions in the benchmark.
}
@bm-desc[
  @bm{snake}
  #:author "David Van Horn"
  #:origin "Research"
  #:purpose "Game"
  #:url "https://github.com/philnguyen/soft-contract"]{
  Implements the Snake game; the benchmark replays a fixed sequence of moves.
}
@bm-desc[
  @bm{take5}
  #:author "Matthias Felleisen"
  #:origin "Educational"
  #:purpose "Game"]{
  Manages a card game between AI players.
}
@bm-desc[
  @bm{acquire}
  #:author "Matthias Felleisen"
  #:origin "Educational"
  #:purpose "Game"
  #:url "https://github.com/mfelleisen/Acquire"]{
  Simulates a board game via message-passing objects.
  These objects encapsulate the core data structures and seldom cross module boundaries.
}
@bm-desc[
  @bm{tetris}
  #:author "David Van Horn"
  #:origin "Research"
  #:purpose "Game"
  #:url "https://github.com/philnguyen/soft-contract"]{
  Replays a pre-recorded game of Tetris.
}
@bm-desc[
  @bm{synth}
  #:author "Vincent St. Amour and Neil Toronto"
  #:origin "Application"
  #:purpose "Music synthesis DSL"
  #:url "http://github.com/stamourv/synth"]{
  Converts a description of notes and drum beats to @tt{WAV} format.
  Modules in the benchmark come from two sources, a music library and an array library.
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
  (list @bm{quadT} @bm{quadU})
  #:author "Matthew Butterick"
  #:origin "Application"
  #:purpose "Typesetting"
  #:url "https://github.com/mbutterick/quad"
  #:depends (list (make-lib "csp" "https://github.com/mbutterick/csp"))]{
  Converts S-expression source code to @tt{PDF} format.
  The two versions of this benchmark came from the original author.
  First, @bm{quadU} is based on a foundational untyped codebase.
  Second, @bm{quadT} comes from a migrated, typed codebase with slightly
   different behavior.
  Overhead is worse in @bm{quadT}, but the types in @bm{quadU}
   are far less descriptive.

  @; To give a concrete example of different types, here are the definitions
  @;  for the core @tt{Quad} datatype from both @bm{quadMB} and @bm{quadBG}.
  @; @racket[(define-type QuadMB (Pairof Symbol (Listof QuadMB)))]
  @; @racket[(define-type QuadBG (Pairof Symbol (Listof Any)))]
  @; The former is a homogenous, recursive type.
  @; As such, the predicate asserting that a value has type @racket[QuadMB] is a linear-time tree traversal.
  @; The predicate for @racket[QuadBG] runs significantly faster.
}

@Figure-ref{fig:tr:static-benchmark} tabulates the size of the migratable
 code in the benchmark programs.
The column labeled @bold{N} reports the number of migratable modules;
 the configuration space for each program has @${2^N} points.
The SLOC column reports lines of code in the fully-typed configuration.
With type annotations, these benchmarks gain between 10 and 300 lines of
 code.
For details about the graph structure of each benchmark and
 the specific types on boundaries, refer to the GTP web page: @|gtp-url|.

@figure*["fig:tr:static-benchmark" @elem{
  Static characteristics of the migratable code in the @|GTP| benchmarks.
  @bold{N} = number of components = number of modules.
  SLOC = source lines of fully-typed code as reported by @|SLOCCOUNT|.}
  @tr:render-static-information[tr:ALL-BENCHMARKS]
]


@subsection[#:tag "sec:tr:ratio"]{Performance Ratios}

@(let* ((RT (tr:get-ratios-table tr:ALL-BENCHMARKS))
        (num-total (length RT))
        (faster-typed (for/list ((rr (in-list RT))
                                 #:when (< (string->number (caddr rr)) 0.91))
                        (car rr)))
        (num-faster-typed (length faster-typed))
       ) @list[
@figure[
  "fig:tr:ratio"
  @elem{Coarse ratios for the @|GTP| benchmarks v@|tr:gtp-version| on Racket v@|tr:default-rkt-version|.}
  @tr:render-ratios-table[RT]
]
@para{
@Figure-ref{fig:tr:ratio} lists the overhead of fully-typed code relative
 to untyped code.
In @bm{sieve}, for example, the typed configuration runs slightly
 faster than untyped.
In @bm{mbta}, the typed configuration is over 1.5x slower
 because of a boundary to an untyped contextual module.

Overall, many benchmarks run significantly faster with types
 (@id[num-faster-typed] of @id[num-total]).
These programs have few boundaries to untyped contextual modules
 and benefit from type-directed compilation.
The highest ratios stay within a modest 2x overhead.
}])

@; -----------------------------------------------------------------------------
@subsection[#:tag "sec:tr:overhead"]{Overhead Plots}

@render-overhead-plot*[
  "fig:tr:overhead"
  "Typed Racket overhead plots"
  overhead-long-caption
  (tr:make-render-overhead-plot "7.7")
  (map tr:benchmark-name tr:ALL-BENCHMARKS)
  tr:cache-dir]

@Figures-ref["fig:tr:overhead" (exact-ceiling (/ (length tr:ALL-BENCHMARKS) overhead-plots-per-page))]
 present an exhaustive evaluation in a series of overhead plots.
As in @figure-ref{fig:overhead-plot-example}, the plots are cumulative
 distribution functions for the proportion of @ddeliverable[] configurations.

@; -- overall, bleak picture
@(let* ([num-bm tr:num-benchmarks]
        [num-bm-str (integer->word num-bm)]
        [num-configs (tr:*TOTAL-NUM-CONFIGURATIONS*)]
        [max-str (format "~a" tr:MAX-OVERHEAD)]
        [suffixtree-num-modules (integer->word 6)]
        [num-max-deliverable (length '(forth fsmoo zombie dungeon jpeg suffixtree take5 synth quadU quadT))]
        [num-max-deliverable-str (integer->word num-max-deliverable)]
        [num-fail-2d (length '(sieve forth fsmoo zombie dungeon jpeg suffixtree kcfa snake take5 acquire tetris synth quadT quadU))]
        [num-fail-10d (length '(forth fsmoo zombie dungeon jpeg suffixtree take5 synth quadU))]
        [format-% (lambda (n) (format "~a%" (round (* 100 (/ n num-configs)))))]
        [lo (number->string (tr:*LO*))]
        [hi (number->string (tr:*HI*))]
        [v-max tr:default-rkt-version]
        [lo-prop (format-% (tr:deliverable* (tr:*LO*) v-max tr:ALL-BENCHMARKS))]
        [hi-prop (format-% (tr:deliverable* (tr:*HI*) v-max tr:ALL-BENCHMARKS))]
       ) @elem{
Many curves are quite flat; they demonstrate that migratory typing introduces
 large and widespread performance overhead in the corresponding benchmarks.
Among benchmarks with fewer than @|suffixtree-num-modules| modules, the
 most common shape is a flat line near the 50% mark.
Such lines imply that the performance of a group of configurations is
 dominated by a single type boundary.
For instance, there is one type boundary in @bm{fsmoo} that adds overwhelming
 slowdown when present; all eight configurations with this boundary have over
 @|max-str| overhead.
Benchmarks with @|suffixtree-num-modules| or more modules generally have
 smoother slopes, but five such benchmarks have essentially flat curves.
The overall message is that for many values of @${D} between 1 and
 @|max-str|, few configurations are @ddeliverable{}.

In @id[num-fail-2d] benchmarks, no more than half the configurations
 are @ddeliverable{2}.
This is quite bad.
The situation is worse for lower (more realistic) overheads, and rarely
 improve at slightly higher overheads.
Even at a generous 10x factor, no more than half the configurations in
 @integer->word[num-fail-10d] benchmarks are good enough.

The curves' endpoints describe the extremes of migratory typing.
The left endpoint gives the percentage of configurations that run at least
 as quickly as the untyped configuration.
With few exceptions, notably @bm{lnm}, these configurations are a low proportion of the total.
The right endpoint shows how many configurations suffer at least 20x overhead.
@string-titlecase[num-max-deliverable-str] benchmarks have at least one such configuration.

In summary, the application of the evaluation method projects a negative
 image of Typed Racket's sound migratory typing.
Only a small number of configurations in the benchmark suite run with low
 overhead; a mere @|lo-prop| of all configurations are @ddeliverable[lo] on
 Racket v@|v-max|.
Many demonstrate extreme overhead; only @|hi-prop| of all configurations are
 @ddeliverable[hi] on v@|v-max|.
})


@subsection[#:tag "sec:tr:threats"]{Threats to Validity}

The concerns raised in @section-ref{sec:conversion} affect this evaluation.
In particular, each benchmark explores one choice of types.
Different types may lead to different conclusions, as @bm{quadT} and
 @bm{quadU} demonstrate at a small scale.

Some benchmarks use a different modularization than the original program.
The @bm{kcfa} benchmark is modularized according to comments in the original,
 single-module program.
The @bm{suffixtree}, @bm{synth}, and @bm{gregor} benchmarks each have a single
 file containing all their data structure definitions, but the original programs
 defined these structures in the same module as the functions on the structures.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:rp:evaluation"]{Application 2: Reticulated Python}

Reticulated Python is the original home of the @|stransient| semantics for
 mixed-typed programs@~cite{v-thesis-2019}.
@|sTransient| is a type-sound semantics (@chapter-ref{chap:design})
 that does not rely on higher-order wrappers or full run-time checks.
Instead, @|stransient| uses light ``shape checks'' throughout typed code.
One would expect fast performance from @|stransient| on mixed-typed code.
The first-ever evaluation, however, only reports data for untyped and fully-typed
 programs@~cite{vss-popl-2017}.

This section presents a systematic evaluation of
 Reticulated without its experimental blame algorithm@~cite{gm-pepm-2018}.
The data offers a big-picture view of @|stransient|,
 further validates the approximate method,
 and identifies bugs in the measured version of Reticulated.
Overall, @|stransient| checks never exceed a 10x slowdown in the benchmarks.


@subsection[#:tag "sec:rp:protocol"]{Protocol}

@parag{Granularity}
The granularity of this evaluation is @emph{function and class fields}.
One syntactic unit in the experiment is either one function,
 one method, or the collection of all fields for one class.
@Figure-ref{fig:rp:example-class} demonstrates this granularity with
 a simple Reticulated module.
The class @tt{Cash} has two fields and one method that requires three arguments;
 the module also include a function that instantiates a @tt{Cash} object with exactly
 5 dollars.
Reticulated permits the removal of every type in the figure, giving
 @${128} possible configurations.
The granularity for our experiment, however, explores the @${8} configurations
 obtained by removing types from the field declaration, the method, and/or the
 function each as a complete unit.

@figure*[
  "fig:rp:example-class"
  @elem{Reticulated code that leads to @${2^3} configurations in the experiment, but supports @${2^7} total.}
  @typed-codeblock['(
    "@fields({\"dollars\": Int, \"cents\":Int})"
    "class Cash:"
    "  def __init__(self:Cash, d:Int, c:Int)->Void:"
    "    self.dollars = d"
    "    self.cents = c"
    ""
    "def make_five()->Cash:"
    "  return Cash(5, 0)"
    )]
]

@parag{Data Collection}
The data is exhaustive for benchmarks with at most @$|{2^{17}}| configurations
 and approximate for larger benchmarks.
The approximations use @integer->word[NUM-SAMPLE-TRIALS] samples
 each containing @${@id[SAMPLE-RATE]\!*\!(F + C)} configurations,
 where @${F} is the number of functions in the benchmark and
 @${C} is the number of classes.

All data comes from jobs that we ran on the
 @hyperlink["https://kb.iu.edu/d/bezu"]{@emph{Karst at Indiana University}}
 computing cluster.
Each job:
@itemlist[#:style 'ordered
@item{
  reserved all processors on one node;
}
@item{
  downloaded fresh copies of @|rp:PYTHON|
  and Reticulated commit @|rp:retic-commit|;
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
All data collection scripts are online:
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
  @bm{fannkuch}
  #:author "Sokolov Yura"
  #:origin PYBENCH
  #:purpose "Test integers, vectors"
  #:url "https://github.com/python/performance"]{
  Implements a classic LISP microbenchmark@~cite{ar-lp-1994}.
  @; 1 iteration
}
@bm-desc[
  @bm{nqueens}
  #:author unknown-author
  #:origin PYBENCH
  #:purpose "Puzzle"
  #:url "https://github.com/python/performance"]{
  Solves the @hyperlink["https://developers.google.com/optimization/puzzles/queens"]{@${8}-queens} problem by a brute-force algorithm.
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
  @bm{nbody}
  #:author "Kevin Carson"
  #:origin PYBENCH
  #:purpose "Test float ops"
  #:url "https://github.com/python/performance"]{
  Models the orbits of Jupiter, Saturn, Uranus, and Neptune.
  @; 1 iteration
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
  @bm{spectralnorm}
  #:author "Sebastien Loisel"
  #:origin PYBENCH
  #:purpose "Test arithmetic"
  #:url "https://github.com/python/performance"]{
  Computes the largest singular value of an infinite matrix.
  @; 10 iterations
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
  @bm{pystone}
  #:author "Chris Arndt"
  #:origin PYBENCH
  #:purpose "Test integer ops"
  #:url "https://github.com/python/performance"]{
  Implements Weicker's @hyperlink["http://www.eembc.org/techlit/datasheets/ECLDhrystoneWhitePaper2.pdf"]{Dhrystone} benchmark.
  @; 50,000 iterations
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
  Creates fractals using the @hyperlink["https://en.wikipedia.org/wiki/Chaos_game"]{chaos game} method.
  @; 1 iteration
}
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
  @bm{take5}
  #:author "Maha Alkhairy and Zeina Migeed"
  #:origin "Educational"
  #:purpose "Game"
  #:depends @list[ @rp:lib-desc["random"]{randrange shuffle random seed} @rp:lib-desc["copy"]{deepcopy}]]{
  Implements a card game and a simple player AI.
  @; 500 iterations
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
  Static summary of the migratable code in the Reticulated benchmarks.
  @bold{N} = number of components = functions + classes + methods.
  SLOC = source lines of code as reported by @|SLOCCOUNT|.}
  @rp:render-static-information[rp:MAIN-BENCHMARKS]]

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
  The number of mixed-typed configurations in the experiment, however, is
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
@figure[
  "fig:rp:ratio"
  @elem{Performance ratios for three important points in
  a configuration space: fully-typed code (typed), untyped code run through
  Reticulated (retic), and untyped code run via Python (python).}
  @rp:render-ratios-table[rp:RT]
]

@elem{
The table in @figure-ref{fig:rp:ratio} lists the endpoints of migratory typing in
 Reticulated.
From left to right, these are:
 the performance of the untyped configuration relative to the Python baseline (the @id[rp:u/p-ratio]),
 the performance of the fully-typed configuration relative to the untyped configuration (the @id[rp:t/u-ratio]),
 and the overall delta between fully-typed and Python (the @id[rp:t/p-ratio]).

For example, the row for @bm{futen} reports a @|rp:u/p-ratio| of @${@|futen-u/p|}.
This means that the average time to run the untyped configuration of the
 @bm{futen} benchmark using Reticulated is that much slower than the
 average time of running the same code using Python.
The @|rp:t/u-ratio| for @bm{futen} states that the fully-typed configuration
 is @${@|futen-t/u|} times slower than the untyped configuration.

Migrating a benchmark to Reticulated, or from untyped to fully-typed, always
 adds performance overhead.
This overhead is always within one order of magnitude.

@; Regarding the @|rp:u/p-ratio|s:
@;  @integer->word[rp-<2] are below @${2}x,
@;  @integer->word[(- rp-<3 rp-<2)] are between @${2}x and @${3}x, and
@;  the remaining @integer->word[(- rp-<4.5 rp-<3)] are below @${4.5}x.
@; The @|rp:t/u-ratio|s are typically lower:
@;  @integer->word[tr-<2] are below @${2}x,
@;  @integer->word[(- tr-<3 tr-<2)] are between @${2}x and @${3}x,
@;  and the final @integer->word[(- tr-<3.5 tr-<3)] are below @${3.5}x.

@Integer->word[num->] benchmarks have @|rp:u/p-ratio|s that are larger
 than their @|rp:t/u-ratio|s.
One would expect @|rp:u/p-ratio|s close to 1 because
 untyped Reticulated need not differ from Python.
But Reticulated duplicates some of Python's run-time checks.
For example, Reticulated checks that a method is bound before proceeding
 with method dispatch.
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

@Figures-ref["fig:rp:overhead" (exact-ceiling (/ (length rp:MAIN-BENCHMARKS)
 overhead-plots-per-page))] summarize the overhead of migratory typing in the
 benchmark programs.
Each plot reports the percent of @ddeliverable[] configurations (@|y-axis|)
 for values of @${D} between @${1}x overhead and @${@id[rp:MAX-OVERHEAD]}x overhead (@|x-axis|).
The heading above a plot states the benchmark's name and indicates whether the
 data is exhaustive or approximate.
Exhaustive plots show the total number of configurations.
Approximate plots show the number of samples
 and the number of randomly-selected configurations in each sample.

The curves for the approximate data---@bm{sample_fsm}, @bm{aespython}, and @bm{stats}---are intervals
 rather than fixed-width lines.
For instance, the height of an interval at @${x\!=\!4} is the range of the
 @sraapproximation[NUM-SAMPLE-TRIALS (format "[~a(F+C)]" SAMPLE-RATE) "95"]
 for the number of @ddeliverable[4] configurations.
These intervals are thin because there is little variance in the proportion
 of @ddeliverable{D} configurations across the @integer->word[NUM-SAMPLE-TRIALS]
 samples, but the @bm{sample_fsm} curve is slightly thicker than the
 @bm{aespython} curve.

Curves in these figures typically cover a large area and reach the
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
Unfortunately, this speedup comes from a soundness bug that we discovered
 thanks to this performance evaluation.
Reticulated
 at commit @|rp:retic-commit| does not type-check the contents of tuples
 (@github-issue["mvitousek" "reticulated" 36]).
})


@subsection[#:tag "sec:rp:threats"]{Threats to Validity}

We have identified five sources of systematic bias.
Three have been noted above:
 the decision to measure one set of type annotations (@section-ref{sec:perf:limits}),
 the coarse granularity (@section-ref{sec:rp:protocol}),
 and the imprecision of Reticulated types (@section-ref{sec:conversion}).
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
  Here, we can offer a few details on type-expressiveness.
  The @bm{take5} benchmark contains one function that must stay untyped because
   it accepts optional arguments (@github-issue["mvitousek" "reticulated" 32]).
  The @bm{go} benchmark uses dynamic typing because Reticulated cannot validate
   its use of a recursive class definition.
  @Integer->word[(length retic-limited)] other benchmarks, @format-bm*[retic-limited],
   use dynamic typing to overcome Reticulated's lack of
   untagged union types.
})

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
  A third issue is that the experiment uses rather small benchmarks.
  The @|rank-info| shows that widely-used
   Python packages have far more functions and methods (accessed 2018).
  @|lib-info|.
})

@(let ([use-io* '(aespython futen http2 slowSHA)]) @elem{
  Fourth and last, the @(authors* (map bm use-io*)) benchmarks read from a file
   within their timed computation.
  Despite the unpredictable running times of system calls,
   we believe our results are representative.
})


@; -----------------------------------------------------------------------------
@section{Additional Visualizations}

The method presented in this chapter targets our most effective answer
 to the question of how to evaluate the performance of a mixed-typed language.
In particular, the notion of @ddeliverable{} configurations is a
 clear and scalable way to summarize performance.
But a mixed-typed language has other interesting properties.
This section presents visualizations that help answer different, targeted
 questions.


@subsection[#:tag "sec:perf:exact"]{Exact Runtime Plots}

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
Unfortunately, this method hides outliers in the data
 and syntactic relations (think back to the lattice, @figure-ref{fig:example-lattice-0})
 among configurations.

@Figure-ref{fig:example-exact-plot} addresses both concerns.
Instead of summarizing one configuration with its average runtime,
 the plot contains one point for every running time in the dataset.
These points are spread left-to-right in one of the @integer->word[(+ 1 num-units)]
 columns of the figure.
If a plot like this does not consist of distinct, horizontal lines, the
 underlying dataset may have irregular running times.
Each column contains all configurations that have the same number of types.
In terms of the configuration lattice (@figure-ref{fig:example-lattice-0}),
 the left-most column contains the bottom level and each successive column
 present a higher levels.
At a glance, @figure-ref{fig:example-exact-plot} therefore shows the overall
 effect of adding types.
}])


@subsection{Relative Scatterplots}

@(let ((bm-name 'morsecode)) @list[
@figure*[
  "fig:example-scatterplot"
  @elem{Scatterplot comparing @bm[bm-name] configurations before
   and after collapsible contracts.
   The @|x-axis| ranges over collapsible overhead and the @|y-axis|
    ranges over baseline overhead.
   A point @${(x, y)} is a head-to-head comparison;
    points above the diagonal are better for collapsible.
  }
  (tr:render-scatterplot-example bm-name)
]

@elem{
Collapsible contracts are a new representation for @|sdeep| run-time type
 checks@~cite{fgsfs-oopsla-2018}.
The representation greatly improves some mixed-typed programs, but can
 slow down others.
To assess the implementation of collapsible,
 we used a scatterplot technique due to Spenser Bauman@~cite{bbst-oopsla-2017}.
@Figure-ref{fig:example-scatterplot} shows one representative example from our work.
Each point in the scatterplot shows how collapsible affects one configuration.
Points above the diagonal line are improved;
 points below the line get worse with collapsible contracts.
More precisely, a point @${(X, Y)} shows the overhead in both systems.
The first coordinate, @${X}, is the overhead with collapsible.
The @${Y} coordinate is the baseline overhead, without collapsible.
If collapsible always led to a lower overhead, then all
 point would lie above the @${X=Y} line (because @${X < Y}).
}])


@subsection{Best-Path Plots}
@; TR = zombie, suffixtree, take5, snake, synth, quadU, quadT
@; RP = go, Espionage, PythonFlow ?

@(let ((bm-name 'suffixtree)) @list[
@render-overhead-plot*[
  "fig:example-path"
  @elem{Overhead plots for @bm[bm-name] comparing the base slowdown to
   the best-possible improvement after adding types to 1 (top) and 2 (bottom)
   modules.
  }
  ""
  (tr:make-render-path-plot "7.7")
  (list (cons bm-name 1) (cons bm-name 2))
  tr:cache-dir
]

@elem{
The plots in @section-ref{sec:tr:evaluation} paint a bleak picture of
 Typed Racket.
Many benchmarks have many configurations that run far slower than the
 untyped code.
A natural question, though, is whether these results are brittle.
If a programmer can escape the slow configurations by converting one or
 two more modules, then the bleak conclusion may be unwarranted.

@Figure-ref{fig:example-path} presents two plots for the @bm[bm-name] benchmark
 that compare the original data against the best-possible performance after
 converting additional modules.
In the top plot, a configuration is @ddeliverable{X} in the orange curve
 if it can reach a @ddeliverable{X} configuration from the blue curve after
 adding types to one module.
Similarly, the bottom plot shows the best-possible performance after typing
 two untyped modules.

The plots show that significant improvements are possible, but not guaranteed
 even in the best case.
Other large benchmarks typically show similar patterns@~cite{gtnffvf-jfp-2019}.
}])

