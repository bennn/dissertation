#lang greenman-thesis/include

@(require
   (only-in greenman-thesis/oopsla-2019/pict
     both:DS0
     both:DS1))

@title[#:tag "chap:future"]{Future Work}

Now that we have a language that provides @|sdeep| types via the @|snatural|
 semantics and @|sshallow| types via the @|stransient| one, two lines of
 crucial future work are apparent: improving blame and improving the performance
 of @|stransient|.


@section{@|sTransient| with Blame, @|sNatural| without Blame}

The most surprising result of my research is the
 huge cost of @|stransient| blame (@section-ref{sec:transient:blame-performance}).
Because so many benchmarks run slower with blame than in the worst case
 of @|sdeep| types, @|sShallow| Racket does not even attempt to track blame.

This result demands a two-step investigation.
The first step is to assess the usefulness of the original blame algorithm.
Both user studies and automated analyses@~cite{lksfd-popl-2020} can help.
Once the community knows more about what makes blame valuable,
 then the second step is the development of efficient algorithms that are
  tailored to developers' needs.

As for @|sdeep| types, the @|snatural| semantics is designed with blame in
 mind.
If blame is not needed, then an alternative semantics could enforce the same type guarantees
 using fewer wrappers.
@citet{fgsfs-oopsla-2018} show that removing some wrappers while preserving
 blame behavior leads to better performance.
An implementation could remove many more wrappers if it ignores blame.


@subsection{@|sTransient| Blame Filtering}

My implementation of blame for @|sShallow| Racket makes an effort to filter
 irrelevant boundaries as suggested by @citet{vss-popl-2017}.
Filtering, however, is expensive and fails on boundaries that use
 generative struct types (@section-ref{sec:transient:blame:types}).
The failure warrants further investigation.
But regardless of whether run-time filtering can cover more types,
 we need to measure its usefulness.
If filtering is unlikely to help programmers diagnose issues,
 then removing it can save a tremendous amount of bookkeeping.
Without the need to filter, the blame map does not need to store types or
 actions---only pointers and source locations.


@section{Improve Fully-Typed @|sTransient|}

Despite the large improvement relative to @|snatural|, the cost of
 @|sshallow| @|stransient| types is still high.
The fully-typed configurations of the benchmarks make this problem apparent (@figureref{fig:transient:ratio});
 in the worst case, @|stransient| is 30x slower than untyped even with type-directed optimizations.
@|sTransient| needs a way to reduce the cost of shape checks.

@citet{vsc-dls-2019} have demonstrated that a whole-program static analysis
 and a tracing JIT compiler can greatly improve performance in Reticulated Python.
What remains to be seen is how well a compositional analysis can do, and
 whether the JIT is essential.
Earlier versions of @|sShallow| Racket ran much slower due to redundant checks
 and the overhead of contract library combinators; perhaps further analysis
 and ahead-of-time optimization can close the gap between fully-typed @|sshallow|
 and @|sdeep|.
Starting points for such an analysis include occurrence typing@~cite{tf-icfp-2010},
 modular set-based analysis@~cite{mff-popl-2006}, and Henglein's tagging optimization@~cite{h-lfp-1992}.
My investigations in @chapter-ref{chap:transient} suggest two additional
 starting points, based on the observation that @|sShallow| Racket checks the
 result of almost every function call that occurs in typed code:
@itemlist[
@item{
  The only function calls that are not protected with a shape check have
   the form @codett{(f x ....)} where the identifier @codett{f} appears
   in a trusted environment.
  For example, @|sShallow| Racket trusts that calls to @codett{map} return
   proper lists.
  This approach has major limitations.
  Checking identifiers is brittle; an alias to @codett{map} defeats the optimization.
  Furthermore, the current approach cannot trust deeper properties of a type.
  A call to @codett{filter}, for example, guarantees the shape of the result
   @emph{and} the shape of every element in the list.
  There should be some way to encode this shape knowledge in a type environment,
   rather than a flat identifier environment.
}
@item{
  Some user-defined functions do not need @|stransient| result checks.
  If a @|stransient| module defines a function @codett{f = (λ (x) ....)}
   then there is no need for the current module to check its results
   because static typing guarantees a shape-correct output.
  Other functions that are defined indirectly, for example by reading
   a function from an untyped list (@codett{f = (car f*)}),
   cannot be trusted.
}]

The Pycket compiler adds a JIT to @|sDeep| Typed Racket and significantly
 reduces the overhead of type boundaries@~cite{bbhkpst-icfp-2015,bbst-oopsla-2017}.
Adapting this backend to @|sShallow| Racket may reduce costs immediately,
 without the need for an analysis.
In the context of a simpler type system,
@citet{rmhn-ecoop-2019} report that a tracing JIT eliminates the cost
 of @|stransient|-inspired checks in Grace.


@section[#:tag "sec:future:nonopt"]{Improving @|sDeep|--@|sTransient| Interaction}

The model in @chapter-ref{chap:both} is safe, but makes @|sdeep| types expensive.
Every boundary to @|sdeep| code gets protected with a @|swrap| check (@figureref{fig:both:base-interaction}).
For boundaries between @|sdeep| and @|suntyped| this is no surprise, because
 the @|suntyped| code is unconstrained.
For @|sshallow| code, though, static typing provides some checked claims;
 one would hope to get away with a less expensive check at the boundary.
After all, closed programs that use only @|sdeep| and @|sshallow| code
 need no checks in principle because every line of code is validated by the
 strong surface-language type checker.

One possible way to optimize is to weaken the boundary between @|sdeep| and
 @|sshallow|.
@|sDeep| can avoid wrapping an export if the value never interacts with @|suntyped|
 code going forward.
Likewise, @|sdeep| can trust an import if the value was never handled or influenced
 by @|suntyped| code.
@Figure-ref{fig:both:opt0} sketches the boundaries that could change via
 this strategy; the @|sdeep|--@|suntyped| and @|sshallow|--@|suntyped| boundaries
 are unaffected.
Note, however, that determining whether a value interacts with @|suntyped|
 code requires a careful analysis.
Developing a correct analysis that runs quickly is a research challenge in
 itself.

@figure*[
  "fig:both:opt0"
  @elem{With an escape analysis, the @|sdeep|--@|sshallow| boundaries could be weakened.}
  both:DS0]

@figure*[
  "fig:both:opt1"
  @elem{With an escape analysis and the ability to create wrappers in @|sshallow| code, all runtime type checks could be pushed to the boundaries with @|suntyped| code.}
  both:DS1]

A second possibility is to make the @|sdeep|--@|sshallow| boundary
 a @|snoop| by delaying wrappers until a @|sdeep| value reaches @|suntyped| code.
Ideally, this strategy can work with an escape analysis to avoid wrapping
 @|suntyped| values that never reach @|sdeep| code (@figureref{fig:both:opt1}).
The challenge here is to design an escape analysis and to add wrapper-making
 code to @|sshallow| without losing the expressiveness that @|stransient| gains
 by avoiding wrappers altogether.
For first-order interactions,
 @|sShallow| can be careful about the identifiers that it sends to @|suntyped| code.
Higher-order communication is the real source of difficulties.
For example,
 if @|sshallow| imports an @|suntyped| map function, then @|sshallow| must be
 prepared to wrap every function that it sends to map just in case such a function
 is @|sdeep|-typed.

If a language can create wrappers in @|sshallow| code, however, then the
 @exact{\fname} semantics (@chapter-ref{chap:design}) may be a better fit than @|sTransient|.
@|sShallow| types via @exact{\fname} do not require shape checks throughout
 typed code, and the 1-level wrappers can dynamically cooperate with @|sdeep|-wrapped
 values; that is, the interactions do not require a static analysis
 because the wrappers carry information.


@section{Evaluate Alternative Shape Designs}

The shape checks in @|sShallow| Racket enforce full type constructors
 (@sectionref{sec:transient:theory:types}).
Other designs are possible, though, and may lead to a better tradeoff
 between type guarantees and performance.

One direction is to strenghten the run-time checks to go beyond the outermost type
 constructor.
Some designs may benefit from two or three levels of constructor checks.
In the limit, a @|stransient| could enforce all first-order properties.

A second alternative is to weaken run-time checks for maximal performance.
The current shapes check too much, in the sense that the Typed Racket optimizer
 cannot use all the information.
For example, the @|sShallow| check @codett{procedure?} does not help any optimizations.
If performance is the only concern, then an implementation can let the dynamically-typed
 runtime system handle function applications.


@section{Other Challenges}

@itemlist[
@item{
  In a performance lattice, an inspection of the configurations with exactly
   one typed unit can reveal the lack of fast paths through the lattice.
  Namely, if any of these bottom-level configurations suffer high overhead
   then a one-by-one conversion path is going to suffer similar overhead
   at some step.
  Perhaps there are other properties that can be predicted without exploring
   a full lattice.
  @citet{grmhn-vmil-2019}, for example, suggest that @|stransient| slowdowns
   can be diagnosed by studying each typed unit individually.
}
@item{
 The performance evaluation method begins by toggling types at a
  certain granularity.
 The definition of granularity in @chapter-ref{chap:performance:definition} does not
  allow for imprecise types such as @tt{List(Dyn)} and @tt{Function([Dyn], Str)}.
 Adapting the definition to such types would improve our understanding of
  prior work that randomly generates imprecise types@~cite{kas-pldi-2019,vsc-dls-2019}.
}
@item{
  Design a semantics, @exact{\xsym}, that eagerly checks pairs like the @|nname| semantics
   and wraps/unwraps functions like @|fname|.
  Prove that @exact{\xsym} does not satisfy complete monitoring,
   but can satisfy blame soundness and completeness.
  There may be an undiscovered variant of complete monitoring that
   distinguishes this @exact{\xsym} semantics from the basic @|fname| semantics,
   which may omit checks on the elements of a pair.
}
@item{
  Rephrase complete monitoring in semantic terms, using types and observable
   behaviors instead of syntax.
}
@item{
  The error preorder (@${\sbehaviorle}) looks like the term precision
   relation (@${\sqle}) from the gradual typing literature@~cite{svcb-snapl-2015,nla-popl-2019}.
  To investigate whether there is a deeper connection,
   use the @|nname| and @|fname| semantics to design two compilers into a core
   language that satisfies graduality.
  @exact{\kafka} may be a good starting point@~cite{clzv-ecoop-2018}.
  Prove that the @|fname| compiler always gives less-precise expressions according
   to the term precision relation.
  Test whether core-language term precision can be used to indirectly prove the surface-language error preorder.
}
@;@item{
@;  @|sShallow| Racket makes no attempt to enforce parametricity for universal
@;   types.
@;  Is there a different way to enforce universal types that offers stronger
@;   reasoning principles, in the spirit of parametricity?
@;}
@;@item{
@;  Find an effective way to allow subtyping and catch boundary errors.
@;  One idea is to use static analysis to identify the different paths from
@;   boundaries to a use-site.
@;  A second is to record subtyping actions in the blame map, as a loss of precision.
@;  The extra metadata, however, will not prevent @codett{lazy-fact} from diverging.
@;}
@item{
  Implement @|stransient| blame with multiple parents per link entry.
  For operations such as @codett{hash-ref}, dynamically choose which parent
   to follow.
  A language of blame types may be necessary to guide choices.
  Measure the quality of errors and the performance cost that results from the
   extra bookkeeping.
}
@;@item{
@;Summarize the different @|snatural| semantics in the literature as a
@; natural transformation between two functors.
@;}
@item{
  Add erased types to the mix; determine what is needed for @|sdeep| types,
   @|sshallow| types, and optional types to interact.
  The prior work on @emph{like types}, which combines optional and concrete
   types, may be a useful guide@~cite{wzlov-popl-2010,rzv-ecoop-2015}.
}
]

