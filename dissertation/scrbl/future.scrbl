#lang greenman-thesis/include

@title[#:tag "chap:future"]{Future Work}

Now that we have a language that provides @|sdeep| types via the @|snatural|
 semantics and @|sshallow| types via @|stransient|, three lines
 of future work stand out: blame, @|stransient| performance, and
 alternative strategies for @|sshallow| types.


@section{@|sTransient| with Blame, @|sNatural| without Blame}

The biggest surprise that I encountered during this research is the
 huge cost of @|stransient| blame (@section-ref{sec:transient:blame-performance}).
Because so many benchmarks run slower with blame than in the worst case
 of @|sdeep| types, @|sShallow| Racket does not even attempt to track blame.
This needs to change, probably.
The first step is to assess the usefulness of the original blame algorithm.
Both user studies and automated analyses@~cite{lksfd-popl-2020} can help.
Once the community knows more about what makes blame valuable,
 then researchers can focus on efficient algorithms that are tailored 
 to the right service.

On the @|sdeep| side, the @|snatural| semantics is designed with blame in
 mind.
If blame is not needed, then a semantics could enforce the same type guarantees
 using fewer wrappers; even Racket's collapsible contracts fail to remove
 many wrappers because they insist on correct blame@~cite{g-popl-2015,fgsfs-oopsla-2018}.
Again, this research calls for an evaluation of blame
 and entails a design and implementation challenge.


@section{Fully-Typed @|sTransient|}

Despite the often-huge improvement relative to @|snatural|, the cost of
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


@section{Stronger @|sShallow| Types}

Boundaries between @|sdeep| and @|sshallow| are, unfortunately, no cheaper
 than boundaries between @|sdeep| and @|suntyped|.
A stronger implementation of @|sshallow| types, that uses wrappers instead
 of only shape checks, may be able to cooperate better with @|sdeep| and avoid
 the typed-to-typed cost.
The @exact{\fname} semantics is a good starting point, but one will need to
 think carefully about how to represent wrappers so they can communicate with
 @|sdeep| wrappers.



