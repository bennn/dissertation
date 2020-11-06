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



@; @futurework{
@; Summarize the different @|snatural| semantics in the literature as a
@;  natural transformation between two functors.
@; }

@; Q about predictions
@; @exercise[2]{
@;   Are there any paths from bottom to top in the @bm{fsm} lattice that stay
@;    under 2x at every point?
@;   Can the configurations with exactly 1 typed module predict whether such
@;    paths exist?
@; }

@futurework{
 Adapt the notion of granularity to imprecise types such as @tt{List(Dyn)}
  and @tt{Function([Dyn], Str)}.
 Compare your new dynamic-type granularity to prior studies that randomly
  select imprecise types@~cite{kas-pldi-2019,vsc-dls-2019}.

@exercise[2]{
  Design a semantics, @exact{\xsym}, that eagerly checks pairs like @|nname|
   and wraps/unwraps functions like @|fname|.
  Prove that @exact{\xsym} does not satisfy complete monitoring,
   but can satisfy blame soundness and completeness.
}

@futurework{
  Formulate a variant of complete monitoring that distinguishes the @exact{\xsym}
   semantics of the previous exercise from the @|nname| semantics.
}

@futurework{
  Rephrase complete monitoring in terms of types and observable behaviors
   (instead of syntactic judgments).
  @; What are the advantages of your semantic theorem relative to the current
  @;  syntactic one?
  @; Any ideas to simplify / economize the syntactic theorem?
}

@futurework{
  The error preorder (@${\sbehavioreq}) looks a lot like the term precision
   relation (@${\sqle}) from the gradual typing literature@~cite{svcb-snapl-2015,nla-popl-2019}.
  To investigate whether there is a deeper connection,
   use the @|nname| and @|fname| semantics to design two compilers into a core
   language that satisfies graduality.
  @exact{\kafka} may be a good starting point@~cite{clzv-ecoop-2018}.
  Prove that the @|fname| compiler always gives less-precise expressions according
   to the term precision relation.
  Can core-language term precision be used to indirectly prove the surface-language error preorder?
}

@futurework{
  Is there a different way to enforce universal types that offers stronger
   reasoning principles, in the spirit of parametricity?
}

@futurework{
  Find an effective way to allow subtyping and catch boundary errors.
  One idea is to use static analysis to identify the different paths from
   boundaries to a use-site.
  A second is to record subtyping actions in the blame map, as a loss of precision.
  The extra metadata, however, will not prevent @codett{lazy-fact} from diverging.
}

@futurework{
  Adapt Typed Racket's occurrence typing to support a completion pass that
   avoids dominated checks.
  Evaluate the performance improvement.
}

@;@futurework{
@;  Design a language of blame types to replace the identifier-based logic.
@;}

@;@futurework{
@;  Allow multiple parents per link entry
@;   and dynamically choose a parent for operations such as @codett{hash-ref}.
@;  How do the changes affect blame errors and performance?
@;}

@section[#:tag "sec:transient:future"]{Future Challenges}

@subsection{Identify Trustworthy Typed Identifiers}

@; aka why doesn't @|stransient| optimize more?

Some user-defined functions do not need @|stransient| result checks.
If a @|stransient| module defines a function @codett{f = (λ (x) ....)}
 then there is no need for the current module to check its results
 because static typing guarantees a shape-correct output.
Other functions in the same module, though, cannot be trusted.
If @codett{f = (car f*)} then we can only trust the output if functions
 in the list are good.

@; @subsection{Trust Types}
@; 
@; For now, @|sShallow| Racket includes a whitelist of trustworthy base-library
@;  functions.
@; Functions like @codett{map} are trusted to return shape-correct results.
@; 
@; Checking identifiers, however, is brittle.
@; Furthermore, the current approach cannot trust deeper properties of a type.
@; A call to @codett{filter}, for example, guarantees the top shape of the result
@;  (a list) and the top shape of every element in the list.
@; There should be some way to encode this shape knowledge in a type.


@subsection{Improve or Drop @|sTransient| Blame Filtering}

@|sShallow| Racket makes an effort to filter irrelevant boundaries
 as suggested by @citet{vss-popl-2017}.
Some boundaries cannot be filtered, however, because @|sShallow| Racket
 cannot parse the complex type definitions (@section-ref{sec:transient:blame:types}).
Short of designing a new blame algorithm, this challenge motivates two research efforts.

One goal is to improve type parsing and filtering to cover all types.
If possible, this seems like the way to go.
But it will be important to measure whether the solution requires additional
 run-time overhead.

A second goal is to evaluate the usefulness of filtering as-is.
If filtering is not useful and seems unlikely to help, then removing it can
 save a tremendous amount of bookkeeping.
Without the need to filter, the blame map does not need to store types or
 actions---only pointers and source locations.
If filtering is useful, it may be helpful to allow types with parse errors
 because an action path explores only a fraction of a full type.
@; The needed portion may not touch the difficult parts.


@subsection{Evaluate Alternative Shape Designs}

The shape checks in @|sShallow| Racket enforce full type constructors
 (@sectionref{sec:transient:theory:types}).
Other designs are possible, though, and may lead to a better tradeoff
 between type guarantees and performance.

One direction is to strenghten the run-time checks to go beyond the outermost type
 constructor.
Some designs may benefit from two or three levels of constructor checks.
In the limit, a @|stransient| could enforce all first-order properties;
 if a programmer can switch between enforcement levels, then this option may
 be helpful for debugging.

A second alternative is to weaken run-time checks for maximal performance.
The current shapes check too much, in the sense that the Typed Racket optimizer
 does not use them all.
For example, the @|sShallow| check for function-arity does not help any optimizations.
Nor would a simple @codett{procedure?} check.
If performance is the only concern, then an implementation can let the dynamically-typed
 runtime system handle function applications.

