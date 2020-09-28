#lang greenman-thesis/include

@title[#:tag "chap:future"]{Future}

Transient on Pycket, how does the JIT compare to AOT in apples comparison.

Static analysis for Transient checks, shouts out to Henglein.

Transient blame, practical alternative to address the many challenges we found.

And maybe we need wrappers after all, pursue Amnesic or Forgetful.


@;@section[#:tag "sec:transient:future"]{Future Challenges}
@;
@;@futurework{
@;  Adapt Typed Racket's occurrence typing to support a completion pass that
@;   avoids dominated checks.
@;  Evaluate the performance improvement.
@;}
@;
@;@futurework{
@;  Design a language of blame types to replace the identifier-based logic.
@;}
@;
@;@futurework{
@;  Allow multiple parents per link entry
@;   and dynamically choose a parent for operations such as @codett{hash-ref}.
@;  How do the changes affect blame errors and performance?
@;}
@;
@;@futurework{
@;  The Typed Racket optimizer does not take advantage of all shapes.
@;  In this sense, the check for functions is an unnecessary cost---even
@;   @codett{procedure?} would be a waste, because the TR optimizer does
@;   not use it.
@;  Improve the optimizer where possible and remove other shape checks.
@;  How do the changes impact performance?
@;  Try writing a few programs;
@;   perhaps by converting Reticulated benchmarks to @|sShallow| Racket.
@;  Do the removed shape checks make programs more difficult to debug?
@;}
@;
@;
@;@subsection{Trust Types}
@;
@;For now, @|sShallow| Racket includes a whitelist of trustworthy base-library
@; functions.
@;Functions like @codett{map} are trusted to return shape-correct results.
@;
@;Checking identifiers, however, is brittle.
@;Furthermore, the current approach cannot trust deeper properties of a type.
@;A call to @codett{filter}, for example, guarantees the top shape of the result
@; (a list) and the top shape of every element in the list.
@;There should be some way to encode this shape knowledge in a type.
@;
@;
@;@subsection{Identify Trustworthy Typed Identifiers}
@;
@;@; aka why doesn't @|stransient| optimize more?
@;
@;Some user-defined functions do not need @|stransient| result checks.
@;If a @|stransient| module defines a function @codett{f = (λ (x) ....)}
@; then there is no need for the current module to check its results;
@; static typing guarantees a shape-correct output.
@;Other functions in the same module, though, cannot be trusted.
@;If @codett{f = (car f*)} then we can only trust the output if functions
@; in the list are good.
@;
@;
@;@subsection{Improve or Drop @|sTransient| Blame Filtering}
@;
@;@|sShallow| Racket makes an effort to filter irrelevant boundaries
@; as suggested by @citet{vss-popl-2017}.
@;Some boundaries cannot be filtered, however, because @|sShallow| Racket
@; cannot parse the complex type definitions (@section-ref{sec:transient:blame:types}).
@;This challenge motivates two research efforts.
@;
@;One goal is to improve type parsing and filtering to cover all types.
@;If possible, this seems like the way to go.
@;But it will be important to measure whether the solution requires additional
@; run-time overhead.
@;
@;A second goal is to evaluate the usefulness of filtering as-is.
@;If filtering is not useful and seems unlikely to help, then removing it can
@; save a tremendous amount of bookkeeping.
@;Without the need to filter, the blame map does not need to store types or
@; actions---only pointers and source locations.
@;If filtering is useful, it may be helpful to allow types with parse errors
@; because an action path explores only a fraction of a full type.
@;The needed portion may not touch the difficult parts.
@;
@;
@;@subsection{Design a new Blame Algorithm}
@;
@;The performance costs of the blame algorithm from @citet{vss-popl-2017} are high.
@;Is there a different algorithm with lower cost that still provides useful
@; information?
@;
