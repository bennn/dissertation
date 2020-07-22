#lang greenman-thesis/include

@title[#:tag "chap:transient"]{Transient Racket}

@; OUTLINE 2020-07-21
@; transient racket (transient + untyped, that's all)
@; - why transient
@; - theory
@;   + macro vs micro
@;     no Dyn type, boundaries instead
@;   + more types, design choices about checks (list? vs pair?),
@;     affects soundness + optimizer
@;     - mu, union, forall, occurrence (noop), class/obj public private init-field
@;       ->* vs (t -> t) ?
@; - eng. adaptation, to racket lang
@;   + tr overview, transient overview, re-use type checker
@;   + changes
@;     > type->contract change
@;     > rewrite typed code with checks
@;     > weaken optimizer
@;     > provide more (macros can leave)
@;     > assemble #%module-begin
@;   + issues, surprises
@;     > optimizer assumed annotation, "dead" code doesn't have them
@;     > found 21 racket world bugs
@;     > can't trust typed ids because of require/typed and occurrence types
@;       (permissiveness vs cost)
@;     > trusted base ids not enough, really need trusted types (make-do-sequence)
@; - perf. properties
@;   + overhead plots
@;   + exact-points, lattices (what are the trends?)
@;   + why so slow, what can be done?
@;     - macros = problem = more typed code than untyped
@; - blame, work in progress
@;   + why blame
@;   + transient-blame ideas
@;     check+update, escape+update, store all v
@;   + trouble, basic implications
@;     big map, no gc, much bookkeeping
@;   + more challenges
@;     TR accessors, multi-parent, cannot trust base env, types at runtime
@;   + preliminary perf = fully-typed table,
@;     looks grim

@Chapter-ref{chap:performance} demonstrates that Typed Racket's gradual typing
 can add huge overhead to a mixed-typed program.
The high cost raises doubts about sound gradual typing;
 the ability to run mixed-typed code is not worth a severe performance hit.
@Chapter-ref{chap:design} shows, however, that Typed Racket's guarded approach
 is one of several type-sound methods in a larger design space.
Adapting a weaker approach may lead to lower, more-predictable costs.

Among the design alternative, the @emph{transient} approach is promising.
Transient offers a basic soundness guarantee,
 requires a low implementation effort,
 and easily supports any combination of typed and untyped code.
Furthermore, the data for Reticulated Python suggests that the overhead
 of Transient types never exceeds a 10x slowdown.

This chapter describes the implementation and performance of Transient Racket,
 a new way to run Typed Racket programs.
Transient Racket uses the same static types as standard, guarded Typed Racket
 but enforces a weaker soundness guarantee using the transient method.
The implementation comes with innovations on two levels.
First, it stands on a generalized theory to support macro-level gradual
 typing and a richer language of static types.
Second, the code reuses large parts of the standard Typed Racket compiler,
 including the type-driven optimizer@~cite{stff-padl-2012}.

The performance of Transient Racket is typically an improvement over
 Guarded Racket, but both semantics have distinct strengths.
In particular, Guarded Racket reports errors with precise blame information.
Transient Racket cannot give precise information, and the best-know
 imprecise algorithm@~cite{vss-popl-2017} adds tremendous runtime overhead.
@; Adding blame to Transient is thus an important direction for future work.
This, among other observations, suggests a need for guarded and transient
 within the same language.


@section{Why Transient}

@; what to do?
@; could improve TR, looking bleak
@; only other option is to pursue a weaker semantics
@; .... nothing else can support existing Dyn lang as intended

@; why not others?
@; ... mostly, no chaperones


@section{Theory}

@section{Implementation}

@section{Performance}

@section{Work-in-progress: Blame}


