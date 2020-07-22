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
The high cost raises doubts about sound gradual typing as a whole;
 being able to run mixed-typed code is not enough, programs also need to
 run in reasonable time.
@Chapter-ref{chap:design} demonstrates alternatives in the design space.
 of sound gradual typing.
Typed Racket's @emph{natural} approach sits at an extreme point;
 it offers strong static guarantees that unfortunately demand run-time support.

Among the design alternative, the @emph{transient} approach is promising.
Transient offers a basic soundness guarantee,
 requires a low implementation effort,
 and easily supports any combination of typed and untyped code.
Furthermore, the data for Reticulated Python suggests that the overhead
 of Transient types never exceeds a 10x slowdown.

This chapter describes my implementation of Transient Racket,
 a new way to run Typed Racket programs.
Transient Racket uses the same static types as normal Typed Racket
 (henceforth: Guarded Racket) but enforces a weaker soundness guarantee
 using the transient method.

The implementation comes with innovations on two levels.
First, it stands on a generalized theory to support macro-level gradual
 typing and a richer language of static types.
Second, the code deals with tricky points and makes clever re-use
 of the Typed Racket library.
Additionally, we describe an experiment adding blame to Transient Racket
 as proscribed by @citet{vss-popl-2017}.
The theory needs significant work, however, because a faithful implementation
 makes several benchmarks run slower than their worst-case Guarded Racket
 configuration.
Transient with blame is therefore an important direction for future work.


@section{Why Transient}

@; what to do?
@; could improve TR, looking bleak
@; only other option is to pursue a weaker semantics
@; .... nothing else can support existing Dyn lang as intended

@; why not others?
@; ... mostly, no chaperones

