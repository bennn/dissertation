#lang greenman-thesis/include

@title[#:tag "chap:transient"]{@|sShallow| Racket}

@; OUTLINE 2020-07-21
@; transient racket (transient + untyped, that's all)
@; - why transient
@; - theory
@;   + macro vs micro
@;     no Dyn type, boundaries instead
@;     why can't remove more checks
@;   + more types, design choices about checks (list? vs pair?),
@;     affects soundness + optimizer
@;     - mu, union, forall, occurrence (noop), class/obj public private init-field
@;       ->* vs (t -> t) ?
@; - blame, work in progress
@;   + why blame
@;   + transient-blame ideas
@;     check+update, escape+update, store all v
@;   + hint at impl. trouble, but focus on theory-only for now
@;     big map, no gc, much bookkeeping
@;   + more challenges
@;     TR accessors, multi-parent, cannot trust base env, types at runtime
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
@;   + blame perf, looks grim, fully-typed table,
@; - future work
@;   + occurrence typing, remove checks
@;   + blame new algorithm
@;   + other performance tuning, JIT level (see cifellows proposal)
@;   + 


The high cost of @|sdeep| types calls for an alternative semantics
 with @|sshallow| types.
Of the vetted alternatives (@chapter-ref{chap:design}), @|stransient| is the
 most promising.
@|sTransient| offers a basic soundness guarantee,
 requires a low implementation effort,
 and easily supports any combination of typed and untyped code.
Furthermore, the data for Reticulated Python suggests that the overhead
 of @|stransient| run-time checks never exceeds a 10x slowdown (@chapter-ref{chap:performance}).

This chapter presents a @|stransient| semantics for Typed Racket.
Adapting the theory to the Typed Racket language required a generalization to
 macro-level gradual typing and several insights to handle a richer
 language of static types (@sectionref{sec:transient:theory}).
In the course of this work, I also adapted the blame algorithm
 of @citet{vss-popl-2017} and identified several challenges (@sectionref{sec:transient:blame});
 first and foremost, the basic algorithm is prohibitively slow.
The final implementation does not include blame; that said, the implementation
 takes care to reuse large parts of Typed Racket, including the static
 type checker and most of the type-driven optimizer (@sectionref{sec:transient:implementation}).

Henceforth, @|sDeep| Racket refers to the original, @|snatural| implementation
 and @|sShallow| Racket refers to my @|stransient| implementation.
Typed Racket refers to the common, static parts; namely, the surface language
 and type system.

The performance of @|sShallow| Racket is typically an improvement over
 @|sDeep| Racket, but both semantics have distinct strengths (@sectionref{sec:transient:performance}).
@|sTransient| always adds overhead relative to untyped Racket, but is the
 safer bet for mixed-typed programs.
@|sNatural| has better performance in programs with large chunks of typed
 code, and surpasses untyped Racket in many cases.
Whether @|sShallow| Racket can ever run faster than untyped code is an open
 question; there are several avenues worth exploring (@sectionref{sec:transient:future}).


@section[#:tag "sec:transient:theory"]{Theory}
@; [ ] macro/micro ... what to say???
@;     - can't remove checks without analysis, but should be easier now
@;     - fewer cast locations (arguably, we missed <:)
@;     - "macro" anticipated by chap:design but ... that one is a bit far from impl.
@;     - 
@; [ ] all types, sec. for each interesting ... ->* mu U 
@; [ ] occurrence what to do = nothing
@; [ ] summary table ... used-for-optz, O(1), O(t), O(n) ....
@;     maybe work with numbers ... say total num checks instead of tabulating all

@citet{vss-popl-2017} present a first @|stransient| semantics.
This semantics helped me understand the behavior of Reticulated Python
 and the key ideas behind @|stransient|.
Three characteristics of the semantics, however, make it unsuitable for
 a @|stransient| implementation in Racket:
 it includes a dynamic type,
 it does not include a subtyping relation,
 and its type checker is intertwined with the @emph{completion} pass
 that rewrites typed code.
This section outlines the design of a suitable model and its properties.
Scaling the new model up to the full Typed Racket language raised questions
 about how to enforce intricate types.
@Section-ref{sec:transient:theory:types} explains my design and potential
 alternatives.


@subsection[#:tag "sec:transient:theory:dyn"]{From Micro to Macro}

Reticulated Python provides a dynamic type, and therefore the original



@subsection[#:tag "sec:transient:theory:subt"]{Pitfalls of Subtyping}

@subsection[#:tag "sec:transient:theory:completion"]{Correct Completion}

Original Retic paper states a translation.
To arrive at a similar translation in our completely-different context,
 need criteria.
Phrase as completion.
Ours is simple, can ask about minimal.
(Draw from ICFP)


@subsection[#:tag "sec:transient:theory:types"]{Run-Time Behaviors for Static Types}


@section[#:tag "sec:transient:blame"]{Work-in-progress: Blame}
@; chap:design says what to do

@section[#:tag "sec:transient:implementation"]{Implementation}

@section[#:tag "sec:transient:performance"]{Performance}
@; [ ] get NSA data
@; [ ] overhead plots ... incrementally
@; [ ] exact plots, for trends
@; [ ] blame perf table

@section[#:tag "sec:transient:future"]{Future Challenges}
@; vsc-dls-2019 has Retic/Pycket faster than untyped, 0.95x best-case


