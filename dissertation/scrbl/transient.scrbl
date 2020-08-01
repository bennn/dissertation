#lang greenman-thesis/include
@(require
   (only-in greenman-thesis/oopsla-2019/pict
     transient:divide
     transient:subtype))

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


@section[#:tag "sec:transient:theory"]{Theory++}
@; [X] macro/micro ... what to say???
@;     - can't remove checks without analysis, but should be easier now
@;     - fewer cast locations (arguably, we missed <: --- no, its not a boundary and it cannot fail!)
@;     - "macro" anticipated by chap:design but ... that one is a bit far from impl.
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
 about how to enforce types.
@Section-ref{sec:transient:theory:types} explains my design and a few
 alternatives.


@subsection[#:tag "sec:transient:theory:types"]{Run-Time Behaviors for Static Types}

The main design choices for @|sShallow| Racket concern the run-time checks
 that enforce types.
In terms of the model, there is a rich language @${\stype} of static types
 and the problem is to define a type-shape interpretation @${\tagof{\stype}} for
 each.
A shape must be decidable; for example,
 @${\tagof{\tfun{\tint}{\tint}} = \tfun{\tint}{\tint}} is unacceptable
 without a predicate that can decide whether an untyped function always
 returns an integer when applied to an integer.
Beyond decidability, type-shapes should be fast to test and imply useful
 properties.
Shape soundness should help a programmer debug a faulty program
 and should enable shape-directed optimizations.

The original @|stransient| model suggests that type-shapes must be decidable
 in constant time@~cite{vss-popl-2017}.
Namely, the only type constructors are for reference cells and functions,
 both of which are easily recognized in a dynamically-typed language.
Following this restriction would severely limit @|stransient| type systems.
Indeed, Reticulated Python goes beyond the constant-time suggestion with
 object types.
The type-shape for an object with @${N} fields/methods checks for the presence
 of each member.
Thus, the cost is linear in the size of an object type.

@|sShallow| Racket includes other non-constant shapes in addition to
 classes and objects.
In general, the goal is to enforce full type constructors.
The type-shape for a function checks arity; for example,
 the types @${(\tfun{\tint}{\tnat})} and @${(\tfun{\tint\,\tint}{\tnat})}
 have different shapes.
The shape for a sized vector checks length.
And the shape for a list checks for a null-terminated sequence of pairs.
Not all types correspond to value constructors, though.
These type @emph{connectives}@~cite{cl-icfp-2017,clps-popl-2019} call for
 a recursive interpretation:
 for example, @${\tagof{\stype_0 \cup \stype_1} = \tagof{\stype_0} \cup \tagof{\stype_1}}
 and @${\tagof{\fforall{\alpha_0}{\stype_0}} = \tagof{\stype_0}}.
Type variables have trivial shapes, @${\tagof{\alpha_0} = \top}.
@Section-ref{sec:transient:types} goes into more detail about the implementation.

@futurework{
  Is there a different way to enforce universal types that offers stronger
   reasoning principles, in the spirit of parametricity?
}


@subsection[#:tag "sec:transient:theory:dyn"]{From Micro to Macro}

Reticulated Python provides a dynamic type in the micro gradual typing
 tradition.
Consequently, every type-checking rule must accomodate the dynamic type
 in addition to the expected type.

Typed Racket does not have a dynamic type; instead it adds run-time tools
 so that a non-dynamic type system can make assumptions about untyped input.
Using this macro approach, only a handful of typing rules need to deal
 with dynamically-typed values.

The differences between micro and macro typing rules have implications
 for @|stransient| run-time checks.
In the original semantics, the evaluation of any expression could bring
 a dynamically-typed value into a typed context.
In a macro semantics, only boundaries and elimination forms can introduce
 an untyped value.
@Figure-ref{fig:transient:app-compare} illustrates the difference by contrasting
 the @|stransient| checks needed for a function application.
On the top, the micro approach requires three checks: two checks in case
 the function and argument are dynamically-typed, and one to validate the
 shape of the result.
On the bottom, macro without blame requires only the result check.

@figure*[
  "fig:transient:app-compare"
  @elem{
   @|sTransient| completion rules for an application under micro (top) and
   macro (bottom) gradual typing.
   Both rules insert run-time shape checks.
   The micro rule depends on a type coercion (@${\scoerce})
   metafunction@~cite{vss-popl-2017}.
  }
  @exact|{
    \begin{mathpar}
      \inferrule*[lab=Micro]{
        \stypeenv_0 \sWT \sexpr_0 : \stype_0 \compilesto \sexpr_0'
        \\
        \stypeenv_0 \sWT \sexpr_1 : \stype_1 \compilesto \sexpr_1'
        \\\\
        \stype_0~\scoerce~\tfun{\stype_2}{\stype_3}
      }{
        \stypeenv_0 \sWT \sexpr_0~\sexpr_1 : \stype_3
        \compilesto
        \echeckone{\stype_3}{((\echeckone{(\tfun{\stype_2}{\stype_3})}{\sexpr_0'})~(\echeckone{\stype_2}{\sexpr_1'}))}
      }

      \inferrule*[lab=Macro]{
        \stypeenv_0 \sWT \sexpr_0 : \tfun{\stype_2}{\stype_3} \compilesto \sexpr_0'
        \\
        \stypeenv_0 \sWT \sexpr_1 : \stype_2 \compilesto \sexpr_1'
      }{
        \stypeenv_0 \sWT \sexpr_0~\sexpr_1 : \stype_3
        \compilesto
        \echeckone{\stype_3}{(\sexpr_0'~\sexpr_1')}
      }
    \end{mathpar}
  }|
]

Adding blame to macro adds the need for an additional blame-map operation
 in @figure-ref{fig:transient:app-compare}, but no additional checks.
The blame map potentially needs an update because the argument flows
 in to the function.
There is no need for a check because the argument has a non-dynamic type.

Other rules can be simplified in a similar fashion.
The benefits are two-fold:
 macro programs have fewer run-time checks to slow them down,
 and programmers have fewer places to search if a program manifests a
 boundary error.


@subsection[#:tag "sec:transient:theory:subt"]{Adding Subtyping}

A type system for untyped code must either include a subtyping
 judgment or force programmers to rewrite their data definitions.
Rewriting takes time and invites mistakes, therefore a type system
 that supports migratory typing (@chapter-ref{chap:why}) needs a subtyping
 judgment.

The dynamic type is not enough because it cannot articulate designs.
For example, the untyped @codett{divide} function in @figure-ref{fig:transient:divide}
 either divides two numbers or returns the symbol @tt{'undef} if the divisor
 is zero.
Typed Racket lets a programmer express this ad-hoc union of two base types.
By contrast, the dynamic type can summarize the result but provides no information
 to callers.

@figure*[
  "fig:transient:divide"
  @elem{Untyped division function with exactly two kinds of output.}
  transient:divide]

Adapting @|stransient| to include subtyping was therefore an essential task
 for @|sShallow| Racket.
The addition was straightforward, but revealed a surprising distinction
 between declaration-site types and use-site types; @|stransient| with
 subtyping may miss certain type mistakes!
@Figure-ref{fig:transient:subtype} illustrates the pitfall of @|stransient|
 subtyping with a lazy factorial function.
This typed function asks for a thunk that computes a non-negative number
 and returns a thunk that computes a factorial.
Because of the type declaration on @codett{lazy-n}, it looks like @|stransient|
 should check that the call @codett{(lazy-n)} returns a non-negative number.
The actual behavior, however, depends on the type of the call expression.
If the language replaces the placeholder @codett{???} with the valid type
 @codett{Integer}, then @|stransient| checks for an integer and the untyped
 code at the bottom of the figure enters an infinite loop.

@figure*[
  "fig:transient:subtype"
  @elem{Lazy factorial function, may diverge under @|stransient|.}
  transient:subtype]

In summary, the flexibility of subtyping limits the ability of @|stransient|
 checks to find mismatches due to type boundaries.
Checks are based on local uses, while boundaries are claims with a broad scope.

@futurework{
  Find an effective way to offer subtyping and catch boundary errors.
  One idea is to use static analysis to identify the different paths from
   boundaries to a use-site.
  A second is to record subtyping actions in the blame map, as a loss of precision.
  The extra metadata, however, will not prevent @codett{lazy-fact} from diverging.
}


@subsection[#:tag "sec:transient:theory:completion"]{From Elaboration to Completion}

@citet{vss-popl-2017} intertwine typing and @|stransient| checks in a
 type-elaboration judgment.
The combination is a good fit for an implementation because check-insertion
 depends on static types, and one pass over the program is more efficient than
 two.
For the theory, however, it is better to keep surface typing separate
 from a @emph{completion}@~cite{h-scp-1994}
 pass that inserts @|stransient| checks 

In the model of @|sShallow| Racket, completion is a judgment (@${\compilesto})
 that transforms a well-typed surface term to a term with @|stransient| checks.
The goal is to insert enough checks to create a target-language term with
 a similar type.

@exact|{
\theoremsketch{completion correctness}{
  If\/ ${\sWT \sexpr_0 : \stype_0}$
  then\/ ${\sWT \sexpr_0 : \stype_0 \compilesto \sexpr_1}$
  and\/ ${\sWTtag \sexpr_1 : \tagof{\stype_0}}$.
}
}|

The first benefit of this theorem is that it rules out nonsensical completions.
By contrast, a type elaboration that converts all surface terms to the
 integer @${42} satisfies every theorem used to validate the original
 @|stransient|@~cite{vss-popl-2017}.

Second, the clear requirement makes it easier to adapt the idea of @|stransient|
 to a new language.
If the language has its own surface-level typing and type-to-shape metafunction
 (@${\tagof{\cdot}}), then completion correctness theorem guides the next steps.

Third, the specification motivates refined completions and target-level typings.
The challenge is to use as few checks as possible to build the target term.
For example, suppose the variable @codett{xy} points to a pair of numbers
 and consider the expression @codett{(+ (car xy) (car xy))}.
The completion for @|sShallow| Racket produces the following term:

@code-nested{(+ (check Num (car xy)) (check Num (car xy)))}

@|noindent|Racket guarantees left-to-right evaluation, however, so the second check
 can never fail.
An improved completion would eliminate this, and other, flow-dominated checks.

@futurework{
  Adapt Typed Racket's occurrence typing to support a completion pass that
   avoids dominated checks.
  Evaluate the performance improvement.
}


@section[#:tag "sec:transient:blame"]{Work-in-progress: Blame}
@; chap:design says what to do

@section[#:tag "sec:transient:implementation"]{Implementation}

@subsection[#:tag "sec:transient:types"]{Example Types and Shapes}

Here are some example types, their shape, their approximate cost,
 and a few brief comments.
These are not exactly the shapes in the implementation.
Costs are either constant, depend on the size of the type,
 or depend on the size of incoming values.
The shape for lists is the only one that recursively checks a value;
 that being said, the Racket predicate @codett{list?} caches its result.

@itemlist[
@item{
  @example-type-shape[
    #:type "(Listof Real)"
    #:shape "list?"
    #:cost "O(v)"]

  The type represents an any-length list of real numbers.
  The shape accepts only proper lists; @codett{(cons 1 (cons 2 3))} is not allowed.
}
@item{
  @example-type-shape[
    #:type "(List Real Real)"
    #:shape "(and/c list? (lambda (v) (= 2 (length l))))"
    #:cost "O(v)"]

  Represents a list with exactly two numbers.
  The shape checks length to enable optimization; @codett{(list-ref v 1)}
   becomes @codett{(unsafe-list-ref v 1)}.
}
@item{
  @example-type-shape[
    #:type "(Vectorof Real)"
    #:shape "vector?"
    #:cost "O(1)"]

  Any-length vector of numbers.
}
@item{
  @example-type-shape[
    #:type "(Vector Real)"
    #:shape "(and/c vector? (lambda (v) (= 1 (vector-length v))))"
    #:cost "O(1)"]

  Vector with one real number.
}
@item{
  @example-type-shape[
    #:type "(All (A) (-> Real A))"
    #:shape "(and/c procedure? (lambda (v) (procedure-arity-includes? v 1)))"
    #:cost "O(1)"]

  Type abstraction
}
@item{
  @example-type-shape[
    #:type "(U Real String)"
    #:shape "(or/c real? string?)"
    #:cost "O(\\stype)"]

  Untagged union.
}
@item{
  @example-type-shape[
    #:type "(-> Real String)"
    #:shape "(and/c procedure? (lambda (v) (procedure-arity-includes? v 1)))"
    #:cost "O(\\stype)"]

  Function with one mandatory argument.
}
]


@futurework{
  Improve the Typed Racket optimizer to take advantage of more shapes,
   and remove any checks that the optimizer cannot use.
  How do the changes impact performance?
  Do the removed shape checks make programs more difficult to debug?
}


@section[#:tag "sec:transient:performance"]{Performance}
@; [ ] get NSA data
@; [ ] overhead plots ... incrementally
@; [ ] exact plots, for trends
@; [ ] blame perf table

@section[#:tag "sec:transient:future"]{Future Challenges}
@; vsc-dls-2019 has Retic/Pycket faster than untyped, 0.95x best-case


