#lang greenman-thesis/include
@(require
   (only-in greenman-thesis/shallow/main
     SHALLOW-CURRENT-BENCHMARK*
     ratios-row-name
     ratios-row-deep
     ratios-row-shallow
     get-ratios-table
     render-ratios-table)
   (only-in greenman-thesis/jfp-2019/main
     MAX-OVERHEAD
     default-rkt-version
     render-relative-exact-plot
     render-relative-overhead-plot)
   (only-in greenman-thesis/oopsla-2019/pict
     transient:divide
     transient:all-type
     transient:occurrence-type
     transient:subtype))

@title[#:tag "chap:transient"]{@|sShallow| Racket}

@; TODO OUTLINE 2020-07-21
@; transient racket (transient + untyped, that's all)
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
 and @${\tagof{\fforall{\alpha_0}{\stype_0}} = \tagof{\stype_0}}
 provided @${\tagof{\stype_0}} does not depend on the bound variable.
Type variables have trivial shapes in other contexts, @${\tagof{\alpha_0} = \top}.
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

@;   + why blame
@;   + transient-blame ideas
@;     check+update, escape+update, store all v
@;   + hint at impl. trouble, but focus on theory-only for now
@;     big map, no gc, much bookkeeping
@;   + more challenges
@;     TR accessors, multi-parent, cannot trust base env, types at runtime

Blame is an important part of a migratory typing system because it strengthens
 the weakest aspect of migratory types.
Static types guarantee that certain errors cannot occur.
Migratory types cannot offer the same promise, errors may occur, and that
 is the biggest weakness of migratory types.
With blame, however, type-mismatch errors are preferable over others because
 they come with an action plan for debugging.
A programmer can follow the blame information to decide what code to edit
 toward a working program.

@|sShallow| Racket does not come with blame, however, despite the fact that
 the original presentation of @|stransient| spells out one blame technique@~cite{vss-popl-2017}.
There are two reasons.
First, scaling the original blame algorithm to Typed Racket presents
 significant challenges.
Second, @|stransient| blame has a tremendous performance cost.
This section explains the theoretical / scaling challenges.
@Section-ref{sec:transient:blame-performance} addresses performance.

@subsection{@|stransient| Blame: the Basics}

The idea of @|stransient| blame is to keep a shadow heap on the side
 with blame information.
There are no wrappers, so the map solves a major problem.
Every action in a program updates this map.
There are two kinds of map entry.
One records a boundary, that is, a cast.
The second records a dependency.
When an error occurs, the idea is to follow dependencies up through the
 map and report all boundaries.

As the analysis of @chapter-ref{chap:design} shows, the set of boundaries
 reported by @|stransient| is neither sound nor complete.
It misses all dependencies in untyped code.
And it conflates different paths taken by the same value.
Nevertheless, we expect the heuristic blame should be useful.

@citet{vss-popl-2017} add a final step to refine the balem results.
They filter blame results using the actual value and the dependency-path taken
 to reach it.
This needs an example.


Now we have the ingredients:
@itemlist[
@item{
  Record casts in a map; these are roots.
}
@item{
  Update the map whenever an action occurs
   with a parent pointer and a path element.
}
@item{
  Filter results using the action list and the actual value.
}
]

Next, the challenges.


@subsection{Richer Language for Paths}

Lets make a table, path and interpretation

Multi dom / cod
Case dom
Ditto for methods
No-op for wrappers
List elem vs rest, hash key val
Object field name
Heterogeneous types
Struct index

@subsection{Multi-Parent Paths}

append, maybe a non-issue because its a new list,
indeed thats choice taken by transient

vector copy,
 still the old vector, no avoiding issue

hash-ref,
 two possibilities for result


@subsection{Complex Flows, filter / map}

worse than multi-parent, focus on example

poly inst problem?


@subsection{Fragile, need Blame Types}

renaming car affects path results, from recognized accessor to unknown function
 application

cannot rely on syntax, need some kind of analysis to deal with aliasing

obvious solution is to propagate types, add alongside current type info,
 how to encode is a challenge, keep in mind the requirements above


@subsection{Cannot Trust Base Env}

? goes without saying?


@subsection{Types at Runtime}

Filtering with types has the following signature.

Need to interpret these types at runtime,
 get contracts and follow paths.
Can make whole new library to interpret syntax.
Or, try to revive types.
Either way challenging with separate compilation and aliases.

How to deal with generative struct types?


@section[#:tag "sec:transient:implementation"]{Implementation}

Overview, figure of compiler pipeline, re-use.

Key aspects below:
 creating shapes,
 inserting shapes,
 optimizing accordingly.
Plus two miscellaneous sections.


@subsection[#:tag "sec:transient:types"]{From Types to Shapes}

@|sShallow| Racket compiles static types to @emph{type shape} checks.
Each check enforces first-order properties of a type constructor.
In general, a successful check means that all well-typed operations
 should succeed at run-time.

For example, the type @codett{(Pairof String String)} uses the @codett{Pairof}
 type constructor; its shape check, @codett{pair?}, accepts any kind of
 pair.
A successful check @codett{(pair? v)} means that the operations
 @codett{(car v)} and @codett{(cdr v)} are well-defined, and nothing more.
Because these two operations are the only elimination forms for the
 @codett{Pairof} constructor, the shape meets its goal.

Types that support many first-order properties have more complex shape checks.
For example, an object comes with field and method names.
The shape check must ensure that type-correct calls to @codett{get-field}
 and @codett{send} succeed at run-time.

Below are several more example types, chosen to illustrate the variety
 and challenges of extending @|stransient|.
Each type comes with a high-level shape that illustrates the implementation
 and a brief discussion.

@itemlist[
@item{
  @example-type-shape[
    #:type "(Listof Real)"
    #:shape "list?"
    #:cost "O(v)"
  ]

  The type represents lists of real numbers.
  The shape accepts any proper list;
   an improper list like @codett{(cons 1 (cons 2 3))} is not allowed.
  The run-time cost depends on the size of input values;
   that being said, pairs are immutable and the predicate @codett{list?} caches
   its results.
  The optimizer uses the shape to rewrite getters.
}
@item{
  @example-type-shape[
    #:type "(List Real Real)"
    #:shape "(and/c list? (λ(v) (= 2 (length l))))"
    #:cost "O(v)"
  ]

  Represents a list with exactly two numbers.
  The shape checks lengths.
  Doing so lets the optimizer change @codett{(list-ref v 1)}
   to @codett{(unsafe-list-ref v 1)}.
}
@item{
  @example-type-shape[
    #:type "(Rec Chain (U Null (Pairof Chain Real)))"
    #:shape "(or/c null? pair?)"
    #:cost "O(1)"]

  The recursive type is isomorphic to @codett{(Listof Real)}, but enforced
   with a more primitive check.
  In general, built-in lists have the only shape whose cost depends on input
   values.
}
@item{
  @example-type-shape[
    #:type "(Vector Real)"
    #:shape "(and/c vector? (λ(v) (= 1 (vector-length v))))"
    #:cost "O(1)"]

  Represents a vector that contains exactly one number.
  The shape checks length; the optimizer uses this fact.
}
@item{
  @example-type-shape[
    #:type "(Mutable-Vectorof Real)"
    #:shape "(and/c vector? (not/c immutable?))"
    #:cost "O(1)"]

  Represents a mutable vector with any number of elements.
  Vectors can also be immutable; the parent type @codett{Vector} covers both.
  The optimizer does not look at mutability, but the type checker does
   to raise static type errors.
}
@item{
  @example-type-shape[
    #:type "(Weak-HashTable Symbol (-> Void))"
    #:shape "(and/c hash? hash-weak?)"
    #:cost "O(1)"]

  Represents a mutable hash table whose keys do not inhibit the garbage collector.
}
@item{
  @example-type-shape[
    #:type "(U Real String)"
    #:shape "(or/c real? string?)"
    #:cost "O(1)"]

  Untagged union.
  The shape accepts either a real number or a string;
   these predicates are elimination forms for the union because of occurrence
   typing@~cite{tf-icfp-2010,t-thesis-2010}.
  Wider unions, with @${N} types inside, have shapes with @${N} components.
}
@item{
  @example-type-shape[
    #:type "(Syntaxof String)"
    #:shape "syntax?"
    #:cost "O(1)"]

  Represents a syntax object that contains a string.
  The shape checks for a syntax object.
}
@item{
  @example-type-shape[
    #:type "(Syntaxof Symbol)"
    #:shape "identifier?"
    #:cost "O(1)"]

  Represents a syntax object that contains a symbol; that is, an identifier.
  The shape check goes deeper and confirms the symbol.
}
@item{
  @example-type-shape[
    #:type "Integer"
    #:shape "exact-integer?"
    #:cost "O(1)"]

  Represents a mathematical integer.
  The shape checks for exactness; an inexact integer such as @codett{4.0} is
   not allowed.

  Other numeric types require larger checks for additional properties,
   for example @codett{Negative-Integer} adds looks for an exact integer that
   is less than zero.

  To the type system, numeric types are wide unions.
  Shape enforcement flattens unions wherever possible.
  Contract enforcement does likewise.
}
@item{
  @example-type-shape[
    #:type "(Refine [n : Integer] (= n 42))"
    #:shape "(and/c exact-integer? (=/c 42))"
    #:cost "O(1)"]

  Represents an integer that is equal to @codett{42}.

  Refinement types attach a predicate to a static type.
  Predicates are limited to a linear arithmetic.
  The shape check uses the whole predicate.
}
@item{
  @example-type-shape[
    #:type "(Class (field [a Natural]) (get-a (-> Natural)))"
    #:shape "(contract-first-order (class/c (field a) get-a))"
    #:cost "O(1)"]

  Represents a class with one field and one method.
  The shape depends on the @codett{racket/contract} library to check simple
   properties of class shape.
  Object types have similar checks, using @codett{object/c} instead.
}
@item{
  @example-type-shape[
    #:type "(-> Real String)"
    #:shape "(arity-includes/c 1)"
    #:cost "O(1)"]

  Represents a function with one mandatory argument.
  The shape checks arity.
}
@item{
  @example-type-shape[
    #:type "(-> Real * Real)"
    #:shape "(arity-includes/c 0)"
    #:cost "O(1)"]

  Represents a function that accepts any number of positional arguments.
  The shape looks for functions that can accept zero arguments,
   but does not check whether they accept more.
}
@item{
  @example-type-shape[
    #:type "(case-> (-> Symbol Symbol) (-> Symbol Real Symbol))"
    #:shape "(and/c (arity-includes/c 1) (arity-includes/c 2))"
    #:cost "O(1)"]

  Represents an overloaded function.
  The shape checks both arities.

  Functions can also have optional, keyword, and optional keyword arguments.
  The shapes for such functions check that the keywords are accepted.
}
@item{
  @example-type-shape[
    #:type "(All (A) (Box A))"
    #:shape "box?"
    #:cost "O(1)"]

  Represents a polymorphic mutable cell.
  The shape checks for a cell.
  If typed code wants to extract a value from the cell, it must instatiate
   the polymorphic type.
  The instantiation provides a shape to check the contents.
}
@item{
  @example-type-shape[
    #:type "(All (A) A)"
    #:shape "none/c"
    #:cost "O(1)"]

  Represents a value that can be instantiated to any type.
  The shape rejects all values.

  This type could be allowed with the trivial shape @codett{any/c} in an
   implementation that checks the result of type instantiation,
   along the lines of @citet{nja-popl-2020}.
  @|sShallow| Racket does nothing at instantiation, and therefore rejects
   the type to prevent unsoundness (@figure-ref{fig:transient:all-type}).
}
]

@figure*[
  "fig:transient:all-type"
  @elem{If the shape of a universal type depends on the bound variable, then
   @|stransient| must reject the program or treat type instantiation as an elimination form.}
  transient:all-type
]

@futurework{
  The Typed Racket optimizer does not take advantage of all shapes.
  In this sense, the check for functions is an unnecessary cost---even
   @codett{procedure?} would be a waste, because the TR optimizer does
   not use it.
  Improve the optimizer where possible and remove other shape checks.
  How do the changes impact performance?
  Try writing a few programs;
   perhaps by converting Reticulated benchmarks to @|sShallow| Racket.
  Do the removed shape checks make programs more difficult to debug?
}


@subsection{Defender}

@subsection{Optimizer}

@subsection{Surprises}

New abilities.
See RFC for motivations.

Some programs are still disallowed.
@Figure-ref{fxg:transient:occurrence-type} is one example.
It uses @codett{require/typed} to import an untyped function with a nonsensical
 occurrence type.
The typechecker trusts that all @codett{require/typed} annotations are valid
 claims, and so the rest of the program type checks.
The typechecker assumes that a run-time check will catch any faulty claims.
In this program, however, the occurrence type adds a side effect claim
 that is not caught by any check.
For now, @|sShallow| Racket rejects any program that uses an occurrence type
 as a claim.

@figure*[
  "fig:transient:occurrence-type"
  @elem{
   @|stransient| must reject the program or treat type instantiation as an elimination form.}
  transient:occurrence-type
]

@; TODO good occurrence type? listof predicate?


@subsection{Additional Fixes and Enhancements}

@figure*[
  "fig:transient:pulls"
  @elem{Pull requests inspired by work on @|sShallow| Racket.}
  @exact{
  \begin{tabular}{rll}
       & kind   & pull request
  \\\hline
     1 & bugfix & @github-pull["racket" "htdp" "98"]
  \\
     2 & bugfix & @github-pull["racket" "pict" "60"]
  \\
     3 & bugfix & @github-pull["racket" "racket" "3182"]
  \\
     4 & bugfix & @github-pull["racket" "typed-racket" "926"]
  \\
     5 & bugfix & @github-pull["racket" "typed-racket" "919"]
  \\
     6 & bugfix & @github-pull["racket" "typed-racket" "916"]
  \\
     7 & bugfix & @github-pull["racket" "typed-racket" "914"]
  \\
     8 & bugfix & @github-pull["racket" "typed-racket" "912"]
  \\
     9 & bugfix & @github-pull["racket" "typed-racket" "923"]
  \\
    10 & bugfix & @github-pull["racket" "typed-racket" "921"]
  \\
    11 & bugfix & @github-pull["racket" "typed-racket" "918"]
  \\
    12 & bugfix & @github-pull["racket" "typed-racket" "913"]
  \\
    13 & bugfix & @github-pull["racket" "typed-racket" "884"]
  \\
    14 & bugfix & @github-pull["racket" "typed-racket" "855"]
  \\
    15 & bugfix & @github-pull["racket" "typed-racket" "612"]
  \\
    16 & bugfix & @github-pull["racket" "typed-racket" "600"]
  \\
    17 & enhancement & @github-pull["racket" "typed-racket" "927"]
  \\
    18 & enhancement & @github-pull["racket" "typed-racket" "925"]
  \\
    19 & enhancement & @github-pull["racket" "typed-racket" "911"]
  \\
    20 & enhancement & @github-pull["racket" "typed-racket" "907"]
  \\
    21 & enhancement & @github-pull["racket" "typed-racket" "917"]
  \end{tabular}
}]

The development of @|sShallow| Racket led to several improvements in
 other Racket libraries.
Debugging sessions occasionally revealed bugs in existing code,
 and the integration of @|sShallow| and @|sDeep| Racket suggested
 enhancements for the latter.
@Figure-ref{fig:transient:pulls} tabulates these fixes and enhancements;
 the third column contains links with more details.

Most improvements came about through @|stransient| run-time checks.
During compilation, @|stransient| relies on types embedded in an intermediate
 representation to generate checks.
Missing types and imprecise types caused problems at this completion step;
 on occasion, the problems were due to Typed Racket bugs.
At run-time, @|stransient| helped identify inaccurate types with its
 checks.
The HTDP fix offers a simple example (@github-pull["racket" "htdp" "98"]).
Here, a library-provided function promised to return a unit value and actually
 returned a boolean.
@|sTransient| caught the unsoundness.

The fix to Racket bears special mention (@github-pull["racket" "racket" "3182"]).
Initially, some @|sshallow|-typed programs failed with a strange error:

@nested[#:style 'inset @codett{Expected a real number, got #<unsafe-undefined>}]

@|noindent|These programs were fully-typed, but somehow a run-time value
 contradicted the type checker without causing trouble in the @|sDeep| semantics.
Worse, this particular value (@codett{#<unsafe-undefined>}) did not
 appear in the source code.
The problem was due to a disagreement between core Racket and Typed Racket
 about how to encode a method with optional arguments as a function with
 a fixed-length argument list.
Racket used an extra run-time check; Typed Racket thought the check was redundant.
The fix was indeed to change Racket, but pre-fix versions of Typed Racket
 are a hair's breadth from a dangerous unsoundness.
Their saving grace is that the type optimizer does not transform methods;
 if it did, then user code would receive unsafe-undefined values because
 of the incorrect type assumption.


@; NOTE discuss this TR-env issue?
@;  the type for `make-do-sequence` is wrong / not enough
@;  the docs have a better type in an or/c
@;  but TR doesn't know which or/c-elem to use, when, and stick with the technically-unsound type below
@;  it's safe thanks to a guard, user code never gets a non-`a` value,
@;  but transient complains without a hand-coded (but needed, for performance) opt-out
@;
@; [make-do-sequence
@;  (-polydots (a b)
@;    ((-> (-values (list (a . -> . (make-ValuesDots '() b 'b))
@;                        (a . -> . a)
@;                        a
@;                        (Un (a . -> . Univ) (-val #f))
@;                        (Un (->... '() (b b) Univ) (-val #f))
@;                        (Un (->... (list a) (b b) Univ) (-val #f)))))
@;     . -> . (-seq-dots '() b 'b)))]


@section[#:tag "sec:transient:performance"]{Performance}
@; [X] get NSA data
@; [X] overhead plots ... incrementally
@; [X] exact plots, for trends
@; [X] table, typed/untyped (see perf sections)
@; [ ] DEBUG why is fsm7.7 faster untyped than fsm-S ???
@;     maybe BC vs CS, fsm-7.7 might be BC
@; [ ] DEBUG why is synth (recent data, 07-31?) so bad?
@; [ ] DEBUG why is fsmoo transient-typed faster than untyped?
@; [ ] blame perf table (use NSA)


@|sShallow| Racket trades static guarantees for performance.
The weakened type soundness and loss of complete monitoring and correct blame
 must lead to better performance.

This section presents the results of an evaluation using the @|GTP| benchmarks.
The granularity of the experiment is module-level, same as our Typed Racket
 experiment reported in @section-ref{sec:tr:evaluation}.
All data came from a dedicated Linux box with 4 physical i7-4790 3.60GHz cores
  and 16GB RAM.


@subsection[#:tag "sec:transient:ratio"]{Performance Ratios}

@(let* ((RT (get-ratios-table SHALLOW-CURRENT-BENCHMARK*))
        (deep-win-names+shallow-win-names
         (let ()
           (define (deep-wins? row)
             (<= (ratios-row-deep row) (ratios-row-shallow row)))
           (define-values [a b] (partition deep-wins? RT))
           (cons (map ratios-row-name a) (map ratios-row-name b))))
        (deep-win-names (car deep-win-names+shallow-win-names))
        (shallow-win-names (cdr deep-win-names+shallow-win-names))
        (num-deep-wins (length deep-win-names))
        (num-shallow-<1
         (for/sum ((row (in-list RT)))
           (if (< (ratios-row-shallow row) 1) 1 0))))
@list[
@figure[
  "fig:transient:ratio"
  @elem{Performance ratios for @|sdeep| and @|sshallow| types on the @|GTP| benchmarks.}
  @render-ratios-table[RT]
]
@elem{
@Figure-ref{fig:transient:ratio} presents typed/untyped ratios for
 the @|GTP| benchmarks.
The middle column lists the overhead of fully-typed @|sdeep| code relative
 to the untyped configuration.
The right column shows the overhead of fully-typed @|sshallow| types.

Because these @|sshallow| types are implemented with the @|stransient| semantics,
 one would expect them to be slower than @|sdeep| types.
The latter has no overhead in completely typed programs.
Indeed, @integer->word[num-deep-wins]
 @(if (= 1 num-deep-wins) "benchmark runs" "benchmarks run")
 faster with deep types.
The exceptions (@oxfordize[shallow-win-names]) are all programs that depend
 on untyped library code.

Surprisingly, @integer->word[num-shallow-<1]
 @(if (= 1 num-shallow-<1) "benchmark runs" "benchmarks run")
 faster with @|sshallow| types than with no types.
Despite the cost of @|stransient| checks, the Typed Racket optimizer is able
 to make the code run faster than untyped.
}])


@subsection[#:tag "sec:transient:overhead"]{Overhead Plots}

@render-overhead-plot*[
  "fig:transient:overhead"
  @elem{
  Deep vs. Shallow
  }
  ""
  ;; TODO stop at 10x? different title? (deep vs shallow?)
  render-relative-overhead-plot
  (for/list ((bm-name (in-list SHALLOW-CURRENT-BENCHMARK*)))
    (cons bm-name (cons default-rkt-version stransient)))
  #f
]


@Figures-ref["fig:transient:overhead" (exact-ceiling (/ (length SHALLOW-CURRENT-BENCHMARK*) overhead-plots-per-page))]
 plots the overhead of gradual typing
 in @|sDeep| Racket and @|sShallow| Racket.
As before, these plots show the proportion of @ddeliverable{D} configurations
 for values of @${D} between 1x and @~a[MAX-OVERHEAD]x.

@|sShallow| types give a tremendous improvement in TODO benchmarks.
These benchmarks fall victim to @|sdeep| types; the very-different
 @|stransient| semantics fares much better.

TODO benchmarks, however, typically run faster with @|sdeep| types.
Each demands a close look:
@itemlist[
@item{
  @bm{morsecode}
}
@item{
  @bm{zordoz}
}
]

Overall, @|sShallow| Racket does not disappoint.



@subsection[#:tag "sec:transient:exact"]{Exact Runtime Plots}

@render-overhead-plot*[
  "fig:transient:exact"
  @elem{
  Exact Deep vs Shallow
  }
  ""
  ;; TODO formatting? sync colors
  render-relative-exact-plot
  (for/list ((bm-name (in-list SHALLOW-CURRENT-BENCHMARK*)))
    (cons bm-name (cons default-rkt-version stransient)))
  #f
]

@Figures-ref["fig:transient:exact" (exact-ceiling (/ (length SHALLOW-CURRENT-BENCHMARK*) overhead-plots-per-page))]
 offers a different perspective on @|sdeep| and @|sshallow| types.
These exact runtime plots show how performance changes as the number of type
 annotations in a benchmark increases.
The leftmost column of each plot has one dot for each fully-untyped running
 time.
The right-most columns plot the fully-typed running times, and columns in
 between have data for every point at the same level of the performance lattice.

In @|sDeep| Racket, mixing typed and untyped code can lead to significant overhead.
Points in the middle columns are for mixed configurations, and can have
 high cost; @bm{zombie} in particular slows down in the middle.
Points on the right columns, however, do not suffer.
After critical boundaries are typed, performance with @|sdeep| types is
 often excellent.

In @|sShallow| Racket, the trend is simple: adding types slows code down.
There is a linear, upward trend in every benchmark except @bm{fsm}.

TODO investigate @bm{fsm}, why is transient untyped so much slower?
And why does it do so well with optimization?
Seems wrong.



@section[#:tag "sec:transient:future"]{Future Challenges}
@; vsc-dls-2019 has Retic/Pycket faster than untyped, 0.95x best-case


