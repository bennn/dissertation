#lang greenman-thesis/include

@; TODO
@; - [ ] describe for-loop opt, other things to reduce cost ... contracts too

@(require
   (only-in greenman-thesis/shallow/main
     SHALLOW-CURRENT-BENCHMARK*
     NSA-num-cores
     NSA-core-name
     NSA-core-speed
     NSA-RAM
     NSA-timeout-minutes
     get-ratios-table
     render-ratios-table
     ratios-row-name
     ratios-row-deep
     ratios-row-shallow
     get-blame-table
     render-blame-table
     blame-row-name
     blame-row-blame
     blame-row-deep
     blame-row-shallow
     s:cache-dir)
   (only-in greenman-thesis/pepm-2018/main
     retic-commit
     POPL-2017-BENCHMARK-NAMES)
   (only-in greenman-thesis/jfp-2019/main
     MAX-OVERHEAD
     transient-rkt-version
     render-relative-exact-plot
     render-relative-overhead-plot)
   (only-in greenman-thesis/oopsla-2019/pict
     typed-codeblock
     untyped-codeblock

     tr:compiler
     transient:defense
     transient:divide
     transient:all-type
     transient:occurrence-type
     transient:subtype
     transient:opt
     transient:blame:map)
   (only-in math/statistics
     mean))


@title[#:tag "chap:transient"]{@|sShallow| Racket}

The high costs of @|sdeep| types @;(@chapter-ref{chap:performance})
 and the weak guarantees of @|sshallow| types @;(@chapter-ref{chap:design})
 motivate a compromise.
In a language that supports both, programmers can mix @|sdeep| and
 @|sshallow| types to find an optimal tradeoff.
This chapter presents the first half of the compromise, namely, a @|sshallow| semantics for Typed Racket.
By default, Typed Racket provides @|sdeep| types via the @|snatural| semantics.
My work brings the @|stransient| semantics to the Typed Racket surface syntax.

Henceforth, @|sDeep| Racket refers to the original, @|snatural| implementation
 of Typed Racket and @|sShallow| Racket refers to its @|stransient| implementation.
Typed Racket refers to the common parts: the surface language and the type system.

@|sTransient| is a promising companion to @|sNatural| because it pursues an
 opposite kind of implementation.
Whereas @|snatural| eagerly enforces full types with guard wrappers, @|stransient|
 lazily checks only top-level shapes.
The lazy strategy means that @|stransient| does not need wrappers, which removes
 the main source of @|snatural| overhead.
@|sTransient| can also run without blame, removing another form of run-time cost.
By contrast, simply removing blame from @|snatural| changes little because
 blame information tags along with the guard wrappers.
To fully benefit from removing blame, @|sNatural| needs a new strategy for allocating wrappers in
 the first place.

Adapting the theory of @|stransient|@~cite{vss-popl-2017} to Typed Racket required
 several generalizations and insights (@sectionref{sec:transient:theory}).
In the course of this work, I also adapted the @|stransient| heap-based blame
 algorithm and identified several challenges (@sectionref{sec:transient:blame});
 first and foremost, the basic blame algorithm is prohibitively slow.
The final implementation takes care to reuse large parts of Typed Racket
 (@sectionref{sec:transient:implementation}).

The performance of @|sShallow| Racket is typically an improvement over
 @|sDeep| Racket, but both semantics have distinct strengths (@sectionref{sec:transient:performance}).
@|sTransient| always adds overhead relative to untyped Racket, but is the
 safer bet for mixed-typed programs.
@|sNatural| has better performance in programs with large chunks of typed
 code, and surpasses untyped Racket in many cases.
Whether @|sShallow| Racket can ever run faster than untyped code is an open
 question.
For now, @sectionref{sec:transient:future} lists several avenues worth exploring.


@section[#:tag "sec:transient:theory"]{Theory}

@citet{vss-popl-2017} present a first @|stransient| semantics.
This semantics communicates the key ideas behind @|stransient| and
 the behavior of Reticulated Python, but four characteristics make it
 unsuitable for a @|stransient| implementation in Racket.
It deals with a simpler language of static types;
 it includes a dynamic type;
 it does not include a subtyping relation;
 and its type checker is intertwined with the @emph{completion} pass
 that rewrites typed code.
This section outlines the design of a suitable model and its properties.


@subsection[#:tag "sec:transient:theory:types"]{More-Expressive Static Types}

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
Shape soundness should be a meaningful property that helps a programmer debug
 and enables shape-directed optimizations.

The original @|stransient| model suggests that type-shapes must be decidable
 in constant time@~cite{vss-popl-2017}.
This model contains type constructors for only reference cells and functions,
 both of which are easily recognized in a dynamically-typed language.
Reticulated, however, does not follow the constant-time suggestion in order
 to express object types.
The type-shape for an object with @${N} fields/methods checks for the presence
 of each member.
Thus, the cost is linear in the size of an object type.

@|sShallow| Racket includes additional linear-time shapes to support
 Typed Racket's expressive types with meaningful run-time checks.
Some are linear in the size of a type; others are linear in the size of incoming values.
In general, the goal is to enforce full constructors.
The type-shape for a function checks arity; for example,
 the types @${(\tfun{\tint}{\tnat})} and @${(\tfun{\tint\,\tint}{\tnat})}
 have different shapes.
The shape for a vector with a fixed number of elements checks length.
And the shape for a list checks for a null-terminated sequence of pairs.
Not all types correspond to value constructors, though.
These amorphous type @emph{connectives}@~cite{cl-icfp-2017,clps-popl-2019} call for
 recursive interpretations.
For example, @${\tagof{\stype_0 \cup \stype_1} = \tagof{\stype_0} \cup \tagof{\stype_1}}
 and @${\tagof{\fforall{\alpha_0}{\stype_0}} = \tagof{\stype_0}}
 provided @${\tagof{\stype_0}} does not depend on the bound variable.
Type variables have trivial shapes in other contexts, @${\tagof{\alpha_0} = \top}.
@Section-ref{sec:transient:types} goes into more detail about the implementation.


@subsection[#:tag "sec:transient:theory:dyn"]{Removing Type Dynamic}

Reticulated Python provides a dynamic type in the micro gradual typing
 tradition.
Consequently, every type-checking rule must accomodate the dynamic type
 in addition to the expected type.
Typed Racket does not have a dynamic type; instead it adds run-time tools
 so that a non-dynamic type system can make assumptions about untyped input.
Using this macro approach, only a handful of typing rules need to deal
 with dynamically-typed values.

The differences between the dynamic (micro) and non-dynamic (macro) typing rules have implications
 for @|stransient| run-time checks.
In the original model, the evaluation of any expression could bring
 a dynamically-typed value into a typed context.
In a non-dyn model, only boundaries and elimination forms can introduce
 an untyped value.
@Figure-ref{fig:transient:app-compare} illustrates the difference by contrasting
 the @|stransient| checks needed for a function application.
On the top, the dynamic approach requires three checks: two checks in case
 the function and argument are dynamically-typed, and one to validate the
 shape of the result.
On the bottom, only one check is needed because the function and argument
 are certain to have a correct, non-dyn shape.

@figure*[
  "fig:transient:app-compare"
  @elem{
   @|sTransient| completion rules for an application with a dynamic type (top) and
   without (bottom).
   Both rules insert run-time shape checks.
   The micro rule depends on a type coercion (@${\scoerce})
   metafunction@~cite{vss-popl-2017}.
  }
  @exact|{
    \begin{mathpar}
      \inferrule*[lab=With Dyn]{
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

      \inferrule*[lab=Without Dyn]{
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

Note: Adding blame to a non-dynamic language adds the need for an additional blame-map operation
 in @figure-ref{fig:transient:app-compare}, but no additional checks.
The blame map potentially needs an update because the argument flows
 in to the function.
There is no need for a check because the argument has a non-dynamic type.

Other rules can be simplified in a similar fashion.
The benefits are two-fold:
 non-dyn programs have fewer run-time checks to slow them down,
 and programmers have fewer places to search if a program manifests a
 boundary error.


@subsection[#:tag "sec:transient:theory:subt"]{Adding Subtyping}

A type system for untyped code must either include a subtyping
 judgment or force programmers to rewrite their data definitions.
Rewriting takes time and invites mistakes, therefore the
 migratory typing perspective demands a subtyping judgment.

The dynamic type is not a replacement for subtyping because it cannot describe designs.
For example, the untyped @codett{divide} function in @figure-ref{fig:transient:divide}
 either divides two numbers or returns the symbol @tt{'undef} if the divisor
 is zero.
Typed Racket lets a programmer express this union of two base types.
By contrast, the dynamic type can only summarize the result in an over-approximate
 way that provides no information to callers.

@figure*[
  "fig:transient:divide"
  @elem{Untyped division function with two kinds of output.}
  transient:divide]

Adapting @|stransient| to include subtyping is therefore an essential task
 for @|sShallow| Racket.
The addition is straightforward, but reveals a surprising distinction
 between declaration-site types and use-site types; @|stransient| with
 subtyping may miss certain type mistakes.
@Figure-ref{fig:transient:subtype} illustrates the pitfall of @|stransient|
 subtyping with a lazy factorial function.
This typed function asks for a thunk that computes a non-negative number
 and returns a thunk that computes a factorial.
Because of the type declaration on @codett{lazy-n}, it looks like @|stransient|
 should check that the call @codett{(lazy-n)} returns a non-negative number.
The actual behavior, however, depends on the type of the call expression.
If the implementation replaces the placeholder @codett{???} with the valid type
 @codett{Integer}, then @|stransient| checks for an integer and the untyped
 code at the bottom of the figure enters an infinite loop.

@figure*[
  "fig:transient:subtype"
  @elem{Lazy factorial function, may diverge under @|stransient| depending on the type subsituted for the @tt{???} placeholder.}
  transient:subtype]

In summary, the flexibility of subtyping limits the ability of @|stransient|
 checks to find mismatches due to type boundaries.
Checks are based on local uses, while boundaries are claims with a broad scope.


@subsection[#:tag "sec:transient:theory:completion"]{From Elaboration to Completion}

@citet{vss-popl-2017} intertwine typing and @|stransient| checks in a
 type-elaboration judgment.
The combination is a good fit for an implementation because check-insertion
 depends on static types, and one pass over the program is more efficient than
 two.
For the theory, however, it is better to keep surface typing separate
 from a second @emph{completion}@~cite{h-scp-1994}
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

Third, the separation encourages research on better completions and target-level
 typings.
The challenge is to use as few checks as possible to build the target term.
For example, suppose the variable @codett{xy} points to a pair of numbers
 and consider the expression @codett{(+ (car xy) (car xy))}.
The completion for @|sShallow| Racket produces the following term:

@code-nested{(+ (check Num (car xy)) (check Num (car xy)))}

@|noindent|Racket guarantees left-to-right evaluation, however, so the second check
 can never fail.
An improved completion would eliminate this check, other flow-dominated checks,
 and potentially many others.


@section[#:tag "sec:transient:blame"]{Work-in-progress: Blame}

Blame is an important part of a migratory typing system because it strengthens
 the key weakness of migratory types.
Static types guarantee that certain errors cannot occur at run-time.
Migratory types are weak because they cannot offer the same promise.
Errors can occur just about anywhere in typed code.
With blame, however, type-mismatch errors come with an action plan for debugging.
A programmer can follow the blame information to attempt a fix.

The usefulness of such an action plan depends on the blame strategy.
The current-best algorithm for @|stransient|, from @citet{vss-popl-2017},
 blames a set of boundaries.
The set is unsound and incomplete in the technical sense of
 @chapter-ref{chap:design}, but one would expect that it is more useful than
 no information.

Early experience with blame in @|sShallow| Racket, however, has identified
 two significant challenges.
First, scaling the original blame algorithm to Typed Racket raises
 questions about its accuracy.
Second, @|stransient| blame has a tremendous performance cost.
This section explains the challenges; performance concerns are deferred
 to @section-ref{sec:transient:blame-performance}.


@subsection{Basics of @|sTransient| Blame}
@; TODO better examples (ramble ok for now)
@;  work through without filter,
@;  then revisit to explain filter

The @|stransient| blame algorithm uses a global @emph{blame map} to connect
 run-time values to source-code boundaries.
This blame map uses heap addresses as keys.
Every non-primitive value in a program has a heap address and potentially
 a blame map entry.
The values in a blame map are collections of entries.
There are two kinds of entry in such a collection:
@itemlist[#:style 'ordered
@item{
 A @emph{boundary entry} combines a type with a source location.
 Whenever a value crosses one of the static boundaries between typed
  and untyped code, the blame map gains a boundary entry.
 For example, if the function @codett{f} flows out of typed code:
 @exact{\\@typed-codeblock['(
   "(define (f (n : Natural)) : String"
   "  ....)"
   "(provide f)")] \\}
 then the blame map gains an entry for @codett{f} that points to the
  type @codett{(-> Natural String)} and a source location.
}
@item{
  A @emph{link entry} combines a parent pointer and an action.
  The parent refers to another blame map key.
  The action describes the relation between the current value and its parent.
  Suppose the function @codett{f} from above gets applied to an
   untyped value @codett{v}.
  As the value enters the function, the blame map gains a link entry
   for @codett{v} that points to @codett{f} with the action @codett{'dom},
   to remember that the current value is an input to the parent.
}
]

If a @|stransient| run-time check fails, the blame map can supply a set
 of boundaries by following
 parent pointers up from the failed value.
Each parent pointer is partially responsible for the mismatched value.
Each boundary at the root of all parent paths contains possibly-unchecked
 type assumptions.
The programmer can begin debugging by reviewing these type assumptions.

@citet{vss-popl-2017} suggest a further refinement to this basic idea.
They filter the set of typed boundaries using the failed value and the
 action path that led to the boundary.
The action path gives a list of selectors to apply to the boundary type,
 ending with a smaller type.
Checking this type against the bad value helps rule out irrelevant boundaries.
For example, if the bad value is an integer and one of the boundary types expects an integer,
 then that boundary is not worth reporting.

In summary, the success of the blame map rests on three principles:
@itemlist[
@item{
  every type boundary in the program adds one boundary entry in the
   map for each value that crosses the boundary at runtime;
}
@item{
  every elimination form adds a link entry with a correct parent and action; and
}
@item{
  there is a run-time way to test whether a value matches part of
   a boundary type.
}
]
These principles are relatively easy to satisfy in a model language,
 but pose surprising challenges for a full language.


@subsection{Trusted Libraries Obstruct Blame}

A migratory typing system must be able to re-use host language libraries.
Racket, for example, comes with a list library that provides a @codett{map}
 function.
Both @|sDeep| Racket and @|sShallow| Racket can re-use this function by
 declaring a static type:

@typed-codeblock['("map : (All (A B) (-> (-> A B) (Listof A) (Listof B)))")]

@|noindent|Furthermore, both can import @codett{map} at no run-time cost.
@|sDeep| can trust that the implementation completely follows the type
 and @|sShallow|---without blame---can trust that @codett{map} always returns a list.

For @|sShallow| with blame, however, trusted re-use leads to imprecise blame.
@Figure-ref{fig:transient:blame:map} illustrates this phenomenon with a tiny example.
The typed function at the top of this figure expects a list of numbers,
 applies a trivial @codett{map} to the list,
 and later finds a bad element in the mapped list.
@|sTransient| blame should point back to the boundary between the typed
 function and the untyped list but cannot if @codett{map} does
 not update the blame map.
The solution is to register @codett{map} in the blame map with a boundary entry
 and add link entries before and after every call.
Unfortunately, the cost of this extra bookkeeping can add up.

@figure*[
  "fig:transient:blame:map"
  @elem{
    Unless @codett{map} updates the blame map, @|stransient| cannot point to
     any boundaries when @codett{(first nums2)} fails to return a number.
  }
  transient:blame:map
]



@subsection{Complex Flows, Tailored Specifications}

Getting blame right for the @codett{map} function requires careful bookkeeping.
The result list must have a link entry that points to the input list.
Additionally, the input function should point to this input list in case
 it receives a bad result.

Blame in @|sShallow| Racket depends on literal syntax to decide when complex
 reasoning is needed.
The original paper used the same method; primitive operations have tailored
 bookkeeping and other function applications create link entries in a standard
 way@~cite{vss-popl-2017}.

Obviously, the syntactic approach is brittle.
Renaming @codett{map} leads to misleading blame errors.
The same goes for applications of an expression instead of a literal identifier.
Improving precision is an open challenge.


@subsection{Multi-Parent Paths}

A link entry points to one parent.
Several functions, however, create data with multiple parents.
One basic example is an @codett{append} function on lists:

@typed-codeblock['("(append xs ys)")]

@|noindent|The result list contains the elements of both inputs.
At a minimum, there should be two parents to blame if something goes wrong.

A second, more complicated example is a @codett{hash-ref} function that
 may return a default value:

@typed-codeblock['("(hash-ref h k d)")]

@|noindent|If the table @codett{h} has a binding for the key @codett{k},
 then the result comes from the table.
Otherwise, the result is computed by the default thunk.

A blame map clearly needs conditional and multi-parent paths to give precise
 error outputs.
But the cost of building and traversing the additional link entries may be
 high.
Thus we leave such paths to future work.


@subsection{Expressive Link-Entry Actions}

@figure*[
  "fig:transient:blame:path"
  @elem{Sample blame actions in @|sShallow| Racket.}
  @exact{{
  \renewcommand{\twoline}[2]{\parbox[t]{2.4in}{#1\newline#2}}
  \begin{tabular}{l@"@" {~~~~}l}
    Action Template & Interpretation
  \\\hline
    @codett{(dom n)} & @codett{n}-th argument to a function
  \\
    @codett{(cod n)} & @codett{n}-th result from a function
  \\
    @codett{(case-dom (k n))} & \twoline{@codett{n}-th argument (of @codett{k} total) to an}{overloaded function}
  \\[3.5ex]
    @codett{(object-method (m n))} & \twoline{@codett{n}-th argument to method @codett{m} of an}{object}
  \\[3.5ex]
    @codett{list-elem} & Element of a homogeneous list
  \\
    @codett{list-rest} & Tail of a list
  \\
    @codett{(list-elem n)} &  \twoline{Element of a heterogeneous list,}{e.g. @codett{(List Boolean Number String)}$\!\!\!\!$}
  \\[3.5ex]
    @codett{hash-key} & Key of a hashtable
  \\
    @codett{hash-value} & Value of a hashtable
  \\
    @codett{(struct-field n)} & @codett{n}-th field of a structure
  \\
    @codett{(object-field f)} & Field @codett{f} of an object type
  \\
    @codett{noop} & No action; direct link to parent
  \end{tabular}}}
]

A check entry in the blame map has two parts: a parent pointer and
 an action.
The action informs the type-based filtering.
Given a type for the parent, the action says what part of the type is
 relevant to the current value.

The model for Reticulated comes with three actions: @codett{Res},
 @codett{Arg}, and @codett{Deref}.
These help traverse simple function types (@${\tfun{\stype}{\stype}})
 and reference cells; for example, starting from the parent type
 @${\mathsf{ref}\,\tint} and applying the @codett{Deref} action
 focuses on the element type @${\tint}.
The implementation of Reticulated adds one action, @codett{Attr},
 and generalizes @codett{Arg} with an index.
Starting from the following Reticulated type:

@code-nested{Function([int, str], float)}

@|noindent|the action @codett{[Arg, 1]} focuses on the type @codett{str} of the
 second positional argument.
Similarly, the action @codett{[Attr, "foo"]} focuses on the member @codett{"foo"}
 of an object type.

Despite the extensions, the action language in Reticulated suffers from
 imprecision in two ways.
First, it has no way to refer to certain parts of a type.
If a function uses optional or keyword arguments, then Reticulated has no
 way to test whether the type is irrelevant; such types cannot be filtered from
 the blame output.
Second, it may conflate types.
The action @codett{Deref} seems to apply to any data structure.
If a nested list value crosses the boundaries @codett{List(List(int))}
 and @codett{List(Dict(str, str))}, and then an elimination returns
 a string where an @codett{int} was expected, a plain @codett{Deref}
 incorrectly filters out the @codett{Dict} type.
The developer needs to see both types because neither matches the actual nested list value.
@; TODO can we make a retic example that blames both? So far I'm getting trivial blame  (in retic repo, blame_conflate.py)

@|sShallow| Racket thus comes with an extensive action language to
 prevent imprecision.
@Figure-ref{fig:transient:blame:path} lists a few actions, along with
 a brief interpretation.
Function actions must handle multiple arguments, multiple results,
 methods, and overloadings.
Data structures have tailored actions.
Lists, for example, require three kinds of actions:
 @codett{list-elem} to dereference a simple list,
 @codett{list-rest} to move to the tail of a list keeping the same type,
 and an indexed element action for fixed-sized lists with distinct types
 in each position.
Finally, the @codett{noop} action adds a direct link
 to track a copy from one data structure to another (@codett{vector-copy!})
 or a wrapper (@codett{chaperone-procedure}).


@subsection[#:tag "sec:transient:blame:types"]{Types at Runtime}

@|sTransient| blame needs types at runtime, or a close substitute, to filter
 irrelevant boundaries.
These runtime types must have selectors for
 each possible action and an interpretation function that checks the
 shape of a value against the shape of a type.

@|sShallow| Racket's runtime types are a revived version of its static types.
During compilation, static types get serialized into a chain of type constructor
 calls.
After a runtime error occurs, @|sShallow| Racket re-evaluates the constructor
 definitions and uses these constructors to revive types.
This revival approach re-uses at least 4,000 lines of Typed Racket to good effect:
 roughly 3,000 lines of constructor and selector definitions,
 @; roughly: 300 type->transient-sc, 60 sc def, 270 sc inst, 450 sc opt
 and 1,000 lines that turn a type into a @|stransient| check.
It also handles type aliases nicely.
The static environment knows all relevant aliases and can serialize them
 along with the type.

Revival unfortunately fails for generative structure types.
The run-time type and the static type are two different entities, and so
 @|sShallow| Racket is unable to parse the serialized types at run-time.
If parsing were to succeed, finding the correct predicate for a generative
 type is a separate challenge.
At compile-time, it suffices to generate a correct identifier.
At run-time, @|stransient| needs to evaluate a correct identifier in
 the right run-time context to find the predicate.

A different approach may be able to solve the generative-types problem,
 but for now it remains an open question.
A related question, though, is whether @|stransient| is better off with
 a different method of filtering.


@section[#:tag "sec:transient:implementation"]{Implementation}

@figure*[
  "fig:transient:tr-overview"
  @elem{Stages in the @|sDeep| Racket compiler. @|sShallow| can re-use the expander and type checker in full, and parts of the optimizer.}
  tr:compiler
]

@|sShallow| Racket is an extension of the (@|sDeep|) Typed Racket codebase.
The goal is not to create a fork, but rather to adapt the existing compiler
 and provide a uniform experience to programmers.

@Figure-ref{fig:transient:tr-overview} presents a high-level organization diagram of the
 Typed Racket compiler@~cite{tscff-pldi-2011}.
Source code goes through a macro-expansion step at the start.
The type checker operates on expanded code; it validates the program and
 attaches type annotations as metadata for later passes.
Third, the compiler turns boundary types into higher-order contracts.
The last major step is the type-driven optimizer, which uses type annotations
 to remove unnecessary runtime checks.

@|sShallow| Racket can re-use the expander and type checker as-is.
The ``contract'' and ``optimize'' steps require changes.
Contract generation must create @|stransient| checks rather than
 @|sdeep| higher-order contracts (@section-ref{sec:transient:types}).
Additionally, the contract pass must rewrite all typed code with @|stransient|
 checks (@section-ref{sec:transient:defense}).
The ``optimize'' pass must be restricted because it cannot rely on full types (@section-ref{sec:transient:optimize}).

Along the way, the implementation effort brought a few surprises.
Challenges with universal types and occurrence types cause the current
 @|sShallow| Racket to reject some well-typed code (@section-ref{sec:transient:surprise}).
@|sDeep| Racket rejects the same programs.
The experience led to several improvements to Typed Racket, Racket, and
 libraries (@section-ref{sec:transient:pr}).


@subsection[#:tag "sec:transient:types"]{Types to Shapes}

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
Each type comes with a shape that illustrates the implementation
 and a brief discussion.
Actual shapes in the implementation do not use contract combinators such
 as @tt{and/c} for performance.

@itemlist[
@item{
  @example-type-shape[
    #:type "(Listof Real)"
    #:shape "list?"
    #:cost "O(v)"
  ]

  The type represents lists of real numbers.
  The shape accepts any proper list, but not improper lists such as
   @codett{(cons 1 (cons 2 3))}.
  The run-time cost depends on the size of input values in the worst case,
   but pairs are immutable and the predicate @codett{list?} caches
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
  In general, built-in lists have the only shape whose cost depends on the size
   of input values.
}
@item{
  @example-type-shape[
    #:type "(Vector Real)"
    #:shape "(and/c vector? (λ(v) (= 1 (vector-length v))))"
    #:cost "O(1)"]

  Represents a vector that contains one number.
  The shape checks length; the optimizer can use this fact.
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
    #:type "(Weak-HashTable Symbol Any)"
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

  Represents a syntax object that contains a symbol.
  The shape checks for a syntax object with a symbol inside.
}
@item{
  @example-type-shape[
    #:type "Integer"
    #:shape "exact-integer?"
    #:cost "O(1)"]

  Represents a mathematical integer.
  The shape checks for exactness; an inexact integer such as @codett{4.1} is
   not allowed.

  Other numeric types require larger checks for additional properties,
   for example @codett{Negative-Integer} looks for an exact integer that
   is less than zero.

  To the type system, numeric types are wide unions.
  Shape enforcement flattens these unions wherever possible.
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

  Represents a function with one required argument.
  The shape checks arity.
}
@item{
  @example-type-shape[
    #:type "(-> Real * Real)"
    #:shape "(arity-includes/c 0)"
    #:cost "O(1)"]

  Represents a function that accepts any number of positional arguments.
  The shape looks for functions that can accept zero arguments,
   but it does not check whether they accept more.
}
@item{
  @example-type-shape[
    #:type "(case-> (-> Real Real) (-> String Real String))"
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


@subsection[#:tag "sec:transient:defense"]{Inserting Shape Checks}
@; 2020-09-25 "defender" , but that name sounds bad to me lately

@figure*[
  "fig:transient:defense"
  @elem{A @|sshallow|-typed function defended with @|stransient| checks.}
  transient:defense
]

@figure*[
  "fig:transient:opt"
  @elem{Type prevents callers from sending an optional argument, but the function body can use the default value.}
  transient:opt
]

@|sShallow| Racket rewrites typed code to include @|stransient| shape checks.
Checks guard the positions where an untyped value might appear
 (@section-ref{sec:design:tech:transient}); in particular:
@itemlist[
@item{
  at the source-code boundaries to untyped code;
}
@item{
  around elimination forms;
}
@item{
  and at the entry of every function.
}
]

Boundaries clearly need protection.
If typed code expects a number and imports a value from untyped code,
 the value could have any shape and therefore needs a check.

Elimination forms need protection for the same reason, but are an
 over-approximation.
@Figure-ref{fig:transient:defense} provides a concrete example with a
 for loop that sums up a list of numbers.
Every step of the loop first checks the current list element.
If the list came from untyped code, then the checks are clearly needed.
The list might come from typed code, though, in which case the checks
 can never fail.

@Figure-ref{fig:transient:defense} also contains a function check.
The inputs to every typed function are checked to validate the type assumptions
 in the function body.
These checks might be unnecessary if the function never escapes to untyped code,
 but escapes are hard to detect because
 a typed function can escape as an argument to a combinator
 @codett{(map sum-list nss)} or via a macro-introduced reference.


@subsubsection[#:tag "sec:transient:surprise"]{Current Limitations}

@figure*[
  "fig:transient:all-type"
  @elem{If the shape of a universal type depends on the bound variable, then
   @|stransient| must either reject the program or treat type instantiation as an elimination form.}
  transient:all-type
]

@figure*[
  "fig:transient:occurrence-type"
  @elem{
    Occurrence types may change the type environment in each branch of
     an @codett{if} statement.
    @|sTransient| must either check the changes or disallow occurrence types
     on untyped functions.
  }
  transient:occurrence-type
]

The current implementation attaches @|stransient| checks at two kinds
 of syntax: boundaries and run-time elimination forms.
This approach does not suffice to protect all types, thus
 some well-typed programs are currently rejected to ensure soundness.

Unrestricted universal types are one problem.
If the shape @${\tagof{\stype}} of a universally-quantified type
 @${\fforall{\alpha}{\stype}} depends on the bound variable, then
 @|sShallow| Racket rejects the program (@figure-ref{fig:transient:all-type}).
The trouble is that type instantiation can change the shape of such types,
 but type instantiation is not currently a run-time elimination form.

Occurrence types at a boundary are a second problem.
A program cannot assign an occurrence type to an untyped value,
 as in @figure-ref{fig:transient:occurrence-type}.
This program uses @codett{require/typed} to import an untyped function with a nonsensical
 occurrence type.
The typechecker trusts that all @codett{require/typed} annotations are valid
 claims, and so the rest of the program type checks.
The typechecker assumes that a run-time check will catch any faulty claims.
In this program, however, the occurrence type adds a side effect claim
 that is not caught by any check.


@subsection[#:tag "sec:transient:optimize"]{Optimizer}

@figure*[
  "fig:transient:optimize"
  @elem{TR optimizations and whether @|sShallow| can re-use them.}
  @exact{{
  \deftablemacros{}
  \begin{tabular}[t]{cc}
  \begin{tabular}{lr}
    Topic & Shape-Safe?
  \\\hline
    \(\mathsf{apply}\)          & \tblY
  \\
    \(\mathsf{box}\)            & \tblY
  \\
    \(\mathsf{dead{\mhyphen}code}\)      & \tblN
  \\
    \(\mathsf{extflonum}\)      & \tblY
  \\
    \(\mathsf{fixnum}\)         & \tblY
  \\
    \(\mathsf{float{\mhyphen}complex}\)  & \tblY
  \\
    \(\mathsf{float}\)          & \tblY
  \\
    \(\mathsf{list}\)           & \tblY
  \end{tabular}
  &
  \begin{tabular}{lr}
    Topic & Shape-Safe?
  \\\hline
    \(\mathsf{number}\)         & \tblY
  \\
    \(\mathsf{pair}\)           & \tblN
  \\
    \(\mathsf{sequence}\)       & \tblY
  \\
    \(\mathsf{string}\)         & \tblY
  \\
    \(\mathsf{struct}\)         & \tblY
  \\
    \(\mathsf{unboxed{\mhyphen}let}\)    & \tblY
  \\
    \(\mathsf{vector}\)         & \tblY
  \\
  \\
  \end{tabular}
  \end{tabular}
}} ]

Typed Racket uses static types to compile efficient code@~cite{sta-nt-base-types,stff-padl-2012,stf-oopsla-2012}.
To give a basic example, a dynamically-typed sum @codett{(+ n0 n1)} can be
 complied to add its inputs without first confirming that they are numbers.
In principle, such optimizations may rely on full types.
These ``@|sdeep|'' optimizations are not safe for @|sShallow| Racket because it only
 guarantees the top type constructor.

@Figure-ref{fig:transient:optimize} lists all optimization topics and shows,
 suprisingly, that only two are unsafe for @|sshallow| types.
The @${\mathsf{dead{\mhyphen}code}} pass remove type-inaccessible branches of an overloaded function.
With @|sdeep| types, run-time contracts make these branches inaccessible.
@|sShallow| types allow raw functions to flow to untyped code, and therefore
 the branches are not sealed off by a wrapper.
The @${\mathsf{pair}} pass depends on full types to rewrite nested accessors, such as @codett{cdar},
 to versions that assume a deep pair structure.

Other passes are re-used in @|sShallow| Racket.
The benefit of these optimizations is sometimes enough to outweigh the cost
 of @|stransient| checks (@section-ref{sec:transient:performance}).
Certain re-used passes, though, force design decisions.
The @${\mathsf{apply}} pass requires all @|sshallow|-typed functions to
 check their inputs, whether or not they escape to untyped code.
The @${\mathsf{list}} and @${\mathsf{sequence}} passes depend on the @${O(n)}
 shape check for list types.
Finally, the @${\mathsf{unboxed{\mhyphen}let}} pass is only safe by virtue
 of a conservative escape analysis.


@subsection[#:tag "sec:transient:pr"]{Bonus Fixes and Enhancements}

@; NOTE: text is too wide for a 2-column table, even if we remove "racket" from every line
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
At run-time, @|stransient| sometimes found incorrect types with its
 checks.
The HTDP fix offers a simple example (@github-pull["racket" "htdp" "98"]).
A library-provided function promised to return a unit value and actually
 returned a boolean.
@|sTransient| caught the unsoundness.

The fix to Racket is especially interesting (@github-pull["racket" "racket" "3182"]).
It came about because some @|sshallow|-typed programs failed with a strange error message:

@nested[#:style 'inset @codett{Expected a real number, got #<unsafe-undefined>}]

@|noindent|These programs were fully-typed, but somehow a run-time value
 contradicted the type checker without causing trouble in the @|sDeep| semantics.
Worse, this sentinel undefined value did not appear in the source code.
The problem was due to a disagreement between core Racket and Typed Racket
 about how to encode a method with optional arguments as a function with
 a fixed-length argument list.
Racket used an extra run-time check; Typed Racket thought the check was redundant.
The fix was indeed to change Racket, which means that pre-fix versions of Typed Racket
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

@; TODO integrate
@; To be clear, @|sShallow| Racket does not include blame because the
@;  performance cost is unacceptable.
@; Except where otherwise noted, @|sshallow| data is this paper is for
@;  @|stransient| without blame.



@|sShallow| Racket sacrifices static guarantees for a wrapper-free implementation.
The loss of wrappers implies a loss of full type soundness, complete monitoring,
 and correct blame.
As compensation, @|sshallow| needs to demonstrate improved performance.

This section applies the methods from @chapter-ref{chap:performance} to evaluate
 @|sShallow| Racket on the @|GTP| benchmarks.
The granularity of the experiment is module-level, same as our @|sDeep| Racket
 experiment from @section-ref{sec:tr:evaluation}.
All data is from a dedicated Linux box with @id[NSA-num-cores] physical
  @id[NSA-core-name] @id[NSA-core-speed] cores and @id[NSA-RAM] RAM.


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
        (deep-wins-all? (= num-deep-wins (length RT)))
        (num-shallow-<1
         (for/sum ((row (in-list RT)))
           (if (< (ratios-row-shallow row) 1) 1 0))))
@list[
@figure*[
  "fig:transient:ratio"
  @elem{Performance ratios for @|sdeep| and @|sshallow| types on the @|GTP| benchmarks.}
  @render-ratios-table[RT]
]
@elem{
@Figure-ref{fig:transient:ratio} presents typed/untyped ratios for
 the benchmarks.
The middle column lists the overhead of fully-typed @|sdeep| code relative
 to the untyped configuration.
The right column shows the overhead of fully-typed @|sshallow| types.

Because these @|sshallow| types are implemented with the @|stransient| semantics,
 one would expect them to run slower than @|sdeep| types because the
 latter has no overhead in completely typed programs.
@(if deep-wins-all?
  @elem{Indeed, @|sdeep| runs faster in every row and has a worst-case overhead under 2x.}
  @elem{Indeed, @integer->word[num-deep-wins]
        @(if (= 1 num-deep-wins) "benchmark runs" "benchmarks run")
         faster with deep types.
        The exceptions (@oxfordize[shallow-win-names]) are all programs that depend
         on untyped library code.})
@|sShallow| typically does well, with overhead under 5x, but a few benchmarks
 have larger slowdowns due to @|stransient| checks.
The worst is @bm{zombie}, which suffers a 30x ovehead because of the many
 elimination forms in typed code.
A better completion pass may be able to reduce this high cost.
The best case for @|stransient| is @bm{lnm}, which nearly runs faster than
 the fully-untyped configuration.
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
    (cons bm-name (cons transient-rkt-version stransient)))
  s:cache-dir
]


@Figures-ref["fig:transient:overhead" (exact-ceiling (/ (length SHALLOW-CURRENT-BENCHMARK*) overhead-plots-per-page))]
 plot the overhead of @|sDeep| and @|sShallow| Racket.
As before, these plots show the proportion of @ddeliverable{D} configurations
 for values of @${D} between 1x and @~a[MAX-OVERHEAD]x.

@|sShallow| types lead to a huge improvement, from over 20x down to 8x or lower, in
 @integer->word[(length '(forth fsmoo dungeon jpeg suffixtree take5 synth quadU quadT))]
 benchmarks.
In one way or another, these benchmarks suffer from high @|sdeep| overhead
 due to eager and wrapped checks.
The wrapper-free @|stransient| semantics removes the issue.
@|sShallow| improves on a few other benchmarks, and does equally-well on
 almost all the rest.
The one exception is @bm{morsecode}, which fares better with @|sdeep| types.
Three characteristics account for the discrepancy:
 @bm{morsecode} boundaries create few wrappers;
 the @|stransient| laziness does not end up saving many checks;
 and the overhead of @|stransient| checks ends up slowing down large chunks of typed code.
Overall, @|sShallow| Racket lives up to its promise of better mixed-typed performance.


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
    (cons bm-name (cons transient-rkt-version stransient)))
  s:cache-dir
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
After critical boundaries are typed, performance is often excellent.

In @|sShallow| Racket, the trend is simple: adding types slows code down.
There is a linear, upward trend in every benchmark.
As the overhead plots anticipate, the linear cost is typically much lower
 than the extremes of @|sdeep| types.


@subsection[#:tag "sec:transient:blame-performance"]{Blame Performance}

@(let* ((BT (get-blame-table SHALLOW-CURRENT-BENCHMARK*))
        (blame-timeout*
         (for/list ((r (in-list BT))
                    #:when (equal? "timeout" (blame-row-blame r)))
           (blame-row-name r)))
        (blame-oom*
         (for/list ((r (in-list BT))
                    #:when (equal? "out of memory" (blame-row-blame r)))
           (blame-row-name r)))
        (deep-beats-blame*
         (filter values
          (for/list ((r (in-list BT)))
            (define n (blame-row-blame r))
            (if (or (not (number? n))
                    (< (blame-row-deep r) n))
              (blame-row-name r)
              #f))))
        (x-blame-over-shallow*
         (for/list ((r (in-list BT))
                    #:when (number? (blame-row-blame r)))
           (/ (blame-row-blame r)
              (blame-row-shallow r))))
        (avgx-blame-over-shallow
         (rnd (mean x-blame-over-shallow*)))
        (worstx-blame-over-shallow
         (rnd (apply max x-blame-over-shallow*))))
@list[
@figure*[
  "fig:transient:blame-performance"
  @; TODO is "perf ratio" the right word? These are really typed/baseline right?
  @elem{
   Performance ratios for @|sShallow| Racket with blame,
    @|sShallow| without blame, and the worst-case of @|sDeep| types.
   The @|sShallow| columns are for the fully-typed configuration;
    the @|sDeep| column uses the slowest configuration.
   The blame experiments ran on a dedicated Linux machine with @id[NSA-RAM] RAM
    for at most @id[NSA-timeout-minutes] minutes.
  }
  @render-blame-table[BT]
]
@elem{

@Figure-ref{fig:transient:blame-performance} evaluates the overhead of
 @|sShallow| Racket with blame enabled.
The second column of this table measures the overhead of blame on
 the fully-typed configuration.
For comparison: the third column lists the overhead of the same
 configuration without blame, and the fourth column lists the absolute
 worst-case of @|sdeep| types.
This table reports only the fully-typed configuration for @|sshallow| because
 this configuration contains the greatest number blame-map updates.
Configurations with fewer typed modules have syntactically fewer locations
 that must touch the global map.

The data shows that blame adds tremendous overhead to @|sShallow| Racket.
@Integer->word[(length blame-timeout*)] benchmarks fail to terminate within
 a generous @id[NSA-timeout-minutes]-minute limit.
One benchmark, @car[blame-oom*], ends with an OS-level memory error
 after consuming a huge chunk of a @id[NSA-RAM] RAM pool.
The rest run far slower than @|sshallow| without blame.

Surprisingly, the fourth column shows that @|sshallow| blame costs
 more than the worst case of @|sDeep| types in @id[(length deep-beats-blame*)] benchmarks.
The benchmarks in which @|sDeep| loses all send higher-order values across
 several boundaries; each crossing makes an expensive wrapper.
By contrast, @|sShallow| blame slows down every operation by a small factor
 and allocates a small amount of memory for every value.
These small costs add up, even in our relatively short-running benchmarks.

Our blame results are far less optimistic than the early report in
 @citet{vss-popl-2017}, which found an average slowdown on 2.5x and
 worst-case slowdown of 5.4x on fully-typed configurations.
For @|sShallow| Racket benchmarks that terminate, the average slowdown
 from blame is @id[avgx-blame-over-shallow]x
 and the worst-case is @id[worstx-blame-over-shallow]x.
These different statistics are due to two factors that let Reticulated
 insert fewer checks: the chosen benchmarks and gradual type inference.

Regarding benchmarks, @citet{vss-popl-2017} use small programs from the
 @|PYBENCH| suite.
@(let ((num-numeric (length '(pidigits nbody float)))
       (num-total (length POPL-2017-BENCHMARK-NAMES)))
  @elem{@Integer->word[num-numeric] of the @integer->word[num-total] benchmarks
  focus on numeric computations; since the blame map does not track primitive
  values, adding blame adds little overhead.})
Four others have since been retired from the Python suite because they are
 too small, unrealistic, and unstable (@shorturl["https://" "pyperformance.readthedocs.io/changelog.html"]).
Among the remaining benchmarks, the overhead of blame appears to increase with
 the size of the program.
Larger Reticulated benchmarks should run on par with @|sShallow| Racket.
Indeed, I converted the @bm{sieve} benchmark to Reticulated and found that
 adding blame increases its running time from ~40 seconds to a time out after
 10 minutes.

The type inference issue is subtle.
Reticulated frequently infers the dynamic type for local variables.
Doing so is type-sound and lets Reticulated skip many runtime checks
 and blame-map updates; however, the programmer gets less precise type
 and blame information.
For example, the following Python snippet creates a list of numbers,
 mutates the list, and reads the first element.

@typed-codeblock['(
  "def set(xs):"
  "    xs[0] = \"X\""
  "    return"
  ""
  "def main():"
  "    nums = list(range(3))"
  "    set(nums)"
  "    return nums[0] < 1"
  ""
  "main()"
  "# TypeError: unorderable types: str() < int()"
)]

Reticulated infers the dynamic type for the list @tt{nums} and does
 not check that @tt{nums[0]} returns a number.
Running this program leads to a Python exception about comparing strings
 and integers.
For the benchmarks, the lack of checks leads to a faster running time,
 especially in programs that incrementally update local variables in a loop.
If updates lead to the dynamic type, then run-time operations are free of
 shape checks.
}])


