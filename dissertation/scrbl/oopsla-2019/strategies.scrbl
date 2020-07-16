#lang greenman-thesis/include
@(require greenman-thesis/oopsla-2019/main)

@title[#:tag "sec:design:strategies"]{Overview of Type-Enforcement Strategies}

@; %% TODO change intro
To validate the expressiveness of the framework, this section models six
 semantics from the literature.
Three semantics have been implemented for full-fledged languages:
 @|nname|, @|tname|, and @|ename| (@sectionref{sec:design:jungle}).
One, @|aname|, explores a theoretical tradeoff to improve the blame guarantees of the
 @|tname| semantics.
The remaining two, @|cname| and @|fname|, originate in prior work@~citep{g-popl-2015,gf-icfp-2018}
 and explore the gap between the @|nname| and @|tname| strategies.

@; % TODO need to discuss higher-order / first-order before this! 2020-01-23 could use more explanation, but I guess its clear

This section presents an informal overview of the type-enforcement strategies;
 all technical definitions and properties appear in @sectionref{sec:design:technical}.
The discussion begins with the semantics that is lowest on the error
 preorder (@|nname|) and ascends (@|ename|):

@exact|{
\smallskip
\noindent{}\begin{tabular}{l@{~~}c@{~~}l}
  \
  \textstrat{@|nname|}
  & : & Wrap higher-order values; eagerly validate first-order values.
  \\[0.3ex]
  \textstrat{@|cname|}
  & : & Wrap higher-order and first-order values.
  \\[0.3ex]
  \textstrat{@|fname|}
  & : & Wrap higher-order and first-order values, but drop inner wrappers.
  \\[0.3ex]
  \textstrat{@|tname|}
  & : & Check the shape of all values that appear in typed code.
  \\[0.3ex]
  \textstrat{@|aname|}
  & : & Check shapes like @|tname|; use wrappers to remember boundaries.
  \\[0.3ex]
  \textstrat{@|ename|}
  & : & Check nothing. Do not enforce static types at runtime.
\end{tabular}
}|


@section[#:tag "sec:design:strat:natural"]{@|nname|}

@|nname| strictly enforces the boundaries between typed and untyped code.
Every time a typed context imports an untyped value,
 the value receives a comprehensive check.
For first-order data, the full check implies a deep traversal of the incoming
 value.
For higher-order data, a full check is impossible; instead,
 @|nname| wraps the incoming value to monitor its future behavior.

More formally, when an untyped value @${\svalue} flows into a context
 that expects some value of type @${\stype},
 @|nname| employs the type-directed validation strategy in the left column
 below.
The strategy on the right protects a typed value from an untyped 
 context:

@exact|{
\formatapproach{@|nname|}{
  check that $\svalue$ is an integer
}{
  check that $\svalue$ is a tuple and recursively validate its elements
}{
  check that $\svalue$ is a function and wrap $\svalue$ to
   protect higher-order inputs and validate outputs
}{
  check nothing
}{
  recursively protect the elements
  \\
}{
  wrap $\svalue$ to
   validate inputs and protect higher-order outputs
}
}|

@subsection[#:tag "sec:design:strat:performance"]{Theoretical Costs, Motivation for Alternative Methods}

Implementations of the @|nname| approach have struggled with the performance
 overhead of enforcing types@~citep{ff-icfp-2002,gtnffvf-jfp-2019}
 and have inspired semantics with tighter time and space
 bounds@~citep{htf-hosc-2010,stw-pldi-2015,g-popl-2015,bbst-oopsla-2017,fgsfs-oopsla-2018,kas-pldi-2019}.
A glance at the sketch above suggests three sources for this overhead:
 the act of @emph{checking} that a value matches a type,
 the layer of @emph{indirection} that a wrapper adds,
 and the @emph{allocation} cost.

For base types and higher-order types, the cost of checking is presumably low.
Testing whether a value is an integer or a function is a cheap operation in
 languages that support dynamic typing.
Pairs, however, illustrate the potential for serious overhead.
When a deeply-nested pair value reaches a boundary, @|nname| follows the type
 to conduct an eager and comprehensize check.
The cost of a successful check is linear in the size of the type.
In a language with recursive types---perhaps for lists---the
 cost is linear in the size of the incoming value.

The indirection cost grows in proportion to the number of wrappers on a value.
There is no limit to the number of wrappers in @|nname|,
 so this cost can grow without bound.
Indeed, the combined cost of checking and indirection can lead to
 exponential slowdown even in simple programs@~citep{htf-hosc-2010,g-popl-2015,tfdfftf-ecoop-2015,fgsfs-oopsla-2018,kas-pldi-2019}.

Lastly, creating a wrapper initializes a data structure.
Creating an unbounded number of wrappers incurs a proportional cost,
 which may add up to a significant fraction of a program's running time.

These theoretical costs motivate the next three strategies.
First, the @|cname| strategy (@sectionref{sec:design:strat:conatural}) reduces the
 up-front cost of checks with additional wrappers.
Second, the @|fname| strategy (@sectionref{sec:design:strat:forgetful}) reduces indirection
 by keeping at most two wrappers on any value and discarding the rest.
Third, the @|tname| strategy (@sectionref{sec:design:strat:transient})
 removes wrappers altogether by enforcing a weaker invariant.


@subsection{\forigins{@|nname|}}

The name ``@|nname|'' comes from @citet{mf-toplas-2009}, who use it
 to describe a proxy method for transporting untyped functions into a
 typed context.
Prior works on higher-order contracts@~citep{ff-icfp-2002},
 remote procedure calls@~citep{ok-popl-2003}, and
 typed foreign function interfaces@~citep{r-jfp-2008}
 employ a similar method.
In the gradual typing literature, this method is also
 called ``guarded''@~citep{vksb-dls-2014}, ``behavioral''@~citep{clzv-ecoop-2018},
 and ``deep''@~citep{tgpk-dls-2018}.


@section[#:tag "sec:design:strat:conatural"]{@|cname|}

The @|cname| method checks only the shape of values at a boundary.
Instead of eagerly validating the contents of a data structure,
 @|cname| creates a wrapper to perform validation by need.
The cost of checking at a boundary is thereby reduced to the worst-case cost
 of a shape check.
Allocation and indirection costs may increase, however, because first-order
 values now receive wrappers.

@exact|{
\formatapproach{@|cname|}{
  check that $\svalue$ is an integer
}{
  check that $\svalue$ is a tuple and wrap $\svalue$ to
   validate projections
}{
  check that $\svalue$ is a function and wrap $\svalue$ to
   protect higher-order inputs and validate outputs
}{
  check nothing
}{
  wrap $\svalue$ to
   protect future projections
   \\
}{
  wrap $\svalue$ to
   validate inputs and protect higher-order outputs
}
}|

@; %% Because the wrappers eventually perform the same checks as @|nname|, the
@; %%  @|cname| method satisfies the same type soundness and complete monitoring
@; %%  properties.
@; %% It may fail to detect a mismatch that @|nname| reports, however, if the
@; %%  program does not depend on the incompatible value.


@subsection{\forigins{@|cname|}}

The @|cname| strategy introduces a small amount of laziness;
 every immutable data structure gets a guard wrapper.
@citet{fgr-ifl-2007} implement exactly this strategy for Racket struct contracts.
Other researchers have explored variations on lazy contracts@~citep{hjl-flops-2006,df-toplas-2011,c-icfp-2012,dtw-pepm-2012};
 for instance, by delaying base-type checks until a computation depends on the value.

@; % - conjecture : others satisfy CM
@; % - note big implementation cost for new wrappers
@; % - note big cognitive cost of changing semantics
@; % - note big compiler (optimization) cost of changing semantics
@; % - tgpk-dls-2018 suggests that programmers want eager checks everywhere, but wonder if blame errors would be acceptable


@section[#:tag "sec:design:strat:forgetful"]{@|fname|}

The goal of @|fname| is to limit the number of wrappers around
 a value.
A key non-goal is to enforce types as compositionally-valid claims about code.
Typed code can rely on the static types that it declares, but nothing more.
Untyped code cannot trust type annotations because those
 types might never be checked.

The @|fname| strategy is to keep at most two wrappers around a value.
An untyped value gets one wrapper when it enters a typed
 context and loses this wrapper upon exit.
A typed value gets a ``sticky'' inner wrapper the first time
 it exits typed code and gains a ``temporary'' outer wrapper whenever
 it re-enters a typed context.
@; %%Each temporary wrapper protects a typed client from a value.
@; %%Each inner wrapper protects a higher-order, typed value against future clients.
@; %% TODO illustrate for (maybe) pairs and (definitely) functions
@; %%  ... pairs are the difficult ones to motivate, gotta think inside

@exact|{
\formatapproach{@|fname|}{
  check that $\svalue$ is an integer
}{
  check that $\svalue$ is a tuple and wrap $\svalue$ to
   validate projections
}{
  check that $\svalue$ is a function and wrap $\svalue$ to
   protect higher-order inputs and validate outputs
}{
  check nothing
}{
  if $\svalue$ has a wrapper, discard it;
   otherwise wrap $\svalue$ to protect projections
}{
  if $\svalue$ has a wrapper, discard it;
   otherwise wrap $\svalue$ to validate inputs and protect higher-order outputs
}
}|


@subsection{\forigins{@|fname|}}

@citet{g-popl-2015} introduces forgetful manifest contracts and proves
 type soundness; the extended version of the paper contains a detailed
 discussion, including the observation that forgetful types cannot support
 abstraction and information hiding (@format-url{https://arxiv.org/abs/1410.2813}).
\citet{cl-icfp-2017} present a forgetful and type sound semantics for a
 mixed-typed language.

By contrast to @|fname|, there are other strategies that limit the number of
 wrappers on a value without sacficing type guarantees@~citep{htf-hosc-2010,g-popl-2015,stw-pldi-2015}.
These methods require a protocol for merging wrappers, whereas
 @|fname| is a simple and type-sound way to save time and space.


@section[#:tag "sec:design:strat:transient"]{@|tname|}

The @|tname| method ensures that typed code
 does not ``go wrong''@~citep{m-jcss-1978} in the sense of applying a
 primitive operation to a value outside its domain.
Every application @${(\sexpr_0~\sexpr_1)} in typed code can, for example,
 trust that the value of @${\sexpr_0} is a function.

@|tname| meets this goal without the use of wrappers or deep checks.
Instead, it rewrites typed code to pre-emptively check the shape of values.
Every type boundary, every typed elimination form, and every typed
 function gets protected with a shape check;
 these checks validate the output of boundaries and elimination forms,
 and the input to typed functions.

As the name suggests, a shape check matches the top-level constructor of a
 value against the top-level constructor of a type.
The following table describes the checks that happen at a boundary;
 the extra checks in typed code perform dynamic-to-static checks.

@exact|{
\formatapproach{@|tname|}{
  check that $\svalue$ is an integer
}{
  check that $\svalue$ is a pair
}{
  check that $\svalue$ is a function
}{
  check nothing
}{
  check nothing
}{
  check nothing
  %% (typed functions must validate all inputs with a shape check)
}
}|

A more expressive language of types, however, may require deeper shape checks.
For instance, a union-type check must consider each alternative in the union.
@citet{gf-icfp-2018} supply additional comments and performance data regarding an
 implementation of @|tname| for the Typed Racket surface language.

In general, @|tname| checks add up to a greater number of run-time validation
 points than those that arise in a wrapper-based semantics, but their overall cost may be
 lower and more predictable.
Static analysis can reduce the number of checks@~citep{vsc-dls-2019}.


@subsection{\forigins{@|tname|}}

@citet{v-thesis-2019} invented @|tname|.
The method was introduced for Python@~citep{vksb-dls-2014,vss-popl-2017},
 has been adapted to Typed Racket@~citep{gf-icfp-2018}
 and has inspired a closely-related approach for Grace@~citep{rmhn-ecoop-2019}.


@section[#:tag "sec:design:strat:amnesic"]{@|aname|}

The goal of the @|aname| semantics is to provide the same behavior as
 @|tname| but improve the error messages when a type mismatch occurs.
@|aname| demonstrates that wrappers offer more than a way to detect errors;
 they seem essential for informative errors.

The @|aname| strategy consists of three steps:
 it wraps values,
 it discards all but three wrappers,
 and it keeps a record of discarded boundary specifications.
When a type mismatch occurs, @|aname| presents the recorded boundaries
 to the programmer.

@|aname| employs both guard wrappers and @emph{trace} wrappers.
In the model, a trace wrapper records
 a list of boundaries that a value has previously crossed.
If an untyped function enters a typed component, @|aname| wraps the
 function in a guard.
If the function travels back to untyped code, @|aname| replaces the
 guard with a trace wrapper that records two boundaries.
Future round-trips extend the trace.
Conversely, a typed function that flows to untyped code and back @${N{+}1} times gets
 three wrappers: an outer guard to protect its
 current typed client, a middle trace to record its last @${N} trips, and an
 inner guard to protect its body.
@; %% TODO illustrate?

@exact|{
\formatapproach{@|aname|}{
  check that $\svalue$ is an integer
}{
  check that $\svalue$ is a tuple and wrap $\svalue$ to
   check projections
}{
  check that $\svalue$ is a function and wrap $\svalue$ to
   protect higher-order inputs and validate outputs
}{
  check nothing
}{
  if $\svalue$ has a guard wrapper, remove and record it;
   otherwise wrap $\svalue$
}{
  if $\svalue$ has a guard wrapper, remove and record it;
   otherwise wrap $\svalue$
}
}|


@subsection{\forigins{@|aname|}}

@|aname| is a synthesis of @|fname| and @|tname| that
 demonstrates how our framework can guide the design of new strategies@~citep{gfd-oopsla-2019}.
@; %% contributes a novel technique for theoretically-useful error messages


@section[#:tag "sec:design:strat:erasure"]{@|ename|}

The @|ename| approach is based on a view of types as an optional syntactic artifact.
Type annotations are a structured form of comment that help developers and
 tools read a codebase.
At runtime, types are meaningless.
Any value may flow into any context:

@exact|{
\formatapproach{@|ename|}{
  check nothing
}{
  check nothing
}{
  check nothing
}{
  check nothing
}{
  check nothing
}{
  check nothing
}
}|

Despite the complete lack of type enforcement, the @|ename| strategy is
 widely used (@figure-ref{fig:landscape}) and has a number of pragmatic benefits.
The static type checker can point out logical errors in type-annotated code.
An IDE may use the static types in auto-completion and refactoring tools.
An implementation does not require any instrumentation to enforce types.
Users that are familiar with the host language do not need to learn
 a new semantics to understand the behavior of type-annotated programs.
Finally, @|ename| runs equally fast as a host-language program.


@subsection{\forigins{@|ename|}}

@|ename| is also known as @emph{optional typing} and dates back to the type hints
 of MACLISP@~citep{m-maclisp-1974} and Common Lisp@~citep{s-lisp-1990}.
StrongTalk is another early and influential optionally-typed language@~citep{bg-oopsla-1993}.
Models of optional typing exist for JavaScript@~citep{bat-ecoop-2014,cvgrl-oopsla-2017},
 Lua@~citep{mmi-dls-2015},
 and Clojure@~citep{bdt-esop-2016}.



