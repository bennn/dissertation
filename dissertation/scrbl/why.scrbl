#lang greenman-thesis/include

@(require
   racket/format
   (only-in greenman-thesis/oopsla-2019/pict
     untyped-codeblock))

@; -----------------------------------------------------------------------------

@title[#:tag "chap:why"]{Migratory Typing}

Migratory typing is a novel approach to an old desire:
 mixing typed and untyped code.
A typed programming language comes with a strict sub-language (of types)
 that articulates what a program computes.
For better or worse, code that does not fit the sub-language may not run.
An untyped language runs any program in which the primitive computations
 stick to legal values.
The mixed-typed idea is to somehow combine some good aspects of both.
A programmer should have some untyped flexibility and some typed guarantees.

Of course, flexibility and guarantees lie at two ends of a tradeoff.
More freedom to run programs means less knowledge about what a new
 program might do, unless there are run-time checks to catch extreme behaviors.
Run-time checks slow down a computation, thus a mixed-typed language needs to
 balance three desires: expressiveness, guarantees, and performance.

Before a language design can address the central 3-way tradeoff, its creators
 must decide what kinds of mixing to allow and what goals to strive for.
Migratory typing is one such theory.
The goal is to add static typing onto an independent untyped
 language.
@;Code that started off untyped can gain the maintenance benefits of
@; static types---ideally with no need to rewrite program logic.
Programmers create a mixed-typed program by writing types for
 one chunk of untyped code; that is, by migrating the chunk into the typed
 half of the language.
Both the goal and the method incorporate lessons from earlier mixed-typed efforts
 (@section-ref{sec:why:related}), along with basic
 observations about programming (@section-ref{sec:why:observations}).
The observations, in particular, motivate design choices that characterize
 migratory typing (@section-ref{sec:why:decisions}).
@; and provide central focus for dissertation

@; Keep in mind, however, that the research community is still looking
@;  for knobs to make turnable.
@; This chapter is the world as I see it as of this moment.
@; Looking for timeless truths, move along.
@; .... a theory


@section[#:tag "sec:why:related"]{Pre-MT: Hits and Misses}
@; TODO what did Henglein do and how does it fit???

In the days before migratory typing, language designers explored several ways
 to mix typed and untyped code.
Some mixtures began with an untyped language and allowed
 user-supplied type annotations.
Others began with a typed language and added untyped flexibility.
Either way, each design had to decide on run-time guarantees for its
 generalized types.


@subsection{Type Hints}
@; lesson = annotations may work, pitfalls of unsoundness

Early Lisps, including MACLISP@~cite{m-maclisp-1974} and Common Lisp@~cite{s-lisp-1990},
 have compilers that accept type hints.
In MACLISP, for example, a programmer can hint that a function expects two
 floating-point numbers and returns one to encourage the compiler to specialize
 the function body (@figure-ref{fig:maclisp-hint}).

Any speedup due to type hints, however, comes at a risk.
There is no static type system to prove that hints are sensible claims.
If a hint is nonsense, then the compiled code may behave in unexpected ways.
Similarly, there is no dynamic guarantee that compiled code receives valid
 inputs.
If the function @tt{F} in @figure-ref{fig:maclisp-hint} is invoked on two
 strings, it may compute an invalid result.
In other words, type hints come with all the perils of types in a C-like language.

@figure*[
  "fig:maclisp-hint"
  @elem{Example type hint in MACLISP@~cite{p-maclisp-1983}.
   The compiler may rewrite @tt{PLUS} into code that assumes floating-point inputs.}
  (untyped-codeblock '(
    "(DECLARE (FLONUM (F FLONUM FLONUM)))"
    "(DEFUN (F A B) (PLUS A B))"
  ))]


@subsection{Soft and Set-Based Inference}
@; lesson = full inference bad idea

In principle, type inference can bring static types to untyped code.
Research on soft typing pursues this goal in an ideal form
 by constructing types for any untyped program.
Soft type systems never raise a type error.
Instead, a soft type checker widens types as needed
 and inserts run-time checks to protect implicit down-casts@~cite{f-thesis-1992,w-thesis-1994}.

The key to the soft typing problem is to adapt inference from equalities
 to inequalities@~cite{tfffgksst-snapl-2017}.
In a language such as ML, a type describes exactly how a variable
 may be used.
Any out-of-bounds use is an error by definition.
Thus ML inference asks for a solution to a system of equalities between
 variables and types.
Inference for an untyped language must relax the equality assumption
 to deal with the less-structured design of untyped programs.
Here, the natural types describe sets of values with compatible behavior.
The inference problem now asks for types that over-approximate the
 behaviors in a set of values.

There are two known methods to solve type inequalities.
Soft inference adds slack variables to types, turns the
 inequalities into equalities, and then uses Hindley-Milner style inference@~cite{f-thesis-1992}.
Set-based inference solves the inequalities by computing a transitive
 closure through constructors over the entire program@~cite{am-popl-1991,awl-popl-1994,f-thesis-1997,ff-pldi-1997,ffkwf-pldi-1996}.
Both solutions, unfortunately, reveal major challenges for inference:
@exact{
\begin{itemize}
\item
  Types can quickly become unreadable as inference computes supersets based on
   the syntax of a program.
  Worse, small changes to a program can end in large changes to inferred types.
\item
  Type structure depends on the whole program.
  Set-based analysis, in particular, faces serious performance issues
   in larger programs@~cite{mfsw-hosc-2005}.
\end{itemize}}

@citet{w-thesis-1994} notes that user-provided annotations can help with
 brittleness and readability, despite friction with the tenets of soft typing.
@citet{m-thesis-2006} improves the performance of set-based analysis
 by leveraging contracts at module boundaries.
Their observations anticipate the migratory typing approach to mixed-typed code.


@subsection{Optional Typing}
@; lesson = annotations work ... possible but lacks ideal

An optional, or pluggable, type system adds a static analysis to an untyped
 language@~cite{bg-oopsla-1993}.
The approach is related to type hints in that programmers must add
 annotations to untyped code.
Optional types are supported, however, by a full-fledged type checker
 and a no-op compiler.
The type checker is the static analysis; it uses types to find basic logical errors.
Compilation erases types to arrive at an untyped program that can safely
 interoperate with the rest of the program.

Despite their widespread adoption (@section-ref{sec:design:landscape}),
 optional types are a badge of shame for the research community because
 these types are unsound.
A programmer cannot use optional types to predict the inputs that a function
 will receive, and likewise a compiler cannot use optional types to justify
 transformations.

@emph{History Note:}
To be fair, optional typing is one valid way to use Lisp type hints.
A Lisp compiler need not optimize based on type hints, and it may even ignore
 types completely@~cite{m-maclisp-1974,s-lisp-1990}.
@citet{bg-oopsla-1993} deserve credit not for inventing the optional style,
 but for explaining why it is a practical mode of use.


@subsection{Type Dynamic}
@; lesson = non-lesson? ... stat->dyn can be done, brings pitfalls

Statically-typed languages often need to interact with untyped values,
 perhaps though a database connection, web socket, or interactive prompt.
Both @citet{acpp-toplas-1991} and @citet{lm-fpca-1991} thus present static
 type systems with a special dynamic type.
Typed code can interact with a untyped value by first testing
 its structure; the type system records observations.

Quasi-static typing extends the type-dynamic idea with implicit structure
 tests@~cite{t-popl-1990}.
Instead of asking the user to write and maintain type-testing code, the
 quasi-static system generates run-time checks.
Consequently, programmers have less incentive to handle the dynamic type
 at the boundary to untrusted code.
The result is a mixed-typed language because entire blocks of code may have
 the dynamic type throughout.
Gradual typing emphasizes the mixed-typed idea in quasi-static typing,
 contributes major technical improvements and design discipline@~cite{st-sfp-2006,svcb-snapl-2015},
 and has inspired a large body of static-to-dynamic research (@format-url{https://github.com/samth/gradual-typing-bib}).

Implicit coercions to type dynamic, however, weaken type-proofs in a
 gradual or quasi-static language.
Rather than showing that components @emph{do} fit together, a gradually-typed
 program is something that @emph{can} fit together given good values at each occurrence
 of the dynamic type.
Words such as ``plausibility''@~cite{t-popl-1990}
 and ``consistency''@~cite{st-sfp-2006} aptly describe the weakened guarantees;
 gradual types can only point out implausibilities and inconsistencies among
 non-dynamic types.


@section[#:tag "sec:why:observations"]{MT: Observations}

Migratory typing stands on three observations:
 untyped code exists,
 type annotations improve maintainability,
 and sound types are a worthwhile ideal.
On the surface, these basic opinions simply motivate a typed/untyped mix;
 between the lines, however,
 they suggest requirements for an effective mixed-typed language.


@subsection[#:tag "why:mt-o1"]{MT-o1: untyped code exists}
@; - exists, quantity
@; - want to reuse no questions asked, no costly migration

Untyped code is a fact.
Large companies such as Dropbox, Facebook, and Twitter started as
 untyped projects.
Small teams continue to employ untyped languages; indeed, most repositories on
 GitHub use either JavaScript, Python, PHP, or Ruby (@format-url{https://githut.info}).

Once an untyped codebase is off the ground and the lack of reliable type
 information becomes a maintenance bottleneck, programmers have two options.
The extreme option is to change languages.
Twitter, for example, was able to port its Ruby codebase over to Scala@~cite{twitter-scala}.
For teams that lack the time and expertise to make such a switch, the alternative
 is to re-create any necessary benefits of types.
An exemplar of the second option is Sweden's pension system, which
 depends on a contract-laden Perl program@~cite{sweden-pluto}.
The contracts ensure that components in this huge program behave as intended.

Research on mixed-typed languages can be a great help to teams in this second
 camp, that cannot afford to rewrite their codebase.
General knowledge about how to design a companion type system can reduce
 the development cost of an in-house solution like Sweden's contracts.
And a tailor-made type system, if one exists, provides an immediate solution
 to maintenance issues.

@;To see the pitfalls of loose coupling, consider a standard foreign-function
@; interface (FFI) that translates values in one language into values of
@; another.
@;Suppose we have an FFI between an untyped language and a sound typed language.
@;The combined language has two major problems.
@;First, untyped code can supply any input to typed code, breaking soundness.
@;Second, the FFI invites untyped-to-typed conversion but requires wholesale
@; rewrites, risking mistakes.
@;
@;By contrast, a mixed-typed language that uses the same base syntax for typed
@; and untyped code offers more than the sum of two languages.
@;Programmers can move from untyped to typed by adding annotations, nothing
@; more; the conversion does not risk behavior-changing mistakes.
@;Untyped programmers may be more motivated to learn these types than a
@; new language.
@;Debugging in untyped code can gradually strengthen it with new type annotations.


@subsection[#:tag "why:mt-o2"]{MT-o2: types communicate}
@; - types catch typos
@; - type annotations keep intent, checked documentation
@; - dialog to compiler / runtime

Type annotations are an important channel of communication.
For human readers, they describe the high-level design of code.
Even the original author of a function can benefit from reading the types
 after some time away from the codebase.
For a compiler, annotations are hints about what the programmer expects.
In languages that lack principal types, user-supplied annotations can resolve
 ambiguity.
Additionally, any type error messages that can point to part of an annotation
 have a direct link to the programmer who needs to deal with the errors.


@subsection[#:tag "why:mt-o3"]{MT-o3: sound types catch bugs}
@; - sound types help programmers
@; - sound types help compilers
@; - unless academics try, nobody will

All static types can find typo-level mistakes, but only sound types
 guarantee type-specified behavior.
In a mixed-typed setting, a guarantee can make a world of difference.
Picture a large untyped codebase made up of several interacting components,
 and suppose that one component behaves strangely.
Adding unsound types to that one component can reveal a syntactic mistake, but nothing more.
Sound types, on the other hand, will halt the program as soon as an incorrect
 value appears in typed code.
If the language can additionally report the source of the untyped value
 and the rationale for the mismatched type expectation, then the programmer
 has two clues about where to begin debugging.

@; TODO really the type system monitors ... not that type is dependable
Going beyond soundness, a mixed-typed language that satisfies complete
 monitoring guarantees the run-time behavior of every type.
If a value flows across a type-annotated source position, then future users
 of the value can depend on the type---no matter whether these uses are in
 statically-typed or untyped code (@chapter-ref{chap:design}).
In other words, silent disagreements between a type and value cannot arise.
Every mismatch stops the program before computations can become further derailed.


@section[#:tag "sec:why:decisions"]{MT: Design Choices}

@; above hits and misses => lessons ...
@; - inference bad,
@; - annotation work
@; - ....

Taken broadly, migratory typing studies how to add types to untyped code.
My dissertation builds on a more focused theory that is grounded in the
 following principles.
Typed Racket shares the same theory@~cite{t-thesis-2010,tfffgksst-snapl-2017}.
Our aim is to maximize the potential benefits of a mixed-typed language,
 in spite of the risk that some goals may prove unattainable.
As always, research is when it can fail.

@subsection[#:tag "why:mt-r1"]{MT-r1: types for untyped code}

Migratory typing begins with an independent untyped language and adds
 a companion type system.
The new types and type system must express common idioms from the untyped world;
 in other words, a type system that demands a re-organizion of untyped code
 is not acceptable.


@subsection[#:tag "why:mt-r2"]{MT-r2: require annotations, reject programs}

Programmers must write type annotations for top-level and recursive
 definitions.
Extra annotations may guide type inference.

The type checker will reject ill-typed programs instead of creating a
 run-time cast to bridge unequal types.
Programmers must deal with the type errors, either by inserting a cast
 or re-designing code.


@subsection[#:tag "why:mt-r3"]{MT-r3: sound types}

Well-typed code must satisfy a non-trivial soundness guarantee.
Both @|sdeep| and @|sshallow| types are acceptable, but nothing less.


@subsubsection{Blame}

Sound types catch bugs, but make no claims about actionable error outputs.
Blame is an additional property geared toward useful errors
 that tell a programmer where to begin debugging.
To this end, a mixed-typed language should try to present relevant source
 locations along with every run-time type mismatch error.


@subsection[#:tag "why:mt-r4"]{MT-r4: clear boundaries}

Typed and untyped code must be linked at static and clearly visible API boundaries.
In order for a typed module to interact with an untyped value, the module
 must declare a type specification for the value.
An untyped module does not need to give specifications because any
 typed value that it imports comes with a static specification for correct use.
@; TODO also we don't want to touch

By contrast, this dissertation is not directly concerned with true gradual
 languages that include a dynamic type@~cite{svcb-snapl-2015}.
Such languages can still benefit from my results at an intermediate step,
 after occurrences of the dynamic type have been replaced with precise types
 and casts.
But it is unclear how to communicate intermediate behaviors up to the
 programmer.

Requiring boundaries greatly simplifies and strengthens the type system.
It is simpler because there is no dynamic type; standard definitions of
 types, subtyping, and all the rest suffice for the type checker.
It is stronger because there is no type precision relation to allow
 odd constructions in otherwise-typed code.
Untyped code can only sneak in through a boundary;
 refer to @section-ref{sec:transient:blame-performance} for a contrary
 example in Reticulated Python.


@subsubsection{Macro, Micro}

Prior works make a distinction between @emph{macro}-level and @emph{micro}-level
 gradual typing systems@~cite{tfdfftf-ecoop-2015,tfgnvf-popl-2016}.
These names express the same idea as my boundary requirement,
 but in terms of granularity and with the term ``gradual typing'' broadly
 construed to refer to any sound mixed-typed language.
Macro allows interaction between typed and untyped chunks of code@~cite{tf-dls-2006}
 whereas micro allows ``fine-grained'' mixing via a dynamic type@~cite{st-sfp-2006}.

Looking back, I think there were two dimensions at play.
First is whether to include a dynamic type.
Second is how to mix: whether to migrate from an untyped host language
 or to add flexibility to a static type system@~cite{g-snapl-2019}.
Micro/macro is a useful mnemonic for the first dimension,
 but it is more direct to talk about dynamic/non-dynamic and migratory/non-migratory
 as two choices in the design of a new mixed-typed language.


@section[#:tag "sec:why:history"]{Recent History}

@citet{t-thesis-2010} developed migratory typing alongside Typed Racket.
The basic ideas arose from work on soft typing@~cite{f-thesis-1992,w-thesis-1994},
 higher-order contracts@~cite{f-thesis-2002},
 and modular set-based analysis@~cite{m-thesis-2006}.
Subsequent work adapted migratory typing to
 multi-paradigm language features:
 compositional flow-based reasoning@~cite{tf-icfp-2010},
 delimited continuations@~cite{tst-esop-2013},
 variable-arity polymorphism@~cite{stf-esop-2009},
 type-driven optimization@~cite{s-thesis-2015},
 first-class classes@~cite{t-thesis-2016},
 units (first-class modules)@~cite{f-thesis-2015},
 and refinement types@~cite{k-thesis-2019}.
Refer to @citet{tfffgksst-snapl-2017} for a ten-year retrospective.
My dissertation adds one step to this lineage.
I began by studying the most pressing challenge, performance costs, and
 arrived at the combination of @|sdeep| and @|sshallow| types.


