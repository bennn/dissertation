#lang greenman-thesis/include

@(require
   racket/format
   (only-in greenman-thesis/oopsla-2019/pict
     untyped-codeblock))

@(define-logger bg-thesis)

@(define (Section-ref tag)
   (list "Section" ~ (secref tag)))

@(define (section-ref tag)
   (list "section" ~ (secref tag)))

@(define mix-principle* '(
   new-tool-for-new-code
   reuse-old-code
   can-improve-old-code
))

@(define migratory-principle* '(
   reject-ill-typed-code
   explicit-annotations
   only-add-types
   no-catchall
   clear-boundary
))

@(define mt-principle* (append mix-principle* migratory-principle*))

@(define num-mt-principle (length mt-principle*))

@(define-values [define-mt-principle assert-mt-principle]
   (let ([undefined-mt* (box migratory-principle*)])
     (values
       (lambda (p-id . title*)
         (set-box! undefined-mt* (remove p-id (unbox undefined-mt*)))
         (apply subsection title*))
       (lambda ()
         (unless (null? (unbox undefined-mt*))
           (log-bg-thesis-error "missing definition for MT principles: ~s" (unbox undefined-mt*)))))))

@(define (mt-observation . str*)
   (apply section (cons "Observation: " str*)))

@(define (mt-requirement . str*)
   (apply section (cons "Requirement: " str*)))

@; -----------------------------------------------------------------------------

@; --- MF
@; So I have read it a couple of times.
@; 
@; — I think all the thoughts are there that you need for the rest of the
@; dissertation and perhaps a few more.
@; — They are presented with occasional lapses in category. (This is a
@; so-called categorical mistake.)
@; — Your style is quite uneven in many different ways (for example,
@; inspect
@; “I” vs “we” or look for “inanimate things that do”).
@; — The biggest technical problem is your discussion of FFIs. I think it
@; doesn’t come across what you want.

@title[#:tag "chap:why"]{Migratory Typing}
@; whence MT

Migratory typing is a novel approach to an old desire:
 mixing typed and untyped code.
A typed programming language comes with a strict meta-language (of types)
 that articulates how a program computes.
For better or worse, code that does not fit the meta-language may not run.
An untyped language is willing to see what happens in many more programs,
 so long as the computations stick to legal values.
@; TODO cite Hoare hints, "cherished"?
The mixed-typed idea is to somehow combine some good aspects of both.
A programmer should have some untyped flexibility and some typed guarantees.

Of course, flexibility and guarantees lie at two ends of a tradeoff.
More freedom to run programs means less knowledge about what a new
 program might do, unless there are run-time checks to catch extreme behaviors.
Run-time checks slow down a computation, thus a mixed-typed language needs to
 balance three desires: expressiveness, guarantees, and performance.

Before a language can address the central 3-way tradeoff, its designers
 must decide what kinds of mixing to allow and what goals to strive for.
Migratory typing is one such philosophy.
The goal of migratory typing is to add static typing onto a vetted untyped
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

In the days before migratory typing, language designs explored several ways
 to mix typed and untyped code.
Some mixtures began with an untyped language and considered
 whether to demand user-supplied type annotations.
Others began with a typed language and added untyped flexibility.
Either way, each design had to decide on run-time guarantees for its
 generalized types.


@subsection{Type Hints}
@; lesson = annotations may work, pitfalls of unsoundness

Early Lisps, including MACLISP@~cite{m-maclisp-1974} and Common Lisp@~cite{s-lisp-1990},
 have compilers that accept type hints.
In MACLISP, for example, a programmer can hint that a function expects two
 floating-point numbers to encourage the compiler to specialize the function
 body (@figure-ref{fig:maclisp-hint}).

Any speedup due to type hints, however, comes at a risk.
There is no static type system to prove that hints are sensible claims.
If a hint is nonsense, then the compiled code may behave in unexpected ways.
Similarly, there is no dynamic guarantee that compiled code receives valid
 inputs.
If the function @tt{F} in @figure-ref{fig:maclisp-hint} is invoked on two
 strings, it may compute an invalid result.
In other words, type hints come with all the unsafety of casts in a C-like language.

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
Soft inference adds slack variables to types and turns the
 inequalities into equalities@~cite{f-thesis-1992}.
Set-based inference solves the inequalities by computing a transitive
 closure over the entire program@~cite{am-popl-1991,awl-popl-1994,f-thesis-1997,ff-pldi-1997,ffkwf-pldi-1996}.
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
 brittleness and readability, despite friction with the soft typing philosophy.
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
The type checker is the static analysis; it uses types to find logical errors.
Compilation erases types to arrive at an untyped program that can safely
 interoperate with the rest of the program.

Despite their widespread adoption (@section-ref{sec:design:landscape}),
 optional types are a badge of shame for the research community because
 these types are unsound.
A programmer cannot use optional types to predict the inputs that a function
 will receive, and likewise a compiler cannot use optional types to justify
 transformations.
Unless researchers can design a practical and non-optional mixed-typed language,
 then work on sound types will remain stuck in a closed world.


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
 and has inspired a large body of static-to-dynamic research (@section-ref{sec:related:gradual}).

Implicit coercions to type dynamic, however, weaken type-proofs in a
 gradual or quasi-static language.
Rather than showing that components do fit together, a gradually-typed
 program is something that can fit together given good values at each occurrence
 of the dynamic type.
Words like ``plausibility''@~cite{t-popl-1990}
 and ``consistency''@~cite{st-sfp-2006} aptly describe the weakened guarantees;
 gradual types can only point out implausibilities and inconsistencies among
 non-dynamic types.


@section[#:tag "sec:why:observations"]{MT: Observations}

Migratory typing stands on three axioms of programming:
 untyped code exists,
 type annotations improve maintainability,
 and sound types are a worthwhile ideal.
On the surface, these basic observations motivate a typed/untyped mix.
Between the lines, they suggest requirements for all mixed-typed languages.


@subsection{MT-O1: untyped code exists}
@; - exists, quantity
@; - want to reuse no questions asked, no costly migration




The world is full of good untyped code.
Many prolific languages on GitHub are untyped.
These languages are home to useful, unique libraries.

Exactly how this code came to be is unimportant.
The fact is, untyped code exists and programmers must be able to use it
 as-is to continue being productive.

@;If untyped code exists and types offer benefits, then clearly the ideal
@; language must stitch together typed and untyped code.
@;A careless stitching, however, may be worst of all.
@;Indeed, the boundaries between typed and untyped code are key to the success of
@; the whole idea.
@;
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
@;
@; A language needs to take care at boundaries, both to protect against
@;  miscommunications and to leverage new strengths.


@subsection{MT-O2: types communicate}
@; - types catch typos
@; - type annotations keep intent, checked documentation
@; - dialog to compiler / runtime

Type annotations are an important channel of communication.
For human readers, they describe the high-level design of code.
Even the original author of a function can benefit from reading the types
 after some time away from the codebase.
For a compiler, annotations are expectations.
In a full-featured type system with subtyping and other points of ambiguity,
 user-supplied annotations drive choices.
Moreover, type errors that can point to part of an annotation have a direct
 link to the programmer who needs to deal with the errors.

These benefits offset the costs of writing and maintaining types.
Languages that can help write types are better off, of course,
 but types belong in source code.
At least for top-level and recursive definitions.


@subsection{MT-O3: sound types matter}
@; - sound types help programmers
@; - sound types help compilers
@; - unless academics try, nobody will




@section[#:tag "sec:why:decisions"]{MT: Design Choices}

@; above hits and misses => lessons ...
@; - inference bad,
@; - annotation work
@; - ....

Characterizing aspects of MT, what sets it apart.

The goal of migratory typing is to begin with an untyped language and
 incrementally add benefits of static types.
My work is based on additional design constraints intended to maximize the
 benefits of the typed/untyped combination.


@subsection{MT-R1: sound types}

Well-typed code must satisfy a non-trivial soundness guarantee.
Static types must predict some aspect of the values that flow though
 a program, beyond the baseline guarantee that every value is well-formed.

A typical soundness theorem helps programmers debug and helps compilers to
 generate efficient code.
MT soundness must provide similar benefits, but perhaps to a lesser extent
 if the cost of enforcing ``typical'' soundness is overwhelming.

@section{Blame}
@; TODO

What, why, how.



@subsection{MT-R2: descriptive types}

The language of static types must be able to describe idioms from the untyped
 code.
Untyped code comes first.
Types must be versatile enough to explain the reasoning behind a well-reasoned
 piece of untyped code without any sort of rewrites.
ML-style tagging and untagging is not acceptable.


@subsection{MT-R3: user-supplied annotations}

Programmers must write type annotations.
These annotations serve three purposes.
First, they establish the intended level of detail.
Descriptive types offer a choice between several valid types for one expression.
Instead of asking the language to pick a level via type inference, which may
 not be feasible, we ask the programmer.
Second, they help the type checker reject code.
Types are supposed to help find bugs, but flexible type inference may miss
 errors due to a lack of guidance.
Third, explicit types help human readers navigate a codebase.


@subsection{MT-R4: clear typed/untyped boundaries}

Every boundary between typed and untyped code is a potential channel
 for a communication error.
If such an error occurs, the programmer needs to find the relevant channels.
The language should help with this debugging challenge.
In order to help, though, boundaries must be clearly stated in source code.
Programmers need to know where these boundaries are as they write the program,
 and the language needs to track them when it runs.

@; withhout, undermine benefits


@subsection{Micro, Macro}
@; TODO

The two original papers on gradual/migratory typing begin with different
 ideas about how to mix typed and untyped code.

@citet{st-sfp-2006} propose a @emph{micro}-level mixing in the spirit
 of type dynamic@~cite{acpp-toplas-1991,lm-fpca-1991} and quasi-static
 typing@~cite{t-popl-1990}.
The grammar of static types acquires a new catch-all member that accepts
 any well-formed piece of code.
Using this @emph{dynamic type}, a programmer can escape the type checker
 on any line of otherwise-typed code.
To allow such flexibility, however, the type checker can no longer affirm
 that a program is type-correct.
Programs that use the dynamic type are but plausibly correct; depending
 on what the dyn-typed code evaluates to, the program may run smoothly.

@citet{tf-dls-2006} propose a @emph{macro}-level mixing.


