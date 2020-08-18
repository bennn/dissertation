#lang greenman-thesis/include

@(require racket/format)

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

Migratory typing is a new refinement of an old idea: mixing
 typed and untyped code.
A typed programming language comes with a strict meta-language (of types)
 that articulates how a program computes.
For better or worse, code that does not fit the meta-language may not run.
An untyped language is willing to see what happens in many more programs,
 so long as the computations stick to legal values.
@; TODO cite Hoare hints, "cherished"?
The mixed-typed idea is to somehow combine good aspects of both.
A programmer should have some untyped flexibility and some typed guarantees.

Of course, flexibility and guarantees lie at two ends of a tradeoff.
More freedom to run programs leads to fewer guarantees about what a new
 program will do, unless there are run-time checks to catch extreme behaviors.
Thus a mixed-typed language needs to balance three desires: expressiveness,
 guarantees, and performance.
But even before this 3-way tradeoff, a language has to decide what kinds
 of mixing to allow and what goals it hopes to achieve.

The goal of migratory typing is to make static typing available in
 an untyped language so that stable programs can gain the maintenance
 benefits of types.
Programmers create a mixed-typed program by writing types for
 one chunk of untyped code; that is, by migrating the chunk into the typed
 half of the language.
Both the goal and the method are a departure from prior mixed-typed efforts
 (@section-ref{sec:why:related}), but they are grounded in observations
 about programming (@section-ref{sec:why:observations})
 and 10+ years of experience with Typed Racket suggests that migratory
 typing is a major advance.
This dissertation contributes a novel way to balance
 expressiveness, guarantees, and performance (@section-ref{sec:why:design}).


@section[#:tag "sec:why:related"]{Pre-History}
@; NOTE we are still figuring out the key knobs + concepts

Beyond migratory typing, there are several ways to mix typed and untyped
 code.
The main competing alternatives are optional typing and gradual typing.


@subsection{Optional Typing}

An optional typing system provides types without soundness.
Unsound types are still useful.
Many languages follow this design.

My goal is soundness.
Benefits listed above.
If academics do not pursue, then nobody will.


@subsection{Gradual Typing}

Gradual typing is not necessarily migratory,
 non-migratory is addressing a dubious problem.

Includes a special dynamic type.
The type makes it easy to add types to code, no insistence on detail.
This makes it harder to detect static type errors ... but not much worse
 than free use of a top type.
More importantly, the dynamic type obscures the boundaries between typed
 and untyped code.
@; easy to get many boundaries, hard to point to these


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




@section[#:tag "sec:why:observations"]{MT Observations}

Migratory typing stems from three observations about the practice of
 programming.
At face value, these basic facts show a need for mixing typed and untyped code.
Between the lines, details suggest extra requirements.


@subsection{Observation 1: untyped code exists}

@; where is the untyped code?
@; why is it good?
@; 

The world is full of good untyped code.
Many prolific languages on GitHub are untyped.
These languages are home to useful, unique libraries.

Exactly how this code came to be is unimportant.
The fact is, untyped code exists and programmers must be able to use it
 as-is to continue being productive.


@subsection{Observation 2: types offer benefits}

@; ps writing may be harder, don't know. Once written, clear benefits.

Typed code has several advantages over untyped code.
The static type checker provides assurance against simple logical mistakes.
Any explicit type annotations communicate the intent of the original author.
All types on an interface serve as a usage guide to future clients.

Sound types come with additional benefits because they predict behaviors.
Clients can trust sound types as they design new code.
These types can speed up debugging tasks because they guarantee certain errors
 cannot occur.
A compiler can use the type predictions to generate efficient run-time code
 from easy-to-read sources.


@subsection{Observation 3: boundaries matter}

If untyped code exists and types offer benefits, then clearly the ideal
 language must stitch together typed and untyped code.
A careless stitching, however, may be worst of all.
Indeed, the boundaries between typed and untyped code are key to the success of
 the whole idea.

To see the pitfalls of loose coupling, consider a standard foreign-function
 interface (FFI) that translates values in one language into values of
 another.
Suppose we have an FFI between an untyped language and a sound typed language.
The combined language has two major problems.
First, untyped code can supply any input to typed code, breaking soundness.
Second, the FFI invites untyped-to-typed conversion but requires wholesale
 rewrites, risking mistakes.

@; TODO anticipate need for subtyping

By contrast, a mixed-typed language that uses the same base syntax for typed
 and untyped code offers more than the sum of two languages.
Programmers can move from untyped to typed by adding annotations, nothing
 more; the conversion does not risk behavior-changing mistakes.
Untyped programmers may be more motivated to learn these types than a
 new language.
Debugging in untyped code can gradually strengthen it with new type annotations.

@; A language needs to take care at boundaries, both to protect against
@;  miscommunications and to leverage new strengths.


@section{MT Design Choices}

The goal of migratory typing is to begin with an untyped language and
 incrementally add benefits of static types.
My work is based on additional design constraints intended to maximize the
 benefits of the typed/untyped combination.


@subsection{Requirement 1: sound types}

Well-typed code must satisfy a non-trivial soundness guarantee.
Static types must predict some aspect of the values that flow though
 a program, beyond the baseline guarantee that every value is well-formed.

A typical soundness theorem helps programmers debug and helps compilers to
 generate efficient code.
MT soundness must provide similar benefits, but perhaps to a lesser extent
 if the cost of enforcing ``typical'' soundness is overwhelming.


@subsection{Requirement 2: descriptive types}

The language of static types must be able to describe idioms from the untyped
 code.
Untyped code comes first.
Types must be versatile enough to explain the reasoning behind a well-reasoned
 piece of untyped code without any sort of rewrites.
ML-style tagging and untagging is not acceptable.


@subsection{Requirement 3: user-supplied annotations}

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


@subsection{Requirement 4: clear typed/untyped boundaries}

Every boundary between typed and untyped code is a potential channel
 for a communication error.
If such an error occurs, the programmer needs to find the relevant channels.
The language should help with this debugging challenge.
In order to help, though, boundaries must be clearly stated in source code.
Programmers need to know where these boundaries are as they write the program,
 and the language needs to track them when it runs.


@section{Blame}
@; TODO

What, why, how.


@; ---
@section[#:tag "sec:why:design"]{Design Space: Expressiveness, Guarantees, Performance}

Anticipate the design-space analysis



