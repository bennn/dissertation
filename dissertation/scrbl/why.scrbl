#lang classicthesis/include

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

@title{Migratory Typing}
@; why / where-from

@; summary:
@; Migratory typing is a principled way to mix typed and untyped code.
@; This dissertation helps improve MT,
@;  and MT is the guiding motivation.
@; MT axioms and principles influence work.
@; ... purpose statement ...

@;There are several ways to mixed-typed.
@;
@;The research area of @emph{migratory typing} provides motivation and
@; constraints for my thesis.

My thesis contributes to the research area of migratory typing.
The methods, experiments, and proofs that support the thesis
 provide new insights about known migratory typing systems
 and offer starting points into the unknown.
Before we can present the results, we need to establish the context
 of migratory typing.

@;Besides motivation, migratory typing adds constraints.
@;Need to introduce before we present the work ahead.
@;Tell the MT story via steps.


@section{MT Observations}

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

The observations 

@subsection{Requirement 1: sound types}




@; sound types


@section{Requirement: descriptive types}

Able to describe untyped behavior,
no changes to code,
flexibility via subtyping


@section{Requirement: user-supplied annotations}

conversation between user and type system,
notes for future readers,
tune descriptiveness of types

led to success where other approaches failed usability


@section{Requirement: clear boundaries}

Because untyped code exists, miscommunications are possible.
Need to trace back to typed/untyped interfaces.
These interfaces need to be clear in the source code for meaningful errors.
Such errors are critical, analogy to static type errors.


@; ---
@assert-mt-principle[]
@section[#:tag "sec:non-mt"]{Other Mixed-Typed Methods}

There are several other methods, besides migratory typing, for mixing typed
and untyped code.
Here we review the main ones.


@subsection{Gradual Typing}

Gradual typing includes a special dynamic type.
Satisfies guarantees.

PS No guarantee that untyped code exists, maybe cannot reuse.

Pros: easy to use

Cons: catch-all type hides errors, inhibits blame 

Conjecture reading vs writing tradeoff


@subsection{Optional Typing}

Aiming for something more than static analysis.
Nobody will if not academics.



