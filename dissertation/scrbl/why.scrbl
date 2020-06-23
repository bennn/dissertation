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




The research area of @emph{migratory typing} provides motivation and
 constraints for my thesis.

My thesis contributes to the research area of migratory typing.


The methods, experiments, and proofs that support the thesis
 provide new insights about known migratory typing systems
 and offer starting points into the unknown.

Besides motivation, migratory typing adds constraints.
Need to introduce before we present the work ahead.
Tell the MT story via steps.


@section{Observation: untyped code exists}

Programmers like it.
Don't care why.
Scripting, familiarity, ease-of-learning/use, culture, libraries.

Python JavaScript Clojure perfect settings.
Racket too, thats what we use.


@subsection{Why Racket?}

Mature implementation, start from non-zero really best place to start
 actively maintained and several users already in the wild.
Far advanced type system, easy to test whether solutions scale up.

Flexible language.
Clojure, impossible, realistically need to fork the JVM for the experiment.

Local expertise.
Whatever the host need to know it very well, helps to have support on hand,
 immersion.


@section{Observation: types offer benefits}

Types offer readability and maintenance, IDE tools, checks, catch errors
quickly.
Communicate intent.

Sound types help debugging.
Predict behavior.
May improve performance ... well, they do otherthingsequal but
otherthings are not equal there are undermining challenges ahead.


@section{Observation: smooth interop >>> FFI}

Previous motivate untyped and typed.
Clearly want both.
One way to achieve is FFI, multi-language.
Well why stop there?

Converting painful, invites bugs.
Loose FFI ties undermine type soundness.

By contrast, gradual.
Can improve legacy code incrementally.
Find bug, add types, fix bug, future payoff with little work simply taking
 notes and not-throwing-away that work you had to do.


@section{Checkpoint MT}

Want to mix.
Great, many other teams want to mix and have solutions for mixed-typed code.

Additional requirements, separate from the rest.

@; awkward digression to have


@section{Requirement: sound types}


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



