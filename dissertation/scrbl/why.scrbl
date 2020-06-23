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


@; -----------------------------------------------------------------------------

@title{Why Migratory Typing}



This dissertation contributes to the world of mixed-typed languages.
Any programming language that combines statically-typed and dynamically-typed
 code can benefit from the results below.
My motivation, however, is the vision of migratory typing---one
 particular way of mixing typed and untyped code.

Since migratory typing is the motivation behind my work,
 @section-ref{sec:mt} introduces and motivates migratory typing.
The discussion is based on @~a[num-mt-principle] defining principles.

For context, @section-ref{sec:non-mt} surveys other ways of making mixed-typed
 programs.
Each has something in common with migratory typing;
 they almost all satisfy basic principles motivating typed/untyped mixtures.
But they fail one or the other MT principle.


@section[#:tag "sec:mt"]{Why Migratory Typing}

@define-mt-principle['new-tool-for-new-code]{Enable Improvements}

Types are a useful technology.
Want the option of using types.


@define-mt-principle['no-catchall]{Flexibility via Subtyping}

Type system must have subtyping to give choice of precision.

Nothing more, no dynamic type for flexibility because plausibility check
makes types too weak.


@define-mt-principle['clear-boundary]{Clear Boundaries}

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



@subsection{Dart 2}

New top type, accepts any method call.

Cons: untyped code does not exist, no reuse


