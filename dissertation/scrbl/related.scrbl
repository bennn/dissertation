#lang greenman-thesis/include

@title[#:tag "chap:related"]{Related}

All the related work for previous chapters.
Evaluation method.
Theory tools: GTT, KafKa, gradual guarantee.
Other implementations of Transient, perhaps other Guardeds.
New related work about the combination, other hybrids, like types, Pyret.



@;@subsection{Gradual Typing}
@;
@;Gradual typing is not necessarily migratory,
@; non-migratory is addressing a dubious problem.
@;
@;Includes a special dynamic type.
@;The type makes it easy to add types to code, no insistence on detail.
@;This makes it harder to detect static type errors ... but not much worse
@; than free use of a top type.
@;More importantly, the dynamic type obscures the boundaries between typed
@; and untyped code.
@;@; easy to get many boundaries, hard to point to these

@;@subsection{Soft Typing}
@;
@;Infer types for any untyped program.
@;
@;Cons: does not reject, no explicit annotations
@;
@;
@;@subsection{Lump Type-Dynamic}
@;
@;Special type, embedding / projection.
@;
@;Cons: rewrite code when adding types, classic con of memorize proj-path ... this
@;one is really more related work
@;
@;
@;@subsection{Quasi-Static Typing}
@;
@;Generalize GT?
@;
@;Cons: lack boundaries,
@;
@;very related work, point out the design alternatives from Thatte
@;
@;
@;@subsection{Dart 2}
@;
@;New top type, accepts any method call.
@;
@;Cons: untyped code does not exist, no reuse
@;
@;@subsection{Pyret}
@;@subsection{Like Types}
@;

@; Interop
@; @citet{mf-toplas-2009} ... gray ... go older too?
@;  ... barret ecoop 15
