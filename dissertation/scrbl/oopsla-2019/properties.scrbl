#lang greenman-thesis/include
@(require greenman-thesis/oopsla-2019/main)

@title[#:tag "sec:design:properties"]{Towards a Formal Comparison}

The design of a type-enforcement strategy is a multi-faceted problem.
A strategy determines many aspects of behavior:
 whether mismatches between type specifications and value flows are discovered;
 whether the typed portion of the code is really statically typed, in a conventional sense;
 what typed APIs mean for untyped client code;
 and whether an error message can pinpoint which type specification does not
 match which value.
All of these decisions imply consequences for the programmer and the language
designer.

The examples in @sectionref{sec:design:jungle} show that
 various languages choose different points in this multi-faceted design space.
But, examples can only motivate a systematic analysis; they
 cannot serve as the basis of such an endeavor.
The selection of example programs and their translation across languages
 require too much insight.
Worse, the examples tell us little about the broader implications of each
 choice; at best they can demonstrate issues.

A systematic analysis needs a suite of formal properties that capture the
consequences of design choices for the working developer and language designer.
Such properties must
    apply to a wide (if not the full) spectrum of design options,
    articulate benefits of type specifications to typed and untyped code alike, and
    come with proof techniques that scale to complex language features.

@|noindent|The literature on gradual typing suggests few adequate properties.
Our analysis therefore brings new properties to the toolbox.


@; -----------------------------------------------------------------------------
@section[#:tag "sec:design:old-properties"]{Comparative Properties in Prior Work}

@emph{Type soundness} is one formal property that meets the above criteria.
A type soundness theorem can be tailored to a range of type systems,
 such a theorem has meaning for typed and untyped code,
 and the syntactic proof technique scales to a variety of language features@~citep{wf-ic-1994}.
The use of type soundness in the gradual typing literature, however, does not
 promote a level comparison.
Consider the four example languages from the previous section.
@citet{cvgrl-oopsla-2017} present a model of Flow and prove a conventional
 type soundness theorem under the assumption that all code is statically-typed.
@citet{vss-popl-2017} prove a type soundness theorem for Reticulated Python;
 a reader will eventually notice that the theorem talks about the @emph{shape}
 of values not their types. 
@citet{mt-oopsla-2017} prove a full type soundness theorem for Nom,
 which implements the concrete approach.
@citet{tf-dls-2006} prove a full type soundness theorem for a prototypical
 Typed Racket that includes a weak blame property.
To summarize, the four advertised type soundness theorems differ in several
regards:  one focuses on the typed half of the language;
 a second proves a claim about a loose relationship between values and types; 
 a third is a truly conventional type soundness theorem;
 and the last one incorporates a claim about the quality of error messages.

@citet{svcb-snapl-2015} propose the @emph{gradual guarantee} as a test to
 identify languages that enable smooth transitions between typed and
 untyped. They and others show that the gradual guarantee holds for
 relatively simple type languages and syntactic constructs; proving that it
 generalizes to complex type systems is the subject of active
 research@~citep{isi-icfp-2017, tlt-popl-2019, nja-popl-2020}.
 The guarantee itself, however, does not tell apart the behaviors in @sectionref{sec:design:jungle}.
Both Reticulated and Nom come with published proofs of the gradual
 guarantee@~citep{vss-popl-2017,mt-oopsla-2017}.
Typed Racket states the guarantee as an explicit design goal.
Even Flow, thanks to its lack of dynamic checks, satisfies the criteria for
 a smooth transition.

The @exact{\kafka{}} framework is able to
 distinguish behaviors but lacks a meta-theoretical analysis@~citep{clzv-ecoop-2018}.
The sole theorem in the paper states type soundness for a statically-typed
 evaluation language.
Different behaviors arise, however, from four translations of a mixed-typed
 surface language into this evaluation language.
One can observe the behaviors, but the model does not characterize them.

@citet{nla-popl-2019} distinguish gradual typing systems via @emph{equivalence preservation}.
Starting from a set of axioms for typed expressions---for
 example, @${\beta} and @${\eta} equations---they ask whether interactions with
 untyped code can violate the axioms.
Equivalence preservation does define a spectrum;
 the @|nname| semantics preserves @${\eta} for pairs and functions,
 and a lazy variant (@|cname|) fails for pairs.
But, this spectrum is rather coarse.
The @|tname| and @|ename| behaviors are indistinguishable under
 equivalence preservation because both fail to preserve the axioms.
Furthermore, the type-centric nature of the equivalences offers no direct
 information to the untyped side.
Authors of untyped code can at best deduce that the behavior of their programs
 cannot be affected by certain changes in typed libraries.
As a final remark, techniques for proving equivalence preservation are an
 active area of research but results so far indicate that they require a lot of
 ingenuity to adapt from one linguistic setting to another.

Another well-studied property is the @emph{blame theorem}@~citep{tf-dls-2006, wf-esop-2009, afsw-popl-2011,
svctg-esop-2015, w-snapl-2015, vss-popl-2017}.
Despite the authoritative name, this property is not the final word on blame.
It states that a run-time mismatch may occur only when an untyped value enters
a typed, or more-precisely typed, context; a typed value cannot trigger an error by
crossing to less-typed code.  The property is a useful design principle, but
does not distinguish the various semantics in the literature.
To its credit, the blame theorem does justify the slogan ``well typed programs can't be blamed''
 for a @|nname| semantics under the assumption that all boundary types are correct.
The slogan does not apply, however, to a semantics such as @|tname| that lets a
value cross a boundary without a complete type check.
Nor does it hold for incorrect types that were retroactively added to an
untyped program; refer to @figureref{fig:tr-example} for
one example and @citet{dnff-icfp-2016} for further discussion.


@section{Our Analysis}

The primary formal property has to be type soundness, because it tells a
programmer that evaluation is well-defined in each component of a mixed-typed programs.

The second property, @emph{complete monitoring}, asks whether types
guard all statically-declared and dynamically-created channels of
communication between typed and untyped code. That is, whether every interaction
between typed and untyped code is mediated by run-time checks.

When a run-time check discovers a mismatch between a type specification and
a flow of values and the run-time system issues an error message, the
question arises how informative the message is to a debugging programmer.
@emph{Blame soundness} and @emph{blame completeness} ask whether a
mixed-typed semantics can identify the responsible parties when a run-time
type mismatch occurs.  Soundness asks for a subset of the potential
culprits; completeness asks for a superset.

Furthermore, the differences among type soundness theorems and the
gap between type soundness and complete monitoring suggests the
question how many errors an enforcement regime discovers. The answer is an
@emph{error preorder} relation, which compares semantics in terms of
the run-time mismatches that they discover.

Individually, each property characterizes a particular aspect of a
type-enforcement semantics. Together, the properties inform us about the
nature of the multi-faceted design space that this semantics problem opens
up. And in general, this work should help with the articulation of
consequences of design choices for the working developer.

