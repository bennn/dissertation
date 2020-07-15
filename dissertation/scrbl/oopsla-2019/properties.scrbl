#lang greenman-thesis/include
@(require greenman-thesis/oopsla-2019/main)

@title[#:tag "sec:design:properties"]{Towards a Formal Comparison}

The design of a type-enforcement strategy is a multi-faceted problem.
It determines whether mismatches between type specifications and value flows
are discovered.
It determines whether the typed portion of the code is really statically typed.
It determines what typed APIs mean for untyped client code.
It determines whether an error message can pinpoint which type specification
does not match which value.
And all of these decisions imply consequences for the programmer and the
language designer. 

The examples in @sectionref{sec:design:jungle} illustrate that 
 various languages choose different points in this multi-faceted design space.
But, while they can motivate a systematic analysis, an example-based approach
 cannot serve as the basis of such an endeavor: the selection of examples
 and their translation across languages will always be somewhat ad-hoc.
Worse, the examples tell us little about the broader implications of each
 choice; at best they can indicate problems.

A systematic analysis needs a suite of formal and universal properties that
capture the consequences of design choices for the working developer and
language designer. Such properties must:
@itemlist[
@item{
    apply to a wide (if not the full) spectrum of design options,
}@item{
    articulate benefits of type specifications to typed and untyped code alike, and
}@item{
    come with proof techniques that scale to complex language features.
}]

@; -----------------------------------------------------------------------------
@section{Prior Work}

@emph{Type soundness} is one formal property that meets the above criteria.
A type soundness theorem can be tailored to a range of type systems,
 such a theorem has meaning for typed and untyped code,
 and the syntactic proof technique scales to a variety of language features@~citep{wf-ic-1994}.
The use of type soundness in the gradual typing literature, however, does not
 promote a level comparison.
Consider the four example languages from the previous section.
@citet{cvgrl-oopsla-2017} present a model of Flow and prove a conventional
 type soundness theorem, though, a close look reveals that it assumes all code
 is statically-typed.
@citet{vss-popl-2017} prove a type soundness theorem for Reticulated Python;
 a reader will eventually notice that the theorem talks about the outer shape
 of values, rather than their types.
@citet{mt-oopsla-2017} prove a full type soundness theorem for Nom.
@citet{tf-dls-2006} prove a full type soundness theorem for a prototypical
 Typed Racket that includes a weak blame property.
To summarize, the four advertised type soundness theorems differ in several
regards:  one focuses on the typed half of the language;
 a second proves a claim about a loose relationship between values and types; 
 a third is a truly conventional type soundness theorem, covering the full language;
 and the last one finally incorporates a claim about the quality of error messages.

@citet{svcb-snapl-2015} propose the @emph{gradual guarantee} as a test to
 identify languages that enable smooth transitions between typed and
 untyped.
They and others show that the gradual guarantee holds for
 relatively simple type languages and syntactic constructs; proving that it
 generalizes to complex type systems is the subject of active
 research@~citep{isi-icfp-2017, tlt-popl-2019, nja-popl-2020}.
The guarantee itself, however, does not tell apart the behaviors in @sectionref{sec:design:jungle}.

Both Reticulated and Nom come with published proofs of the gradual
 guarantee@~citep{vss-popl-2017,mt-oopsla-2017}.
Typed Racket has the guarantee as a design goal.
@; cite TR + gradual guarantee? I don't know an official statement
Even Flow, thanks to its lack of dynamic checks, satisfies the criteria for
 a smooth transition.
In sum, the gradual guarantee characterizes a dimension that is only loosely related
 to the central question of type enforcement. And the all-or-nothing nature
 of the guarantee suggests a need for additional  properties.

By contrast, the @exact{\kafka{}} framework is able to
 distinguish behaviors but lacks a meta-theoretical analysis@~citep{clzv-ecoop-2018}.
The sole theorem in the paper states type soundness for a statically-typed
 evaluation language.
Different behaviors arise, however, from four translations of a mixed-typed
 surface language into this evaluation language.
One can observe the behaviors, but the model does not characterize them.

@citet{nla-popl-2019} distinguish gradual typing systems via @emph{equivalence preservation}.
They start from a set of axioms for fully-typed expressions---for
 example, @${\beta} and @${\eta} equations---and ask whether interactions with
 untyped code can violate the axioms.
Equivalence preservation does define a spectrum;
 the @|nname| semantics preserves @${\eta} for pairs and functions,
 but a lazy variant (@|cname|) fails for pairs.
It also cannot tell apart the @|tname| and @|ename|
 semantics because both fail to preserve the axioms.
Furthermore, its type-centric nature offers no direct information to authors of
 untyped code.
Authors of untyped code can at best deduce that the behavior of their programs
 cannot be affected by certain changes in typed libraries.
And programmers get no information about why a cast error occurs.
As a final remark, techniques for proving equivalence preservation are an
 active area of research but results so far indicate that they require a lot of
 ingenuity to adapt from one linguistic setting to another.

Another well-studied property is the @emph{blame
theorem}@~citep{tf-dls-2006, wf-esop-2009, afsw-popl-2011,
svctg-esop-2015, w-snapl-2015, vss-popl-2017}.
Despite the authoritative name, this property is not the final word on blame.
It merely states that a run-time mismatch may occur only when an untyped value
enters a typed context; a typed value cannot trigger an error when it enters
untyped code.
Under the assumption that all types in a mixed-typed program are correct,
this theorem justifies the slogan ``well typed programs can't be blamed''
for a @|nname| semantics.
But, types that are imposed retroactively on an
untyped program may not be correct; refer to @figure-ref{fig:tr-example} for
one example, and @citet{dnff-icfp-2016} for further discussion.
Furthermore, the slogan does not apply to @|tname| or any other semantics
in which a value can cross a boundary without a complete type check.


@section{Our Analysis}

The primary formal property has to be type soundness, because it tells a
programmer that evaluation is well-defined for mixed-typed programs.

The second property, @emph{complete monitoring}, asks whether types
are enforced on all explicit and implicit channels of
communication between typed and untyped code.
That is, every interaction between typed and untyped code respects all relevant
type annotations.

When a run-time check discovers a mismatch between a type specification and
a flow of values and the run-time system issues an error message, the
question arises how informative the message is to a debugging programmer.
@emph{Blame soundness} and @emph{blame completeness} ask whether a
mixed-typed semantics can identify the responsible parties when a run-time
type mismatch occurs.
Soundness asks for a subset of the potential
culprits; completeness asks for a superset.

Furthermore, the differences among type soundness theorems and the
difference between type soundness and complete monitoring suggests the
question how many errors an enforcement regime discovers. The answer is an
@emph{error preorder} relation, which compares semantics in terms of
the run-time mismatches that they discover.

Individually, each property characterizes a particular aspect of a
type-enforcement semantics.
Together, the properties inform us about the nature of the multi-faceted design
space that this semantics problem opens up.
And in general, this work should help with the articulation of
consequences of design choices for the working developer.



