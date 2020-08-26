#lang greenman-thesis/include

@title{What It's All About}
@; from abstract, have feelings about:
@; - type system
@; - mixed-typed language
@; - 3way tradeoff
@; - order to design space
@; - two ideas: deep and shallow types

A language that can mix typed and untyped code must balance three conflicting
 dimensions:
@exact{{\renewcommand{\labelitemi}{{\large\decothreeright}} %{\raisebox{0.4ex}{\tiny\(\bullet\)}}
\begin{itemize}
\item \emph{Proofs}\/:
  Static types should be accurate predictions about the way a program
   behaves at run-time.
  If a type makes a claim, then other code---typed or untyped---may
   depend on it.
\item\emph{Performance}\/:
  Adding types to part of a codebase should not cripple its running time.
  On the contrary, a smart compiler should use type-proofs to generate
   efficient code.
\item \emph{People}:
  Untyped code must be free to create all sorts of values;
   typed code must be able to interact with many untyped designs.
\end{itemize}}}

@|noindent|@;
The ideal mixed-typed language would satisy all three goals,
 letting programmers add descriptive types to any component in a program
 and supporting those types with deep guarantees and fast performance.
This ideal is not here yet.
Friction between the dimensions raises a whole host of problems
 about how to enforce types at run-time.
In particular, @emph{performance} is the driving question.
Type guarantees that can (in principle) be enforced against untyped code often bring an
 overwhelming cost, slowing a program down by several orders of magnitude.

Researchers have addressed the performance question with designs that advertise
 low costs, but overall progress towards the ideal is marginal because these
 designs are incomparable.
For one, the performance of a new mixed-typed language is intertwined
 with its host language; comparing performance across different languages
 is hopeless.
Second, the new designs typically compromise on proofs or people.
Lacking an apples-to-apples comparison, it is impossible to decide whether
 a language has solved the performance question.

The first half of this dissertation untangles the design space.
I present methods to measure performance, methods to measure type guarantees,
 and basic requirements for expressiveness.
I apply these methods and conclude that there are two promising designs:
 @|sdeep| types via the @|snatural| semantics and @|sshallow| types via
 the @|stransient| semantics.
The now-obvious question is whether a language can have both.
Hence the second half of this dissertation, which supports my thesis statement.


@section{Thesis Statement}

@|sDeep| and @|sshallow| types can coexist in a way that preserves their formal
properties; programmers can combine these types to strengthen @|sshallow|-type
guarantees, avoid unimportant @|sdeep|-type runtime errors, and lower the
running time of typed/untyped interactions.


@section{Dissertation Overview}

Looking ahead, the first order of business is to lay down ground rules for
 expressiveness.
My goal is to combine typed and untyped code in a @emph{migratory typing}
 system, in which types accommodates the grown idioms of an
 untyped host language (@chapter-ref{chap:why}).
Languages that fail the expressiveness criteria, however, can still benefit
 from the results in later chapters.
Next up, @chapter-ref{chap:performance} presents methods to evaluate performance
 and validates the methods through an empirical study of two migratory
 typing systems: Typed Racket and Reticulated Python.
Both languages guarantee type soundness, but come with very different
 performance; more surpringly, they compute different results for
 seemingly-equal code.
@Chapter-ref{chap:design} brings these two languages, and several others,
 into a common model for a mathematically-precise comparison of different
 designs.
The design-space analysis motivates a compromise between two semantics,
 @|snatural| and @|stransient|.
@Chapter-ref{chap:transient} presents the first half of the compromise;
 namely, a @|stransient| semantics for Typed Racket.
@Chapter-ref{chap:both} formally proves that @|sdeep| and @|sshallow|
 types can interoperate and reports on a Typed Racket that supports
 both @|snatural| and @|stransient| behavior.
Along the way, each chapter acknowledges collaborators and closely-related
 work.
A broad family tree comes at the end (@chapter-ref{chap:related}),
 along with a view toward future work (@chapter-ref{chap:future})
 and reflections on the wider research context (@chapter-ref{chap:conclusion}).


@section{Specification, Implementation, and Naming}
@; http://www.ccs.neu.edu/home/shivers/papers/whats-in-a-name.html

This dissertation is about different ways of mixing typed and untyped code
 in a programming language.
Each ``way'' starts from a rough idea, comes to life via a formal semantics,
 and is tested against formal specifications.
Different instances of these three concepts need names.

My primary focus is on two rough ideas: @|sdeep| types and @|sshallow| types.
@|sDeep| types are nearly as good as static types.
If types in a statically-typed language provide a certain guarantee, then
 the @|sdeep| versions of those types strive for the same guarantee no matter
 what untyped code throws at them.
@|sShallow| types are weaker than @|sdeep| types, but better than nothing.
A @|sshallow| type might provide a temporary guarantee, and it might talk
 about fewer behaviors than the corresponding static type.

These two ideas are accompanied by two leading semantics: @|snatural|
 and @|stransient| (@chapter-ref{chap:design}).
@|sNatural| provides @|sdeep| types by carefully monitoring the boundaries
 between typed and untyped code---either with exhaustive assertions or proxy
 wrappers.
@|sTransient| provides @|sshallow| types by rewriting all typed code to
 check the basic shape of every value that might be from untyped.

@exercise[1]{
  Why is it a bad idea to reuse the names ``@|sdeep|'' and ``@|sshallow|''
   in place of @|snatural| and @|stransient|?
}

The two properties that distinguish these semantics, and thereby provide a
 formal distinction between @|sdeep| and @|sshallow| and weaker ideas,
 are @|scm| and @|sts| (@chapter-ref{chap:design}).
@|sNatural| satisfies @|scm| while @|stransient| does not.
Both @|snatural| and @|stransient| satisfy a non-trivial @|sts|.
Weaker mixings are unsound.

As a final note on word choice, I use informal words to talk about
 different ``ways of mixing typed and untyped code,'' including:
 methods, strategies, and approaches.
There is no hope in trying to be authoritative because the research community
 is still seeking a best method for a best combination.


@subsection{Names in Prior Work}

@citet{tgpk-dls-2018} introduce the names @|sdeep| and @|sshallow|, but use
 them to refer to the @|snatural| and @|stransient| implementations.
@citet{gf-icfp-2018} use @emph{higher-order} for the @|sdeep| idea and
 @emph{first-order} for the @|sshallow| idea.

@|sNatural| goes by many names.
@citet{vksb-dls-2014} and several others call it @emph[sguarded] because
 the semantics keeps a firm barrier between typed and untyped code.
@citet{clzv-ecoop-2018} introduce the word @emph{behavioral} for both
 the semantics and its characteristic wrapper values.
Foundational papers simply call it gradual typing@~cite{st-sfp-2006,tf-dls-2006,htf-hosc-2010}.

The name @|snatural| comes from @citet{mf-toplas-2009}, who use it
 to describe a proxy method for transporting untyped functions into a
 typed context.
Earliers works on higher-order contracts@~citep{ff-icfp-2002},
 remote procedure calls@~citep{ok-popl-2003}, and
 typed foreign function interfaces@~citep{r-jfp-2008}
 employ a similar method---probably because it seemed like the obvious
 way to preserve input/output behaviors.
@citet{nla-popl-2019} present a semantic argument that @|snatural| is indeed
 the only way to enforce the key properties of static types.

@futurework{
Summarize the different @|snatural| semantics in the literature as a
 natural transformation between two functors.
}


