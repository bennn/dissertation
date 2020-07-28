#lang greenman-thesis/include

@title{What It's All About}


@; summary:
@; Languages that can mix typed and untyped code must come with a way to
@;  fine-tune the strength of types.
@; Part 1 = Performance analysis demonstrates the need for tuning.
@; Part 2 = Design-space analysis shows possible strengths.
@; Part 3 = Implements one tunable language, demonstrates benefits.
@;
@; ... gee do we want parts in this book after all?

Short intro to gradual typing, assumptions, gradual versus migratory.

Thesis statement:

@nested[#:style 'inset]{
 Honest and lying types can coexist in a way that preserves their formal
 properties; programmers can combine these types to strengthen lying-type
 guarantees, avoid unimportant honest-type runtime errors, and lower the
 running time of typed/untyped interactions.
}

Contributions that led to thesis:
@; don't really use an itemize in the end!

@itemlist[
@item{
  Performance evaluation method, sampling, apply to TR and Reticulated.
}
@item{
  Theoretical analysis, compromise semantics, complete monitoring, blame soundness and completeness.
}
@item{
  Transient implemented in Racket, generalized to richer type system, remove type dynamic.
}
@item{
  Integration of Transient and Guarded Typed Racket.
}
]

Upcoming chapters outline.



@section{Specification, Implementation, and Diction}
@; http://www.ccs.neu.edu/home/shivers/papers/whats-in-a-name.html

@; TODO names deep/natural/guarded ... shallow/transient ... prior work

This dissertation is about different ways of mixing typed and untyped code
in a programming language.
Each ``way'' starts from a rough idea, comes to life via a formal semantics,
and is tested against formal specifications.
Different instances of these three concepts need names.

My primary focus is on two rough ideas: @emph[sdeep] types and @emph[sshallow] types.
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
 between typed and untyped code---either with detailed assertions or proxy
 wrappers.
@|sTransient| provides @|sshallow| types by rewriting all typed code to
 check the basic shape of values anywhere that an untyped value might sneak in.

The two properties that distinguish these semantics, and thereby provide a
 formal distinction between @|sdeep| and @|sshallow| and weaker ideas,
 are @|scm| and @|sts| (@chapter-ref{chap:design}).
@|sNatural| satisfies @|scm| while @|stransient| does not.
Both @|snatural| and @|stransient| satisfy a non-trivial @|sts|;
 weaker mixings do not.

As a final note on word choice, I use informal words to talk about
 different ``ways of mixing typed and untyped code,'' including:
 methods, strategies, and approaches.
There is no hope in trying to be authoritative; the literature is filled with
 brilliant ideas and future work will continue to find novels ways to mix.


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
Earliers works on: higher-order contracts@~citep{ff-icfp-2002},
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


@section{Why Transient?}

There are several approaches to mixing typed and untyped code (@chapterref{chap:design}).
Some fail our basic criteria for adding types to an existing language (@chapterref{chap:why}).
Of the others, the guarded one is a worthwhile ideal because it offers strong
 static type guarantees.
Guarded types require run-time enforcement, however, and the cost of
 enforcement is high and hard to reduce.

@; fork in the road, what can be done?
  @; what to do?
  @; could improve TR, looking bleak
  @; only other option is to pursue a weaker semantics
  @; .... nothing else can support existing Dyn lang as intended

A promising compromise is to implement a different, weaker semantics alongside
 guarded types.
Analyzing the costs of guarded shows that the transient approach is the
 most promising alternative.
Guarded slows a program down through soundness checks and blame tracking.
The checks can be huge, and can require behavioral wrappers.
Blame requires additional wrappers.
Transient reduces checks to a minimum, a shape check.
(Anything less is hard to motivate!)
Transient can also run without blame simply.
By contrast, removing blame from guarded solves nothing at first;
 the change must be accompanied by a wholly-new strategy for allocating
 wrappers, and this strategy may require reflection tools for the wrappers
 themselves.

Transient is a promising wrapper-free approach to gradual typing.


