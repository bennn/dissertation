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



@; TODO names deep/natural/guarded ... shallow/transient ... prior work

