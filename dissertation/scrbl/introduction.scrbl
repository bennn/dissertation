#lang classicthesis/include

@title{Introduction}

Thesis statement: 
 Honest and lying types can coexist in a way that preserves their formal
 properties; programmers can combine these types to strengthen lying-type
 guarantees, avoid unimportant honest-type runtime errors, and lower the
 running time of typed/untyped interactions.

Short intro to gradual typing, assumptions, gradual versus migratory.

Contributions that led to thesis:

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
