dissertation
===

Thesis:
- Q0. Does migratory typing benefit from a combination of honest and lying types?
- A1. Honest and lying types can coexist in a way that yields significant
      performance benefits.
      (coexist to acheive significant performance benefits)
- A2. Honest and lying types can coexist in a way that preserves their formal
      properties; programmers can combine these types to strengthen lying-type
      guarantees, avoid unimportant honest-type runtime errors, and lower the
      running time of typed/untyped interactions.

Title: TBA

Deadline: Barkpeelers 2020


Acknowledgments:
 split into a dedication & bibliography
 bibliography should acknowledge everyone who cannot be cited
 dedication should give heartfelt thanks
 .... unless we have something to say, no acks!
   maybe a program or formula: \forall person \in (dedication, bio, bib) . thankyou(person)

- - -

update 2020-07-21
---

Dear committee members,

Since the last update, I have:

- collected some data on the transient implementation;
- tried adding blame, found the POPL'17 algorithm unusably slow;
- and started piecing together the dissertation


## transient performance

Transient is often better than normal Typed Racket, but not always.

The table here shows worst-case overheads in a few small benchmarks.
Transient wins by a lot in `jpeg` and `zombie`, but fares worse in three.
`snake` is especially bad.

| benchmark |  max TR |  max transient |
|-----------+---------+----------------|
| fsm       |   1.25x |          1.06x |
| jpeg      |  19.98x |          2.04x |
| kcfa      |   3.81x |          3.78x |
| mbta      |   1.43x |          1.41x |
| morsecode |   1.84x |          2.48x |
| snake     |  15.19x |         22.13x |
| zombie    |  53.95x |          3.39x |
| zordoz    |   2.77x |          5.74x |

What to do?
In the past, my Transient would remove the runtime check around `(f x)` if `f`
was defined in transient code and never crossed a boundary. This helped bring
`snake` to a ~8x overhead, but isn't safe in general.


## blame

I tried implementing the blame algorithm from Vitousek et al POPL 2017.

The algorithm idea is to keep a map from functions/pairs/objects to boundary
types. When one value gets eliminated, this map gets a new entry pointing
from the result to its parent's boundary types.

Keeping this map up-to-date, however, takes a LOT of extra time and space
at runtime. Here are a few rows from above with a new column for a
preliminary transient with blame:

| benchmark |  max TR |  max transient | transient-blame |
|-----------+---------+----------------+-----------------|
| jpeg      |  19.98x |          2.04x |           4.79x |
| snake     |  15.19x |         22.13x |         224.27x |
| zordoz    |   2.77x |          5.74x |          10.81x |

Besides the huge cost, there are other challenges that I will explain in
the dissertation. Transient blame seems impractical.


## dissertation

I have been importing material from past work---mostly JFP 19 and our submission
to the JFP special issue on gradual typing---into the dissertation. Right now,
I have first drafts for 3 of the 8 planned chapters.


## public pre-release of transient

The goal for Transient is to merge into a future Typed Racket release. Things
are coming along, so I've opened a pull request with the main part of the
implementation to solicit comments:

  <https://github.com/racket/typed-racket/pull/948>




update 2020-02-21
---

Dear committee members,

Since last time, I've stopped work on the model and switched focus to
an implementation of Transient Typed Racket.


## revised thesis statement

Honest and lying types can coexist in a way that preserves their formal
properties; programmers can combine these types to strengthen lying-type
guarantees, avoid unimportant honest-type runtime errors, and lower the
running time of typed/untyped interactions.


## model

In January / February, I worked to reduce the number of run-time checks
that the previous model required. This work has been unsuccessful in general;
see the bottom of this message for details.

I've decided to stick with the model from last time and base the
implementation on that.


## implementation

I've been making progress fixing bugs and getting benchmarks to run with a
Transient Typed Racket. Right now all of the benchmarks can run, but I need to
do more testing to make sure they're running correctly and efficiently.

Here is a patch that shows the changes to base Typed Racket:

  https://gist.github.com/bennn/e2971f65448adaa5831288d585861f43


## next

For next time, the goal is to report some lessons and measurements about the
implementation.

Ben

- - -

## extra notes on the model

We currently have a model that combines honest & lying types in a simple way.
If we split a program into 3 worlds --- H = honest, L = lying, U = untyped ---
then the diagram below shows the checks that currently happen at every boundary.
A "wrap" is a wrapper or deep check, a "scan" is a shape check, and a "noop"
is no check.

      +--[wrap]-->  H  <--[wrap]--+
      |                           |
      v                           v
         >-------[noop]-------->
      L                           U
         <-------[scan]--------<

     Current status

The question from last update was whether we can get away with fewer wraps
and scans by adding labels to every boundary. For example, a `wrap` boundary
could turn into `wrap(from H to L)`.

If lying types = transient, then the answer is usually no. For special cases
we can avoid wrapping an L value that enters an H context, but in general
boundary labels aren't enough. So the picture above doesn't change. We need to
decorate a value with its original type to know whats safe. That may be a good
future project, but I think adding metadata to the runtime is out of scope for
my dissertation.

If lying types != transient and we use a kind of wrappers instead, then we
can make the H <-> L boundary a noop.

      +--[noop]-->  H  <--[wrap]--+
      |                           |
      v                           v
         >-----[noop/wrap]----->
      L                           U
         <-----[scan/wrap]-----<

     Potential, if L creates wrappers

But now we lose the benefits of transient and need to implement a new kind of
wrapper.



update 2019-12-18
---

Dear committee members,

Since my proposal presentation, I've drafted a new thesis statement, made
progress on the model, and found some library-using programs that run faster
with my Transient prototype.


## draft thesis statement

Honest and lying types can coexist in a way that retains the formal
properties of each and yields measurably significant performance benefits.


## model

I developed a model with:

- three surface languages (honest, lying, untyped)
- one evaluation language
- one surface-to-evaluation compiler that inserts runtime checks

The semantics of the evaluation language does not rely on type or language
information; it simply executes the checks inserted by the compiler.

I have mostly completed the proofs of type soundness (for all 3 surface langs)
and complete monitoring (for honest types).

This model reflects what we can easily implement in the current Racket
implementation --- we can insert runtime checks during compilation,
but we don't yet have a way to attach type & language info to the checks.


## transient, math library

I've found 7 untyped programs (from various Racket packages) that use the typed
math library. When I recompile the math library to use the transient prototype,
these programs get 2x to 30x faster.

I'll keep looking for more case studies like these, and hope to convert some
into full benchmarks.


## next

My next step is to add language information to the runtime checks in the model.
I expect to exploit this information to reduce the number of checks.

A little more precisely, the current model has 3 kinds of check:

1. wrap = check full type and wrap higher-order values
2. scan = check value shape
3. noop = do nothing

The next model will keep these kinds of checks and annotate each check in a
program with two languages. So if H = honest, L = lying, and U = untyped, we
have at least these 6 checks:

1. H -> wrap -> U
2. U -> wrap -> H
3. H -> wrap -> L
4. L -> wrap -> H
5. U -> scan -> L
6. L -> noop -> U


Ben



2019-11-25
---

Promises

- change the thesis question to be clearer; articulate the goals & ways it can fail

- find criteria to test whether my implementation of Transient is
  worth measuring (two options so far: compare the overhead to
  Reticulated, and compare the perf. relative to Natural on programs
  where the two should theoretically do the same checks)

- for the evaluation, show the "fixed" modules and how they are typed
  (untyped, N, or T)

- look for additional benchmarks with fixed modules that cannot be typed as-is

- be sure to support & evaluate object-oriented programs

- run a quick experiment on the Math library to see what kind of
  improvement we can expect; find an untyped program that suffers,
  convert the library to Transient, see if the untyped suffering is
  gone, and see what happens if the program is Natural-typed

- rename "honest" & "lying"


2019-11-26
---

(email from Fritz; I'd asked about a reference to a McAllester paper)

> Hi Ben:
> 
> I've been thinking about contracts on and off recently; they are reflected in
> the enclosed talk given at KAIST earlier this year.  It's based on discussions
> with Thomas Jensen (INRIA) over the years.  It'd be great to see it written up
> and published at some point.  (It's not, yet :-))
> 
> A good part, except the duality per se, is technically worked out in Blume &
> MacAllester (enclosed), as it turns out, which is based on semantic types;
> these in turn have made a come-back, in great part due to work by Derek Dreyer
> and collaborators.  The duality idea was mentioned to me by Peter Thiemann in
> the coffee room at a Dagstuhl seminar (that Shriram organized -- the other 3
> organizers, including myself, were, relatively speaking, extras).  Peter
> subsequently published a paper with a surprisingly syntactic approach to
> contracts, similar to the Felleisen/Findler approach that you're following
> (whether you know it or not :-)); he later mentioned that he found that the
> duality idea didn't work, which has left me puzzled -- duality seems to work
> perfectly as the natural semantic framework for contracts, with
> instrumentation/monitoring, blame and blame label algorithms, etc, explicable
> as (sound) algorithms for constructing a proof that one of the components
> doesn't have the property it claims to have.  
> 
> I'm also including Jakob Rehof's Master's thesis; it's the most advanced
> write-up of Dynamic Typing, which is a static type discipline with explicit
> coercions (for deep/induced -- 'natural' in your terminology -- or top-level --
> 'transient' in your terminology -- checking and tagging).  Prior publications
> are in LFP '92, ESOP '92, FPCA '95, Science of Computer Programming '94.  In
> terms of 'gradual typing', sections of code without coercions are
> statically/fully typed in Dynamic Typing; those with coercions are dynamically
> typed/run-time typed ('untyped'), though some tags and checks may be omitted
> when it's statically proved to be safe to do so.  Note that the 'positive'
> coercions in Dynamic typing represent tagging operations -- the data structures
> are only implemented with those tags if there is such a coercion inserted or
> inferred in the source code.  In particular, statically typed code has no tags,
> like C or like a modern (post-Lisp)  ML family compiler; see e.g. our FPCA '95
> article (also in MS thesis, Chap. 7).  If you're wondering about support for
> garbage collection, fair enough; we should talk about region-based memory
> management at some point then...
> 
> Best regards,
> Fritz

Followup:

> ICFP '15 is what they published after the Dagstuhl seminar where I talked to
> both of them (actually, Peter; Matthias is a very quiet person) -- but didn't
> contain what I thought it would.  For some reason they didn't choose a
> semantic types approach; a semantic types approach is tricky, but actually
> quite elegant (the difference between error and nontermination is the
> critical ingredient); see my slides (I have written up more, but nothing is
> published yet; many of the semantic ideas are present or implicit in
> McAllester's and Dreyer's work) .  Syntactic dependent type approaches can
> get entangled in various quandaries; a semantic duality approach (probably
> best developed in a semantic realizability setting rather than syntactic
> contracts/dependent types, etc.)  has proved quite robust so far.

Well okay, he too is interested in the semantic types perspective. That makes:
 Ron = type-semantics to get N,T,E operational semantics
 Amal = semantic version of complete monitoring

- - -


