talk
===

Dissertation talk / slides


pitch 2020-10-27
---

Ben, final-semester Matthias
studying what types mean in gradual languages
NOT about what gradualized types mean,
 lots of fuss about effects of mixing Dyn
 but all second-order ... usually authors have a meaning in mind and want to achieve
 ... interested in the different meanings
some don't check types at all, TS
 others are sound but in a funny way, Transient
 motivations = performance, but not so cut & dry, expressiveness changes too
working on criteria, CM not TS, and finer distinctions across the design space
with perf, though, motivates a combo semantics
 so working on transient + natural typed racket
 highlights ...
 - theory
 - reuse knowledge? no
 - implementing, several grown idioms to work around


outline 2020-11-02, 2020-11-18 (!!!), 
---

wonderful idea, mixing typed and untyped

huge response ... headlines? promises?
 maybe go like a security talk ... why not its different for me

but hey different behaviors & performance
... warring states rather than happy village (lol really 3 states in end D S E + concrete)

ben = order to chaos,
 organize along 2 dimensions
 - perf for each village
 - design creats regions, deep shallow optional
 performance leads to thesis, that is D + S can interoperate
 leads to thesis question ... D + S can interact, benefits (details later)

perf space
 method
 - old days, mixed perf in passing
 - clear problem, unclear what to do or even suggest
   ... of course want to look at whole space, but not feasible in short time or perhaps necessary
 - but emails rolled in ... so we took seriously
 - collected benchmarks
 - ran FULL performance
 - study results, big space
 - visualization / interpretability also key to scaling
   ... other questions hard to stats-answer, max min ... average even
   anyway, D-delivable
 - method in full: benchmarks, establish full types for migratable modules,
   systematically remove and measure, plot D for range of D
 - approximate version: S samples of N configurations, make ivl for D-deliv from data
   ... success with linear size
   ... can reuse data for several
 uses
 - apply to TR and retic
 - recall different behaviors?
 results
 - D first, small number for low D in natural
   much better for retic, never falls off the map, 10x for these very different benchmarks
 - night and day
   (do NOT talk about num. types yet, thats a sideline observation that we can't scalably make)

design space (... lead into D S definitions)
 - wow this night & day is quite a surprise
   because both claim to be sound
 - Q is sound gt dead, published perf results,
   and yet seems like TR is dead others are fine!
 - not so fast, examples, different behaviors
   = no transient is not simply "better" because its different behavior
 - next question is whether transient behavior is preferable
   ... that'll depend but here's a closer example
   ... clear error here, but transient misses it
   ... natural catches --- well thats odd
 survey
 - don't take my word for it, put together a survey with examples like this,
   show one, see natural vs transient side-by-side,
   ask like/expected "attitude"
   sent to N students M developers K turkers
   results like the following --- transient dislike / unexpected
 - so no, transient is not clearly better
 return
 - but what's the property that distinguishes them?
   need a property to guide the design space ... because there are other points on map to assess
 - developed one point to show that TS not it
 - hm... adapted CM from contracts ... ideas here?

 - not talking about blame, but also helps for that
 - gotta mention the preorder too

 - out of steam

now, understanding of the design space, main regions,
 and have the performance results
 guarantees vs perf
 - motivates compromise = thesis statement
 - as you might have guessed we picked natural and transient
 - justification
   - natural = strongest guarantees lowest on error preorder
   - transient = only sound design without wrappers, soundness minimum requirementA

support for thesis
 - implement transient racket (side-by-side)
 - model, proves keep properties
   AND proves alluring extensions unsafe
 - implement 3-way interactions

unpublished results, let us proceed

transient racket


model
 - syntax? yes gotta 
 - ....
 - testbed for strategies, optimizations
   show D S U pict, trust boundaries

combination
 - hmm don't get too technical



