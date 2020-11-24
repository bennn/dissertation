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

intro
 - wonderful idea, mixing typed and untyped
 - huge response ... headlines? promises?
   maybe go like a security talk ... why not its different for me
 - but hey different behaviors & performance
   ... warring states rather than happy village (lol really 3 states in end D S E + concrete)
 - ben = order to chaos,
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

 HEY shoot down other properties --- gg, ts ....
  and criticize Max?

design space (... lead into D S definitions)
 - wow this night & day is quite a surprise
   because both claim to be sound
 - Q is sound gt dead, published perf results,
   and yet seems like TR is dead others are fine!
   (need "sound" qualifier, talk about unsound = static-only at some point)
 - not so fast, examples, different behaviors
   = no transient is not simply "better" because its different behavior
 - next question is whether transient behavior is preferable
   ... that'll depend but here's a closer example
   ... clear error here, but transient misses it
   ... natural catches --- well thats odd
 survey (maybe not so much time on this)
 - don't take my word for it, put together a survey with examples like this,
   show one, see natural vs transient side-by-side,
   ask like/expected "attitude"
   sent to N students M developers K turkers
   results like the following --- transient dislike / unexpected
 - so no, transient is not clearly better
 return
 - differences are significant,
   need to articulate instead of example-by-example
 - for better understanding of pros cons
   and perhaps shed light on design space
   ... other points worth studying / implementing
 - more than 2 competitors, remember, wonder how they stack up
 cm
 - core idea back to example ... temporary vs permanent types
   clearly temporary in transient,
   seem to last, may be permanent, in natural
 - eureka, from christos, visit suggest CM a property from contracts
   test whether contract system is in control of all communications
   ... for us, ask whether type system in control
 - color half the space
 - then, using the same framework, able to further classify by studying blame
   (introduce TS colors here too)
 - final "richness" from error preorder (lift plateaus?)
 - let me show different repr, tabular
   (fill out appendix table?)
   semantics first, then properties one-by-one
   preorder is last ... maybe 1 2 3 4 4 5 ?

now, understanding of the design space, main regions,
 and have the performance results
 guarantees vs perf
 - what to do ... return of octopus? ... 
   - at least, credit to "Feltey" & "Grift" & Pycket
   - my angle
 - compromise = thesis statement
 - as you might have guessed we picked natural and transient, but with good reason!
 - justification
   - natural = strongest guarantees lowest on error preorder
   - transient = only sound design without wrappers, soundness minimum requirementA

unpublished results, let us proceed
  * moonlight is decent illustration,
    can move left-to-right across major sections

support for thesis
 - implement transient racket (side-by-side)
   theory (main challenges)
   - more types
   - completion, shout out to fritz (anticipate future challenge)
   impl
   - compiler pipeline, reuse
   - shape choices, optimize, list-rec
 - model, proves keep properties
   AND proves alluring extensions unsafe
   - draw D S U, alterative trust zones
 - implement 3-way interactions
   - ... remark mixed-typed API?
   - b0: fewer errors
   - b1: uniform behavior
   - b2: faster with mix
 - overall perf data
   - best of both, natural first then superimpose, conclude xxx
   - paths ... maybe show only the 6 small ones we have?
   - 

conclusion
 - with those expr, perf, data concludes defense of thesis
 - reminder: D S can coexist
 - whats next
  - finishing touches, release
  - static analysis, better completion
  - investigate blame ... what is useful? what is realizable?
 summarize contributions (as opposed to thesis)

extra
 blame!
 - major challenge, blame for transient / shallow
 - perf numbers omit blame, have to switch to Natural unclear if effective
 - yes transient does have a theory, rather lacking from our analysis but nonetheless ...
 - implemented, but yikes
 - show perf table, was unexpected but appears solid
 transient rejects
 - crazy all types, unprotected var, fix with explicit inst
 - crazy occurrence types, ... fix by guarding branches? inferences?
 - 
 bonus fixes / enhancements

