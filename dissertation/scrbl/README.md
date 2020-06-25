TODO
- [ ] intro : assumptions about gradual / migratory typing,
  see jfp19=X oopsla19=? jfp20=? proposal=X
  - [X] hang on, assumptions must go in extra chapter,
        they're not critical for theory work,
        they only set stage for performance ---- again not critical,
        it's just a gradual essay for why we picked this
        chapter2 = why
        chapter1 = what it's all about
        ... maybe DO want that TS Eliot quote to explain the writing experiment
- [ ] performance chapter (2), adapt jfp19
- [ ] transient-racket chapter (4), committe update email
- [ ] theory chapter (3)
- [ ] full intro (1)

- [ ] ABC00 cite style, like theory papers

- [ ] start every chapter with acks, papers, people


#### dear diary

2020-06-18 : need to get moving, spend 2 hours/day or more cmon now


#### outlines

2020-02-03 :
 Matthias says, the two journal papers will be approx. two chapters so we can
 work those in whenever ready.

2020-05-22 :
 outline attempt
  (why not chronological?)

 0. honest + lying can coexist, thesis ... quest for practical GT
 1. different semantics, which is best? lacking theory + eval landscape
 2. perf eval, applied to TR + Retic (ignore ICFP)
 3. theory eval, for Guarded + Transient + Erasure + compromises
 4. Guarded + Transient
    - why this pair
    - model
    - evaluation
    - other benefits
 5. threats, other concerns (survey)
 6. related work
 7. 

Ben's dissertation outline (2020-05-29):

1. intro, up to thesis,
   contributions that led to thesis
   - perf. evaluation method
     sampling,
     apply to TR, Retic
   - theory analysis
     compromise semantics (TS not enough)
     CM distinguishes honest vs lying
     blame soundness + completeness
   - transient racket (without TR)
     generalize to richer type system (unions, subtyping, ...)
     remove type dynamic
     ... generalize what's needed to insert check?
   - integrating transient + TR
     generalizes smoothly, no surprises
   [[ chapter outline:
      begin with thesis statement (<=1.5 pages),
      short intro to GT, assumptions (migratory vs "gradual"),
      contributions that led to thesis, see above, co-influences,
      chapters outline ]]
2. performance analysis method (goal: fair relative perf, ....)
   - systematic, data collection
     - sampling,
   - visualization
   - apply to TR & retic, validate method
     - suggestion: order of mag. difference, intrinsic!, different shape/curve
     - question: can port transient to racket & reproduce?
3. design analysis method
   - thesis: use standard theory + some new theory to organize design space,
   - tools:
     - parameterized TS
     - complete monitoring
     - blame S + C
   - apply to TR, Retic, Amnesic, Erasure, ....
4. transient racket, implementation (transient + untyped Racket, that's all, can be standalone paper)
   (inherit types + checker from TR but nothing more)
   scale transient to real language, richer type system, 10x gone,
   - theory generalization, to racket types
   - eng. adaptation, to racket lang
   - perf. properties
   - blame, more accessors + manipulators, compromises, huge allocation,
     circularity type at runtime
5. natural + transient + untyped
   (meaning: what are we giving to programmers)
   - simple model
     - no worries, properties still hold
     - failed attempts at natural/transient cooperation to reduce checks,
       possible futures (forgetful)
   - expressiveness
     - (Syntaxof (-> Int Int)) ... new mixed programs that TR doesn't allow
   - implementation
     - require/untyped-contract
     - define-typed/untyped-id
     - ....
   - evaluation
     - ?? 2-way lattice? 3-way
     - ?? programs where mix is better than natural-only or transient-only
     - threats: no blame in transient, 
6. related work soup
   chap 2 - evaluation method
   chap 3 - GTT, KafKa, gradual guarantee?, other analysis
   chap 4 - other impls of transient
   chap 5 - (new)
7. new questions
  - transient + JIT, really fast!? on Pycket, apples-to-apples AOT Racket vs JIT
  - static analysis for transient checks, minimize ala Henglein?
  - transient + blame, practical alternative?
  - need wrappers? forgetful/amnesic instead of transient
8. conclusion
  - summarize chapters, what did
  - revisited fundamental questions (TS, what do types mean at runtime)






