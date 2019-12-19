premodel
===

Early thoughts on the model, Natural + Transient

- [X] simple 3-semantics (simple.tex),
  - [X] draft
  - [X] prove US, TS, CM
  - [X] typeset
- [ ] add / keep boundary info ... can we use to remove checks?
  - [ ] draft
  - [ ] prove US TS CM
  - [ ] typeset

Idea (2019-12-04):
---

Can easily collapse Transient and Untyped ... add "nothing" to the model
 and the semantics can run smoothly.

- Need a syntactic boundary for each of the three langs, but the
  semantics shouldn't really need to know whats inside.

- HMPH our old CM results break down if self-edges are allowed, because
  then a value can have multiple owners ... need a richer notion of CM to make
  meaningful statement (Option Contracts relate)
