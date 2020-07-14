#lang greenman-thesis/include
@(require greenman-thesis/oopsla-2019/main)

@title[#:tag "sec:design:jungle"]{Assorted Behaviors by Example}

Although every gradual typing system shares a common motivation,
 one and the same program may behave quite differently in the context
 of two different systems.
These behavioral differences stem from alternative views about how to enforce
 types at the boundaries between typed and untyped code.
To understand the existing behaviors and to develop new approaches, one
 must therefore understand the design space of checking strategies.


