#lang greenman-thesis

@; "interop" > "coexist", less danger of a side-by-side interpretation

A type system can be a support or a straightjacket, depending on how your code
 fits into its rigid constraints.
Fortunately, researchers have developed mixed-typed languages that
 allow typed and untyped code to interoperate.
Unfortunately, every mixed language is a new compromise among three dimensions:
 expressive interactions, type guarantees, and performance.
Prior work does not rigorously assess the tradeoffs;
 in fact, each mixed-typed idea is tangled in the details of its
 proof-of-concept language.
The design space is a zoo.

In this dissertation, I introduce methods to compare
 mixed-typed languages and bring order to the design space.
One family of methods quantifies performance across the
 exponentially-many ways that a program can mix typed and untyped code.
A second family relates type guarantees using parameterized
 theorems.
Given two equally-expressive languages, the methods reveal strengths and weaknesses.

The comparison does not reveal a clear winner, and therefore motivates
 a synthesis of two ideas from the literature: @|sdeep| and @|sshallow|.
@|sDeep| types offer strong guarantees, but impose a high cost on communications
 that span type boundaries.
@|sShallow| types come with weak guarantees but can be implemented with low and
 predictable costs.
I prove that @|sdeep| and @|sshallow| types can interoperate, and validate
 the concept with a full-fledged implementation.

