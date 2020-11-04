#lang greenman-thesis

Researchers have developed languages that
 allow typed and untyped code to interoperate.
Every mixed-typed language, however, is a compromise along three dimensions:
 expressive interactions, type guarantees, and performance.
Prior work does not rigorously assess the tradeoffs;
 in fact, each mixed-typed idea is tangled in the details of its
 proof-of-concept language.
The design space is a zoo.

In this dissertation, I introduce methods to compare
 mixed-typed languages and bring order to the design space.
One set of methods quantifies performance across exponentially-many program
 configurations.
A second set articulates the guarantees that static types can provide in
 a mixed setting.
Together, the methods characterize the strengths and weaknesses of language designs.

The comparison does not reveal a clear winner, and therefore motivates
 a synthesis of two ideas from the literature: @|sdeep| and @|sshallow|.
@|sDeep| types offer strong guarantees but impose a high cost on communications
 that span type boundaries.
@|sShallow| types come with weak guarantees but can be implemented with
 relatively low and predictable costs.
I prove that @|sdeep| and @|sshallow| types can interoperate, and show that
 a three-way mix often improves performance.

