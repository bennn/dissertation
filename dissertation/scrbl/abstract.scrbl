#lang greenman-thesis

The design space of mixed-typed languages is lively but disorganized.
On one hand, researchers across academia and industry have contributed language
 designs that allow typed code to interoperate with untyped code.
These designs explore a range of goals;
 some improve the expressiveness of a typed language, and
 others strengthen untyped code with a tailor-made type system.
On the other hand, experience with type-sound designs has revealed major challenges.
We do not know how to measure the performance costs of sound interaction.
Nor do we have criteria for ``true'' mixed-typed soundness; indeed,
 a normal type soundness theorem says nothing about whether untyped code
 can trust the types.

In this dissertation, I introduce methods to compare
 mixed-typed languages and bring order to the design space.
My first contribution is a performance-analysis method that systematically
 measures the cost of mixed-typed interaction.
The method scales to large programs and comes with a visualization that
 provides a concise summary.

My second contribution is a design-analysis method that can distinguish the
 various sound designs in the literature.
The central question of this method is the extent to which types affect
 higher-order interactions.
Further distinctions arise by asking whether the language can direct a programmer
 to potentially-faulty interactions.

The design and performance comparison does not reveal a clear winner,
 and therefore motivates a synthesis of two ideas from the literature:
 @|sdeep| and @|sshallow|.
@|sDeep| types offer strong guarantees but impose a high cost on interactions.
@|sShallow| types come with weak guarantees but can be implemented with
 relatively low and predictable costs.
I prove that @|sdeep| and @|sshallow| types can interoperate and show that
 a three-way mix is often desirable.

