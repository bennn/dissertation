#lang greenman-thesis

The design space of mixed-typed languages is lively but disorganized.
On one hand, researchers across academia and industry have contributed language
 designs that allow typed code to interoperate with untyped code.
These design efforts explore a range of goals;
 some improve the expressiveness of a typed language, and
 others strengthen untyped code with a tailor-made type system.
On the other hand, experience with type-sound designs has revealed major challenges.
We do not know how to measure the performance costs of sound interaction.
Nor do we have criteria that distinguish ``truly sound'' mixed-typed languages
 from others that ignore type obligations in untyped code.

In this dissertation, I introduce methods for assessing
 mixed-typed languages and bring order to the design space.
My first contribution is a performance-analysis method that allows language
 implementors to systematically measure the cost of mixed-typed interaction.

My second contribution is a design-analysis method that allows language designers
 to understand implications of the type system.
The method addresses two central questions: whether typed code can cope with
 untyped values, and whether untyped code can trust static types.
Further distinctions arise by asking whether error outputs can
 direct a programmer to potentially-faulty interactions.

I apply the methods to several designs and discover limitations that motivate
 a synthesis of two ideas from the literature:
 @|sdeep| types and @|sshallow| types.
@|sDeep| types offer strong guarantees but impose a high cost on interactions.
@|sShallow| types come with weak guarantees but lower worst-case costs.
This dissertation proves that @|sdeep| and @|sshallow| types can interoperate
 and shows that a three-way mix is often desirable.


Touch! 
