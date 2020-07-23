#lang greenman-thesis/include

@title{Conclusion}

Summarize previous chapters and whats done.

Work revisits fundamental questions, type soundness and what do types mean
 for behavior.

@; another step along the road to practical migratory typing,
@; story so far .... expressiveness, types,
@; added performance, found need to gradual runtimes
@;
@; 


Perhaps the most pressing questions is how to measure the costs and benefits
 of blame.
Guarded blame clearly imposes a huge cost; getting blame right requires
 careful allocation of wrappers.
Settling for a weaker guarantee would solve the space-efficiency problem.
But without knowing how blame benefits programmers, and how their work might
 suffer without precise blame information, attempts in either direction are
 misguided.
Transient blame also imposes a huge cost.
Does it help programmers more than, say, a stack trace?
Can it be weakened to a useful and reasonably-expensive method?
Finding ways to address these user-facing questions is imperative.


@; -----------------------------------------------------------------------------
@exact|{\bibliography{bg}}|
