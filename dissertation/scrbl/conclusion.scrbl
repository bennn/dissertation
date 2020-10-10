#lang greenman-thesis/include

@title[#:tag "chap:conclusion"]{Conclusion}

@|sDeep| and @|sshallow| types can interoperate,
 both in theory and in a practical implementation, and the synthesis brings
 measurable benefits.
The benefits improve all three main dimensions of a mixed-typed programming:
@exact{{\renewcommand{\labelitemi}{{\large\decothreeright}} %{\raisebox{0.4ex}{\tiny\(\bullet\)}}
\begin{itemize}
\item \emph{Proofs}\/:
  Switching from \sshallow{} to \sdeep{} types strengthens the formal guarantees
   for a block of code.
  In Typed Racket, changing one line of code improves types from local spot-checks
   to claims that hold throughout the program.
\item\emph{Performance}\/:
  Switching between \sdeep{} to \sshallow{} can improve performance.
  In fully-typed programs, \sdeep{} types have zero cost---and often run faster
   due to type-directed optimizations.
  In mixed-typed programs, \sshallow{} avoids the tremendous overheads
   of \sdeep{} type boundaries.
\item \emph{People}:
  \sShallow{} types can express new combinations of typed and untyped code
   because they enforce weaker guarantees.
  Programmers can choose between this flexibility and the stability
   of \sdeep{} types as they see fit, for each part of a codebase.
\end{itemize}}}
@|noindent|Integrating @|sdeep| and @|sshallow| within one codebase---as
 opposed to picking one or the other---improves several concrete examples (@sectionref{sec:both:perf:both}).
For now, I recommend @|sshallow| types when initially converting an untyped
 program.
Once the types are in place and the boundaries are clear, then consider
 moving from @|sshallow| to @|sdeep|.
@; Further experience may suggest other best practices.

Beyond the central contribution, this dissertation introduces methods to
 systematically measure any mixed-typed language.
The performance methods in @chapter-ref{chap:performance} offer a comprehensive
 and scalable picture of run-time costs.
Any multi-language system with performance implications can apply the methods
 to learn about its exponentially-large space of configurations.
The design methods in @chapter-ref{chap:design} rigorously assess the strengths
 and weaknesses of static types.
Our application of these methods leads to an unexcelled characterization
 of different designs from the literature.
@; Thanks to the syntactic nature of the proof techniques, I can recommend
@;  the theorems to other designers.

Overall, we have learned a lot about what it means to offer a practical
 mixed-typed language.
The step from @emph{untyped or typed} to @emph{mixed-typed} has presented a
 serious challenge to the conventional wisdom about static types.
Techniques that led to strong guarantees and fast performance in a fully-typed
 setting now give weaker guarantees and poor running times.
Methods and measurements have improved our understanding of the design space
 and articulated the benefits of combining strong (@|sdeep|) and weak (@|sshallow|)
 types in a mixed-typed language.
Yet much remains to be done, especially to see how programmers comprehend
 and use the new freedom.


@; -----------------------------------------------------------------------------
@exact|{\bibliography{bg}}|
