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
  In Typed Racket, a one-line change thus improves types from local spot-checks
   to claims that hold throughout the program, including in untyped modules.
\item\emph{Performance}\/:
  Flipping between \sdeep{} and \sshallow{} can improve performance.
  In fully-typed programs, \sdeep{} types have zero cost---and often run faster
   due to type-directed optimizations.
  In mixed programs, \sshallow{} avoids the tremendous overheads
   of \sdeep{} type boundaries.
\item \emph{People}:
  \sShallow{} types can express new combinations of typed and untyped code
   because they enforce weaker guarantees.
  Programmers can choose between this flexibility and the stability
   of \sdeep{} types as they see fit, for each part of a codebase.
\end{itemize}}}
@|noindent|Integrating @|sdeep| and @|sshallow| within one codebase---as
 opposed to picking one or the other---improves several concrete examples (@sectionref{sec:both:perf:both}).
These examples all use @|sshallow| types for code that is tightly coupled to
 an untyped boundary and @|sdeep| types everywhere else.
More experience is likely to reveal other patterns and best practices.
For now, I recommend @|sshallow| types when initially converting an untyped
 program.
Once the types are in place and the boundaries are clear,
 then moving from @|sshallow| to @|sdeep| may assist with debugging tasks
 and may improve performance.

The foundations of this work are the methods that I developed to systematically
 measure mixed-typed languages.
@itemlist[#:style 'ordered
@item{
The performance evaluation methods from (@chapter-ref{chap:performance}) offer a comprehensive
 and scalable picture of run-time costs.
An exhaustive method summarizes the complete dataset when feasible, and an
 approximate method gives an empirically-justified weakening otherwise.
}
@item{
The design evaluation method (@chapter-ref{chap:design}) rigorously assess the strengths
 and weaknesses of static types.
Our application of this method leads to the most precise
 characterization of designs in the literature.
}]

Overall, my dissertation brings us closer to useful mixed-typed languages.
The step from @emph{untyped-or-typed} to @emph{mixed-typed} has presented a
 serious challenge to the conventional wisdom about static types.
Standard techniques that realize strong guarantees and fast performance in a fully-typed
 setting yield weaker guarantees and slower running times in mixed programs.
In the words of one anonymous Racket survey respondent, mixed languages ``seemed to combine the
 best of both worlds .... but in practice seem to combine mainly the
 downsides'' because of friction between static and dynamic typing.
Methods and measurements have improved our understanding of the design space
 and articulated the benefits of mixing @|sdeep| and @|sshallow|
 types to soften the edges.
With both styles available, programmers can avoid severe performance and
 expressiveness issues.
Yet much remains to be done, especially to see how programmers comprehend
 the new types and leverage the new choices.


@; future ....
@;  @subsection{Design Ideas for Shapes}
@; TODO cite "The VM Already Knew That" here? @~cite[rat-oopsla-2017]
@;  
@;  Clearly, the move from type tags to general shape checks raises
@;   a new question for transient semantics.
@;  One type @${\stype} may have several possible shapes, and the language
@;   designer needs to choose one representative that strikes a balance
@;   between guarantees and performance.
@;  Consider an uncurried function type @${(\tfun{\stype_d \ldots}{\stype_c})}
@;   that expects a fixed number of arguments and promises one result.
@;  A shape cannot enforce the higher-order aspects of this type,
@;   but there are still at least two ways to enforce first-order properties in
@;   an instance such as @${(\tfun{\tint \tint}{\tint})}:
@;   simply check for a procedure,
@;   or check for a procedure that accepts two inputs.
@;  The latter design may incur a higher run-time cost, but it offers stronger
@;   guarantees for programmers and for type-directed optimizations.
@;  
@;  The choice problem calls for a guiding design.
@;  Our implementation of transient for Typed Racket chooses to enforce
@;   full type constructors with structural checks;
@;   refer to @secref{design-choices} for examples.
@;  This decision is inspired by Reticulated, which enforces constructors in the context
@;   of a simpler type system in which ambiguities rarely arise@~cite[vss-popl-2017].
@;  For example, Reticulated's shapes do not rule out Python arity errors.
@;  Grace has a nominal type system, and is therefore able to equate shape checks
@;   with name checks@~cite[rmhn-ecoop-2019].
@;  Pallene has a structural type system, but limits itself to tag checks@~cite[gi-scp-2020].
