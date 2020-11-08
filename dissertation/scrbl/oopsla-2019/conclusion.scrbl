#lang greenman-thesis/include
@(require greenman-thesis/oopsla-2019/main)

@title[#:tag "sec:design:conclusion"]{Discussion}

One central design issue of a mixed-typed language is the semantics of
 types and specifically how their integrity is enforced at the boundaries between typed and untyped code.
Among other things, the choice determines whether typed code can trust
 the static types and the quality of assistance that a programmer receives
 when a mixed-typed interaction goes wrong.
Without an interaction story, mixed-typed programs are no better than
 dynamically-typed programs when it comes to run-time errors.
Properties that hold for the typed half of the language are only valid
 under a closed-world assumption@~citep{bat-ecoop-2014,rsfbv-popl-2015,cvgrl-oopsla-2017};
 such properties are an important starting point, but make no contribution to
 the overall goal.

As the analysis of this chapter demonstrates, the limitations of
 the host language determine the invariants that a language designer can hope to enforce.
First, higher-order wrappers enable strong guarantees, but need support from the host
 runtime system.
For example, Typed Racket is a mature language but
 lacks wrappers for certain higher-order values.
A language without wrappers of any sort can provide weaker guarantees by
 rewriting typed code and maintaining a global map of blame metadata.
If this metadata can be attached directly to a value, then stronger
 blame guarantees are in reach.

More precisely, this chapter analyzes six distinct semantics via four properties (@tableref{tbl:technical})
 and establishes an error preorder relation:

@itemlist[
  @item{
    Type soundness is a relatively weak property for mixed-typed programs;
     it determines whether typed code can trust its own types.
    Except for the @|ename| semantics, which does nothing to enforce types,
     type soundness does not clearly distinguish the various strategies.

  }@item{
    Complete monitoring is a stronger property, adapted from the literature on
     higher-order contracts@~citep{dtf-esop-2012}.
    It holds when @emph{untyped} code can trust type specifications and vice-versa;
     see @sectionref{sec:design:lying-type} for examples.
  }]

@|noindent|The last two properties tell a developer what aid to expect if a
 type mismatch occurs.

@itemlist[
  @item{
   Blame soundness states that every boundary in a blame message is potentially
    responsible.
   Four strategies satisfy blame soundness relative to a standard, path-based notion of
    responsibility.
   @|tname| satisfies blame soundness only if the notion of responsibility is weakened to
    merge distinct references to the same heap-allocated value.
   @|ename| is trivially blame-sound because it gives the programmer zero type-related information.

  }@item{
   Blame completeness states that every blame error comes with an
    overapproximation of the responsible parties.
   Three of the four blame-sound semantics also satisfy blame completeness.
   @|fname| can be modified to satisfy this property.
   The @|ename| strategy trivially fails blame completeness.
   And the @|tname| strategy fails because it has no way to supervise untyped
   values that flow through a typed context.
  }]

@|noindent|@Tableref{tbl:technical} points out, however, that the weakest strategies
 are the only ones that do not require wrapper values.
Perhaps a future design can strengthen the wrapper-free guarantees.

@exact|{
\begin{table}[t]
  \caption{Technical contributions}
  \label{tbl:technical}

  {\deftablemacros{}
   \newcommand{\addmark}[2]{\hphantom{{}^{#2}}{#1}^{#2}}
   \hfill\(\begin{array}{l@{\quad}c@{\hsep}c@{\hsep}c@{\hsep}c@{\hsep}c@{\hsep}c}
     & \nscr\LE & \cscr\LE & \fscr\LE & \tscr\EQ & \ascr\LE & \escr
     \\[1.0ex] \textrm{type soundness}
        &     \TSfull &     \TSfull &     \TSfull &     \addmark{\TStag}{\dagger} &     \TSfull &     \TSnone
     \\ \textrm{complete monitoring}
        &     \tblY &     \tblY &     \tblN &     \tblN &     \tblN &     \tblN
     \\ \textrm{blame soundness}
        &     \Bpath &     \Bpath &     \Bpath           &     \Bheap &     \Bpath &     \emptyset
     \\ \textrm{blame completeness}
        &     \Bpath &     \Bpath &     \addmark{\tblN}{\ddagger} &     \tblN &     \Bpath &     \tblN
     \\[1ex] \textrm{no wrappers}
        &      \tblN &      \tblN &      \tblN &      \tblY &      \tblN &      \tblY
   \end{array}\)\hfill}

   \medskip
   \hfill
     \begin{tabular}{c@{~~}l}
     $\dagger$ & {note that $\tscr{}$ is bisimilar to $\ascr{}$ (\theoremref{thm:TAsim})}
     \\
     $\ddagger$ & {satisfiable by adding $\ascr{}$-style trace wrappers, see appendix}\!\!\!\!
     \end{tabular}
\end{table}
}|

