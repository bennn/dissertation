#lang greenman-thesis/include
@(require greenman-thesis/oopsla-2019/main)

@title[#:tag "sec:design:conclusion"]{Discussion}

One central design issue of a mixed-typed language is the semantics of
 types and specifically how their integrity is enforced at the boundaries between typed and untyped code.
Among other things, the choice determines whether typed code can trust
 the static types and the quality of assistance that a programmer receives
 when a mixed-typed interaction goes wrong.
Without an interaction story, mixed-typed program are no better than
 dynamically-typed programs.
Properties that hold for the typed half of the language are only valid
 under a closed-world assumption@~citep{bat-ecoop-2014,rsfbv-popl-2015,cvgrl-oopsla-2017};
 such properties are an important starting point, but make no contribution to
 the end goal.

As the analysis of this paper demonstrates, the limitations of
 the host language determine the invariants that a language designer can hope to enforce.
Higher-order wrappers enable strong guarantees, but need support from the host
 runtime system.
For example, Typed Racket is a mature language but
 lacks wrappers for certain higher-order values.
A language without wrappers of any sort can provide weaker guarantees by
 rewriting typed code and maintaining a global map of blame metadata.
If this metadata can be attached directly to a value, then stronger
 blame guarantees are in reach.

More precisely, this paper analyzes six distinct semantics via four properties (@tableref{tbl:technical})
 and establishes an error preorder relation:

@itemlist[
  @item{
    Type soundness is a relatively weak property for mixed-typed programs;
     it determines whether typed code can trust its own types.
    Except for the @|ename| semantics, which does nothing to enforce types,
     type soundness does not clearly distinguish the various strategies (@exact{\remarkref{TS-not-enough}}).

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
   Four strategies satisfy blame soundness relative to a path-based notion of
    responsibility.
   @|tname| satisfies blame soundness only if the notion of responsibility is weakened to
    merge distinct references to the same heap-allocated value.
   @|ename| is trivially blame-sound because it gives the programmer zero information.

  }@item{
   Blame completeness states that every blame error comes with an
    overapproximation of the responsible parties.
   Three of the four blame-sound semantics also satisfy blame completeness.
   @|fname| can be modified to satisfy this property.
   The @|ename| strategy trivially fails blame completeness.
   And the @|tname| strategy fails because it has no way to supervise untyped
   values that flow through a typed context.
  }]

@|noindent|@Tableref{tbl:technical} notes, however, that the weakest strategies
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
     $\dagger$ & {indirectly satisfies $\propts{\sidproj}$ by a bisimulation to $\ascr{}$ (\theoremref{thm:TAsim})}\!\!\!\!
     \\
     $\ddagger$ & {satisfiable by adding $\ascr{}$-style trace wrappers, see supplement}\!\!\!\!
     \end{tabular}
\end{table}
}|

The design of a semantics of type enforcement has implications for two
other, major aspects of the design of a mixed-typed language: the
performance of its implementation and its acceptance by working developers.
@citet{gtnffvf-jfp-2019} developed an evaluation framework for the
performance concern that is slowly gaining in acceptance,
@citet{tgpk-dls-2018} present rather preliminary results concerning the
acceptance by programmers.
In conclusion, though, all three problem areas are barely understood.
Much remains to be done before the community can truly claim to understand this
complex design space.

@; -----------------------------------------------------------------------------

@exercise[2]{
  Design a semantics, @exact{\xsym}, that eagerly checks pairs like @|nname|
   and wraps/unwraps functions like @|fname|.
  Prove that @exact{\xsym} does not satisfy complete monitoring,
   but can satisfy blame soundness and completeness.
}

@futurework{
  Formulate a variant of complete monitoring that distinguishes the @exact{\xsym}
   semantics of the previous exercise from the @|nname| semantics.
}

@futurework{
  Rephrase complete monitoring in terms of types and observable behaviors
   (instead of syntactic judgments).
  @; What are the advantages of your semantic theorem relative to the current
  @;  syntactic one?
  @; Any ideas to simplify / economize the syntactic theorem?
}

@futurework{
  The error preorder (@${\sbehavioreq}) looks a lot like the term precision
   relation (@${\sqle}) from the gradual typing literature@~cite{svcb-snapl-2015,nla-popl-2019}.
  To investigate whether there is a deeper connection,
   use the @|nname| and @|fname| semantics to design two compilers into a core
   language that satisfies graduality.
  @exact{\kafka} may be a good starting point@~cite{clzv-ecoop-2018}.
  Prove that the @|fname| compiler always gives less-precise expressions according
   to the term precision relation.
  Can core-language term precision be used to indirectly prove the surface-language error preorder?
}

