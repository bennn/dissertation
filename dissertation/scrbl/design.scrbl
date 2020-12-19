#lang greenman-thesis/include
@(require greenman-thesis/oopsla-2019/main)

@title[#:tag '("sec:design:introduction" "chap:design")]{Design Analysis Method}
@jointwork[
  #:people* '(
    "Christos Dimoulas"
    "Matthias Felleisen"
  )
  #:paper* '("gf-icfp-2018" "gfd-oopsla-2019" "gdf-jfp-2020")
]

Over the years, researchers have developed several languages that mix typed and untyped code.
Typed Racket and Reticulated are but two implementations in a wide space.
To a first approximation, the designs fall into four broad strategies:

@itemlist[
  @item{
    @emph{Optional} typing adds a best-effort static analysis but
     ignores type annotation at runtime@~citep{bg-oopsla-1993,bat-ecoop-2014}.
  } @item{
    @emph{Transient} inserts shape checks in type-checked code
     to guarantee that operations cannot not ``go wrong'' due to
     untyped values@~citep{vss-popl-2017,v-thesis-2019}.
    A shape check validates a top-level value constructor with respect to a
     top-level type constructor, which is just enough for this notion of safety.
  } @item{
    @emph{Natural} enforces types with higher-order checks
     and thereby ensures the full integrity of types@~citep{st-sfp-2006,tf-dls-2006}.
  } @item{
    @emph{Concrete} requires that every value is tagged with a type and
    maintains integrity with simple checks@~citep{wzlov-popl-2010, mt-oopsla-2017}.
}]
@|noindent|In addition, though, researchers have proposed and implemented
 hybrid techniques@~citep{g-popl-2015, svctg-esop-2015, gf-icfp-2018, bfnorsvw-oopsla-2009, rzv-ecoop-2015}.
An outstanding and unusual exemplar
 of this kind is Pyret, a language targeting the educational realm (@shorturl["https://www." "pyret.org"]).

Each of these type-enforcement strategies picks a tradeoff among
 static guarantees, expressiveness, and run-time costs (@sectionref{sec:design:jungle}).
If stringent constraints on untyped code are acceptable, 
 then @emph{concrete} offers strong and inexpensive guarantees.
If the goal is to interoperate with an untyped language that
 does not support wrapper/proxy values,
 then @emph{transient} may offer the strongest possible guarantees.
If performance is not an issue,
 then @emph{natural} is the perfect choice.

Unfortunately, the literature provides little guidance to programmers and
 language designers on how to compare different semantics.
Standard meta-theoretical tools do not articulate what is gained
 and lost in each tradeoff (@sectionref{sec:design:properties}).
The gradual guarantee@~citep{svcb-snapl-2015}, for example, is trivially
 satisfied by any optionally-typed language.
Simply put, the field lacks an apples-to-apples way of comparing different
 type-enforcement strategies and considering their implications for programmers.

This chapter introduces a framework for systematically comparing the behavioral
 guarantees offered by different mixed-typed semantics.
Because each semantics is essentially a method of enforcing static types,
 the comparison begins with a common mixed-typed syntax.
This surface syntax is then assigned multiple semantics,
 each of which follows a distinct protocol for enforcing type specifications.
With this semantic framework, one can directly observe the possible behaviors
 for a single program.

The chosen models illustrate @emph{natural} (@${\nscr}), @emph{transient} (@${\tscr}),
 @emph{optional} (also known as @emph{erasure}, @${\escr}),
 and three theoretical strategies (@emph{co-natural} @${\cscr}, @emph{forgetful} @${\fscr}, and @emph{amnesic} @${\ascr}) that demonstrate how to fill design gaps.
The comparison excludes two classes of prior work:
 @emph{concrete}, because of the constraints it places on untyped code (@sectionref{sec:design:anti-concrete}),
 and mixed-typed languages that must analyze untyped code to interoperate with it.
Our focus is on strategies that can deal with untyped code
 as a ``dusty deck'' without needing to recompile the untyped world each time a new type boundary appears.

@exact|{
\begin{table}[t]
  \caption{Informal sketch of the design-space analysis.}
  \label{tbl:contributions}

  {\deftablemacros{}
   \hfill\(\begin{array}{l@{\qquad}c@{\hsep}c@{\hsep}c@{\hsep}c@{\hsep}c@{\hsep}c}
     & \nscr\hphantom{\LE} & \cscr\hphantom{\LE} & \fscr\hphantom{\LE} & \tscr\hphantom{\EQ} & \ascr\hphantom{\LE} & \escr
     \\[1.0ex] \textrm{type soundness}
        &     \tblY &     \tblY &     \tblY &     \tblY &     \tblY &     \tblN
     \\ \textrm{complete monitoring}
        &     \tblY &     \tblY &     \tblN &     \tblN &     \tblN &     \tblN
     \\ \textrm{blame soundness}
        &     \tblY &     \tblY &     \tblY &     \tblN &     \tblY &     \tblY
     \\ \textrm{blame completeness}
        &     \tblY &     \tblY &     \tblN &     \tblN &     \tblY &     \tblN
     \\ \textrm{error preorder}
     & \nscr\LE & \cscr\LE & \fscr\LE & \tscr\EQ & \ascr\LE & \escr
   \end{array}\)\hfill}
\end{table}
}|

@Tableref{tbl:contributions} sketches the results of the evaluation (@sectionref{sec:design:technical}).
The six letters in the top row correspond to different type-enforcement strategies,
 and thus different semantics, for the common surface language.
As to be expected, @|nname| (@${\nscr}) accepts the fewest programs without raising a run-time
 type mismatch, and @|ename| (@${\escr}) accepts the greatest number
 of programs;
 the symbols @${\sbehaviorle} and @${\sbehavioreq} indicate these
 behavioral differences.
Lower rows introduce additional properties that underlie our comparison.
Type soundness guarantees the validity of types in typed code.
Complete monitoring guarantees that the type system moderates all boundaries between
 typed and untyped code---even boundaries that arise at run-time.
Blame soundness ensures that when a run-time check goes wrong, the error message
 points to boundaries that are relevant to the problem.
Blame completeness guarantees that error messages come with @emph{all} relevant
 information.
For both blame soundness and completeness, @emph{relevance} is determined by an
 independent (axiomatic) specification that tracks values as they cross
 boundaries between typed and untyped code (@sectionref{sec:design:laws}).

In sum, the five properties enable a uniform analysis of existing strategies
 and can guide the search for new strategies. Indeed, the synthetic
 @|aname| semantics (@${\ascr}) demonstrates how a semantics can
 fail complete monitoring but guarantee sound and complete blame.


@section{Chapter Outline}

Chapters @exact{\ref{sec:design:jungle}} through @exact{\ref{sec:design:basic}} explain the
 @emph{what}, @emph{why}, and @emph{how} of our design-space analysis.
There is a huge body of work on mixed-typed language that desperately
 needs organizing principles (@sectionref{sec:design:jungle}).
Past attempts to organize fall short; by contrast,
 the properties that frame @tableref{tbl:contributions} offer an expressive and scalable
 basis for comparison (@sectionref{sec:design:properties}).
These properties guide an apples-to-apples method that begins
 with a common surface language and studies different semantics (@sectionref{sec:design:basic}).
In particular, this chapter analyzes six semantics based on six ideas for
 enforcing static types.

@Sectionref{sec:design:technical} presents the six semantics and the key results.
Expert readers may wish to begin there and refer back to @sectionref{sec:design:basic}
 as needed.
An appendix contains a complete formal account of our results.



@; -----------------------------------------------------------------------------

@include-section{oopsla-2019/jungle.scrbl}
@include-section{oopsla-2019/properties.scrbl}
@include-section{oopsla-2019/basic.scrbl}
@include-section{oopsla-2019/technical.scrbl}
@include-section{oopsla-2019/conclusion.scrbl}

