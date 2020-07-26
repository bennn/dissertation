#lang greenman-thesis/include
@(require greenman-thesis/oopsla-2019/main)

@; TODO be careful about Natural vs Deep,
@;  Natural = semantics,
@;  Deep = idea = synonym for Nat in other parts of the paper (want Deep, get via Natural)

@title[#:tag "chap:design"]{Design Analysis Method}
@jointwork[
  #:people* '(
    "Christos Dimoulas"
    "Matthias Felleisen"
  )
  #:paper* '("gf-icfp-2018" "gfd-oopsla-2019" "gdf-jfp-2020")
]

Gradual typing comes in many forms:
@itemlist[
@item{
    @emph{Optional} typing adds a best-effort static analysis but
     ignores type annotation at runtime@~cite{s-lisp-1990,bg-oopsla-1993}.
}
@item{
    @emph{Transient} inserts shape checks
     in type-checked code
     to guarantee that operations cannot not ``go wrong'' due to
     untyped values@~cite{vss-popl-2017,v-thesis-2019}.
    A shape check enforces a correpondence between a top-level value
     constructor and the top-level constructor of a type. It generalizes the tag
     checks found in many runtime systems.
}
@item{
    @emph{Natural} enforces types with higher-order checks
     and thereby ensures the full integrity of types@~citep{st-sfp-2006,tf-dls-2006}.
}
@item{
    @emph{Concrete} requires that every value is tagged with a type and
    maintains integrity with simple checks@~cite{wzlov-popl-2010, mt-oopsla-2017}.
}]
In addition, researchers have proposed and implemented
 hybrid techniques@~citep{g-popl-2015, svctg-esop-2015, gf-icfp-2018,
 bfnorsvw-oopsla-2009, rzv-ecoop-2015}; an outstanding and unusual exemplar
 of this kind is Pyret, a language targeting the educational realm (@format-url{https://www.pyret.org}).

Each type-enforcement strategy represents a different tradeoff among guarantees,
 expressiveness, and performance~(@section-ref{sec:design:jungle}).
If stringent constraints on untyped code are acceptable, 
 then @emph{concrete} offers strong and inexpensive guarantees.
If the goal is to preserve the idioms of an existing untyped language that
 does not support proxy values,
 then @emph{transient} may offer the strongest possible guarantees.
If performance is not an issue,
 then @emph{natural} seems a perfect choice.

Unfortunately, the literature provides little guidance to programmers and
 language designers on how to compare these semantics~(@section-ref{sec:design:properties}).
The standard meta-theoretical tools do not articulate what is gained
 and lost in each tradeoff.
Simply put, the field lacks an apples-to-apples way of comparing different
 type-enforcement strategies and considering their implications for programmers.

This chapter introduces a framework for systematically comparing the guarantees
 offered by different semantics of gradual typing.
Because each approach is essentially a method of enforcing static types,
 the comparison begins with a mixed-typed syntax.
This surface syntax is then assigned multiple semantics (@section-ref{sec:design:basic}),
 each of which follows a distinct protocol for enforcing type specifications.
With this semantic framework, one can directly observe the possible behaviors
 for a single program.
The chosen models illustrate @emph{natural}, @emph{transient}, @emph{optional} (henceforth @emph{erasure}),
 and three theoretical strategies (@section-ref{sec:design:strategies}).
The comparison excludes @emph{concrete} because of the
 stringent constraints it places on untyped code (@section-ref{sec:design:anti-concrete}).

@figure*[
  "tbl:design:contributions"
  @elem{Informal summary of contributions}
  @exact|{
  {\deftablemacros{}
   \hfill\(\begin{array}{l@{\qquad}c@{\hsep}c@{\hsep}c@{\hsep}c@{\hsep}c@{\hsep}c}
     & \nscr\LE & \cscr\LE & \fscr\LE & \tscr\EQ & \ascr\LE & \escr
     \\[1.0ex] \textrm{type soundness}
        &     \tblY &     \tblY &     \tblY &     \tblY &     \tblY &     \tblN
     \\ \textrm{complete monitoring}
        &     \tblY &     \tblY &     \tblN &     \tblN &     \tblN &     \tblN
     \\ \textrm{blame soundness}
        &     \tblY &     \tblY &     \tblY &     \tblN &     \tblY &     \tblY
     \\ \textrm{blame completeness}
        &     \tblY &     \tblY &     \tblN &     \tblN &     \tblY &     \tblN
   \end{array}\)\hfill}
  }|
]

@Figure-ref{tbl:design:contributions} sketches the results of the evaluation (@section-ref{sec:design:technical}).
The six letters in the top row correspond to different type-enforcement strategies,
 and thus different semantics, for the common surface language.
As to be expected, @|nname| (@${\nscr}) accepts the fewest programs without raising a run-time
 type mismatch, and @|ename| (@${\escr}) accepts the greatest number
 of programs;
 the symbols @${\sbehaviorle} and @${\sbehavioreq} indicate these
 behavioral differences.
Lower rows introduce formal properties.
Type soundness guarantees the validity of types in typed code.
Complete monitoring guarantees that types moderate all boundaries between
 typed and untyped code---even boundaries that arise at run-time.
Blame soundness ensures that when a run-time check goes wrong, the error message
 points to boundaries that are relevant to the problem.
Blame completeness guarantees that error messages come with @emph{all} relevant
 information.
For both blame soundness and completeness, @emph{relevance} is determined by an
 independent specification of responsibility, which tracks values as they cross
 boundaries between typed and untyped code. 

In sum, the five properties enable a uniform analysis of existing strategies
 and can guide the search for new strategies. Indeed, the synthetic
 @|aname| semantics (@${\asym}) is designed to demonstrate how a particular
 open column in the above table can be filled.


@; -----------------------------------------------------------------------------

@include-section{oopsla-2019/jungle.scrbl}
@include-section{oopsla-2019/properties.scrbl}
@include-section{oopsla-2019/basic.scrbl}
@include-section{oopsla-2019/strategies.scrbl}
@include-section{oopsla-2019/technical.scrbl}
@include-section{oopsla-2019/conclusion.scrbl}



