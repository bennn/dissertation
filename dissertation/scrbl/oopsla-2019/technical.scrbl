#lang greenman-thesis/include
@(require greenman-thesis/oopsla-2019/main)

@title[#:tag "sec:design:technical"]{Technical Development}

@figure*[
  "fig:technical-outline"
  @elem{Map of basic definitions in @sectionref{sec:design:technical}}

  @exact|{
  { %\newcommand{}{}
    \tikzstyle{every node}=[align=center]
    \begin{tikzpicture}
      \node (00) [text width=6em] {Surface stx.\\\sectionref{sec:design:surface-language}};
      \draw [line width=0.50mm] (00.south west)--(00.south east);
      \node (01) [text width=4em,below of=00,yshift=-0.1cm] {\raisebox{-2.5mm}{($\sWT \sexpr : \stype$)}\\{~}};

      \node (00top) [right of=01,xshift=2mm,inner sep=0mm,minimum size=0mm] {};
      \node (00mid) [below of=00top,yshift=-2cm,inner sep=0mm,minimum size=0mm] {};

      \node (10) [text width=7.5em,right of=00,xshift=2.6cm] {Evaluation stx.\\\sectionref{sec:design:tech:eval}};
      \draw [line width=0.50mm] (10.south west)--(10.south east);
      \node (11) [text width=10em,below of=10,yshift=-0.1cm] {Higher Ord. ($\sWTfull\!\sexpr\!:\!\stype$)\\\sectionref{sec:design:tech:eval:HO}};
      \node (12) [text width=8em,below of=11,yshift=-2.00cm] {Flat ($\sWTtag \sexpr : \stag$)\\\sectionref{sec:design:tech:eval:FO}};
      \node (13) [text width=8em,below of=12,yshift=-0.95cm] {Erased ($\sWTnone \sexpr : \tdyn$)\\\sectionref{sec:design:tech:eval:E}};

      \node (20) [right of=10,xshift=10mm] {};
      \node (21) [draw,ellipse,x radius=0mm,below of=20,yshift=-1.1cm,outer sep=2mm] {$({\snredsta},{\snreddyn})$\\\(\!\!\!\!\!\!\)sec.~\ref{sec:design:tech:common-HO}\(\!\!\!\!\!\!\)};

      \node (20mid) [right of=21,xshift=3mm,inner sep=0mm,minimum size=0mm] {};
      \node (20bot) [below of=20mid,inner sep=0mm,minimum size=0mm] {};
      \node (20bot2) [below of=20bot,yshift=1mm,inner sep=0mm,minimum size=0mm] {};
      \node (20bot3) [below of=20bot2,yshift=8mm,inner sep=0mm,minimum size=0mm] {};

      \node (30) [text width=6em,right of=20,xshift=2.0cm] {Semantics\\{~}};
      \draw [line width=0.50mm] (30.south west)--(30.south east);
      \node (31) [text width=6em,below of=30,yshift=-0.1cm] {$\nscr{}$ (${\nredNS}$,${\nredND}$)\\\sectionref{sec:design:tech:natural}};
      \node (32) [text width=6em,below of=31] {$\cscr{}$ (${\nredCS}$,${\nredCD}$)\\\sectionref{sec:design:tech:conatural}};
      \node (33) [text width=6em,below of=32] {$\fscr{}$ (${\nredFS}$,${\nredFD}$)\\\sectionref{sec:design:tech:forgetful}};
      \node (34) [text width=6em,below of=33] {$\tscr{}$ (\wideas{\nredTX$)$}{{\nredFS}$,${\nredFD}}\\\sectionref{sec:design:tech:transient}};
      \node (35) [text width=6em,below of=34] {$\ascr{}$ (${\nredAS}$,${\nredAD}$)\\\sectionref{sec:design:tech:amnesic}};
      \node (36) [text width=6em,below of=35] {$\escr{}$ (\wideas{\nredEX$)$}{{\nredFS}$,${\nredFD}}\\\sectionref{sec:design:tech:erasure}};

      \draw[->,dashed] (00) -- (10);
      \draw[->,dashed] (10) -- (30);

      \draw[black!75!white,->] (01) -- (11);
      \draw[black!75!white] (00top) -- (00mid);
      \draw[black!75!white,->] (00mid) -- (12);
      \draw[black!75!white,->] (00mid) |- (13);
      \draw[black!75!white,->] (11) |- (21);
      \draw[black!75!white,->] (20mid) |- (31);
      \draw[black!75!white,->] (21) -- (32);
      \draw[black!75!white] (20mid) -- (20bot);
      \draw[black!75!white,->] (20bot) -- (33);
      \draw[black!75!white,-] (20bot) -- (20bot2);
      \draw[black!75!white,-] (20bot2) arc(90:270:0.10) (20bot3);
      \draw[black!75!white,->] (20bot3) |- (35);
      \draw[black!75!white,->] (12) -- (34);
      \draw[black!75!white,->] (13) -- (36);

    \end{tikzpicture}}
}|]

This section presents the main technical details of our analysis: the model,
 the six semantics, and the properties that each semantics satisfies.
Because this is a long and intricate section, @figureref{fig:technical-outline} gives an outline.
The discussion begins with one surface syntax (@sectionref{sec:design:surface-language})
 and proceeds with three target languages that can run surface programs (@sectionref{sec:design:tech:eval}).
Each target comes with a target type system; type soundness relates
 surface types to target types.
@Sectionref{sec:design:tech:common-HO} presents notions of reduction that are shared
 among several languages.
The final sections state the six base semantics and their properties.

Several properties depend on a lifted semantics that propagates
 ownership labels in accordance with the guidelines from @sectionref{sec:design:laws}.
This means that the map in @figureref{fig:technical-outline} is only half
 of the formal development; each syntax and semantics has a parallel, lifted version.
@Sectionref{sec:design:surface-language} presents the lifted surface syntax, but other
 sections give only the most important details regarding ownership.
Full definitions appear in the appendix.


@section[#:tag "sec:design:surface-language"]{Surface Syntax, Types, and Ownership}
@latex-label{sec:design:surface-language}

@figure*[
  "fig:surface-language"
  @elem{Surface syntax and typing rules}

  @exact|{
  \lbl{\fbox{\ssurfacelang}}{
    \begin{tabular}{l@{~}l}
      \begin{langarray}
        \sexpr & \BNFeq &
          \svar \mid \sint \mid \snat \mid
          \epair{\sexpr}{\sexpr} \mid
          \efun{\svar}{\sexpr} \mid
        \\ & &
          \efun{\tann{\svar}{\stype}}{\sexpr} \mid
        \\ & &
          \eapp{\stoptional}{\sexpr}{\sexpr} \mid
        \\ & &
          \eunopt{\stoptional}{\sexpr} \mid
        \\ & &
          \ebinopt{\stoptional}{\sexpr}{\sexpr} \mid
        \\ & &
          \edynb{\sbnd}{\sexpr} \mid
          \estab{\sbnd}{\sexpr}
        \\
        \stype & \BNFeq &
          \tint \mid \tnat \mid \tfun{\stype}{\stype} \mid \tpair{\stype}{\stype}
        \\
        \stoptional & \BNFeq &
          \stype \mid \tdyn
      \end{langarray}
      &
      \begin{langarray}
        \sbnd & \BNFeq &
          \obnd{\sowner}{\stype}{\sowner}
        \\
        \sbset & \BNFeq &
          \powerset{\sbnd}
        \\
        \sowner & \BNFeq &
          \textrm{\scountable{} set of names}
        \\
        \sownerlist & \BNFeq &
          \textrm{sequences of names}
        \\
        \stypeenv & \BNFeq &
          \snil \mid \fcons{\tann{\svar}{\stoptional}}{\stypeenv}
        \\
        \sint & \BNFeq & \integers
        \\
        \snat & \BNFeq & \naturals
        \\
        \sunop & \BNFeq &
          \sfst \mid \ssnd
        \\
        \sbinop & \BNFeq &
          \ssum \mid \squotient
      \end{langarray}
    \end{tabular}
  }

  \bigskip
  \lbl{\fbox{$\stypeenv \sWT \sexpr : \stype$}~\missingrules{}}{
    \begin{mathpar}
      \inferrule*{
        \tann{\svar_0}{\stype_0} \in \stypeenv_0
      }{
        \stypeenv_0 \sWT \svar_0 : \stype_0
      }

      \inferrule*{
        \fcons{\tann{\svar_0}{\stype_0}}{\stypeenv_0} \sWT \sexpr_0 : \stype_1
      }{
        \stypeenv_0 \sWT \efun{\tann{\svar_0}{\stype_0}}{\sexpr_0} : \tfun{\stype_0}{\stype_1}
      }

      \inferrule*{
        \stypeenv_0 \sWT \sexpr_0 : \stype_1
        \\\\
        \sDelta(\sunop, \stype_1) \subteq \stype_0
      }{
        \stypeenv_0 \sWT \eunopt{\stype_0}{\sexpr_0} : \stype_0
      }

      \inferrule*{
        \stypeenv_0 \sWT \sexpr_0 : \tfun{\stype_1}{\stype_2}
        \\
        \stypeenv_0 \sWT \sexpr_1 : \stype_1
        \\\\
        \stype_2 \subteq \stype_0
      }{
        \stypeenv_0 \sWT \eapp{\stype_0}{\sexpr_0}{\sexpr_1} : \stype_0
      }

      \inferrule*{
        \stypeenv_0 \sWT \sexpr_0 : \tdyn
      }{
        \stypeenv_0 \sWT \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sexpr_0} : \stype_0
      }

    \end{mathpar}
  }

  \bigskip
  \lbl{\fbox{$\stypeenv \sWT \sexpr : \tdyn$}~\missingrules{}}{
    \begin{mathpar}
      \inferrule*{
        \tann{\svar_0}{\tdyn} \in \stypeenv_0
      }{
        \stypeenv_0 \sWT \svar_0 : \tdyn
      }

      \inferrule*{
        \fcons{\tann{\svar_0}{\tdyn}}{\stypeenv_0} \sWT \sexpr_0 : \tdyn
      }{
        \stypeenv_0 \sWT \efun{\svar_0}{\sexpr_0} : \tdyn
      }

      \inferrule*{
        \stypeenv_0 \sWT \sexpr_0 : \stype_0
      }{
        \stypeenv_0 \sWT \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sexpr_0} : \tdyn
      }

    \end{mathpar}
  }
  }|]

@Figureref{fig:surface-language} presents the syntax and typing judgments for
 the common syntax sketched in @sectionref{sec:design:basic:surface}.
Expressions @${\sexpr} include variables, integers, pairs, functions,
 primitive operations, applications, and boundary expressions.
The primitive operations consist of pair projections
 and arithmetic functions, to model interactions with a runtime system.
A @${\sdyn} boundary expression embeds a dynamically-typed expression into
 a statically-typed context,
 and a @${\ssta} boundary expression embeds a typed expression in an untyped context.

A type specification @${\stoptional} is either a static type @${\stype}
 or the symbol @${\tdyn} for untyped code.
Fine-grained mixtures of @${\stype} and @${\tdyn}, such as @${\tpair{\tint}{\tdyn}},
 are not permitted; the model describes two parallel syntaxes
 that are connected through boundary expressions (@sectionref{sec:design:basic:surface}).
A statically-typed expression @${\sexpr_0} is one where the judgment
 @${\stypeenv_0 \sWT \sexpr_0 : \stype_0} holds for some type environment
 and type.
This judgment depends on a standard notion of subtyping (@${\subteq}) that is
 based on the relation @${\tnat \subteq \tint}, is covariant for pairs and
 function codomains, and is contravariant for function domains.
The metafunction @${\sDelta} determines the output type of a primitive operation.
For example the sum of two natural numbers is a natural (@${\sDelta(\ssum, \tnat, \tnat) = \tnat})
 but the sum of two integers returns an integer.
A dynamically-typed expression @${\sexpr_1} is one for which
 @${\stypeenv_1 \sWT \sexpr_1 : \tdyn} holds for some environment.

Every function application and operator application comes with a type
 specification @${\stoptional} for the expected result.
These annotations serve two purposes:
 to determine the behavior of the @|tname| and @|aname| semantics,
 and to tell apart statically-typed and dynamically-typed redexes.
An implementation could easily infer valid annotations.
The model keeps them explicit to easily formulate examples where subtyping
 affects behavior; for instance, the source-language terms
 @${\eunopt{\tnat}{\sexpr_0}} and
 @${\eunopt{\tint}{\sexpr_0}} may lead to different run-time checks.


@figure*[
  "fig:surface-ownership"
  @elem{Ownership syntax and single-owner consistency}

  @exact|{
    \lbl{\fbox{Ownership Syntax}}{
      \begin{langarray}
        \sexpr & \BNFeq &
          \svar \mid \sint \mid \snat \mid
          \epair{\sexpr}{\sexpr} \mid
          \efun{\svar}{\sexpr} \mid
          \efun{\tann{\svar}{\stype}}{\sexpr} \mid
          \eapp{\stoptional}{\sexpr}{\sexpr} \mid
          \eunopt{\stoptional}{\sexpr} \mid
        \\ & &
          \ebinopt{\stoptional}{\sexpr}{\sexpr} \mid
          \edynb{\sbnd}{\obars{\sexpr}{\sowner}} \mid
          \estab{\sbnd}{\obars{\sexpr}{\sowner}} \mid
          \obars{\sexpr}{\sowner}
        \\
        \sowner & \BNFeq &
          \textrm{\scountable{} set}
        \\
        \sownerenv & \BNFeq &
          \snil \mid \fcons{\tann{\svar}{\sowner}}{\sownerenv}
      \end{langarray}
    }

  \bigskip
    \lbl{\fbox{$\fwellformed{\sexpr}{\stoptional}$}}{\(\begin{array}{l} % well-formed expression
      \fwellformed{\obars{\sexpr_0}{\sowner_0}}{\stype_0}
      \\\mbox{\quad if}~\,\sowner_0 \sWLsingle \obars{\sexpr_0}{\sowner_0}
        \mbox{ and } \cdot \sWT \sexpr_0 : \stype_0
      \\
      \fwellformed{\obars{\sexpr_0}{\sowner_0}}{\tdyn}
      \\\mbox{\quad if}~\,\sowner_0 \sWLsingle \obars{\sexpr_0}{\sowner_0}
        \mbox{ and } \cdot \sWT \sexpr_0 : \tdyn
    \end{array}\)}

  \bigskip
  \lbl{\fbox{$\sownerenv; \sowner \sWLsingle \sexpr$}}{\begin{mathpar}
    \inferrule*{
      \tann{\svar_0}{\sowner_0} \in \sownerenv_0
    }{
      \sownerenv_0; \sowner_0 \sWLsingle \svar_0
    }

    \inferrule*{
    }{
      \sownerenv_0; \sowner_0 \sWLsingle \sint_0
    }

    \inferrule*{
      \fcons{\tann{\svar_0}{\sowner_0}}{\sownerenv_0}; \sowner_0 \sWLsingle \sexpr_0
    }{
      \sownerenv_0; \sowner_0 \sWLsingle \efun{\svar_0}{\sexpr_0}
    }

    \inferrule*{
      \fcons{\tann{\svar_0}{\sowner_0}}{\sownerenv_0}; \sowner_0 \sWLsingle \sexpr_0
    }{
      \sownerenv_0; \sowner_0 \sWLsingle \efun{\tann{\svar_0}{\stype_0}}{\sexpr_0}
    }

    \inferrule*{
      \sownerenv_0; \sowner_0 \sWLsingle \sexpr_0
      \\
      \sownerenv_0; \sowner_0 \sWLsingle \sexpr_1
    }{
      \sownerenv_0; \sowner_0 \sWLsingle \epair{\sexpr_0}{\sexpr_1}
    }

    \inferrule*{
      \sownerenv_0; \sowner_0 \sWLsingle \sexpr_0
    }{
      \sownerenv_0; \sowner_0 \sWLsingle \eunopt{\stoptional}{\sexpr_0}
    }

    \inferrule*{
      \sownerenv_0; \sowner_0 \sWLsingle \sexpr_0
      \\
      \sownerenv_0; \sowner_0 \sWLsingle \sexpr_1
    }{
      \sownerenv_0; \sowner_0 \sWLsingle \ebinopt{\stoptional}{\sexpr_0}{\sexpr_1}
    }

    \inferrule*{
      \sownerenv_0; \sowner_0 \sWLsingle \sexpr_0
      \\
      \sownerenv_0; \sowner_0 \sWLsingle \sexpr_1
    }{
      \sownerenv_0; \sowner_0 \sWLsingle \eapp{\stoptional}{\sexpr_0}{\sexpr_1}
    }

    \inferrule*{
      \sownerenv_0; \sowner_1 \sWLsingle \sexpr_0
    }{
      \sownerenv_0; \sowner_0 \sWLsingle \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sexpr_0}
    }

    \inferrule*{
      \sownerenv_0; \sowner_1 \sWLsingle \sexpr_0
    }{
      \sownerenv_0; \sowner_0 \sWLsingle \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sexpr_0}
    }

    \inferrule*{
      \sownerenv_0; \sowner_0 \sWLsingle \sexpr_0
    }{
      \sownerenv_0; \sowner_0 \sWLsingle \obars{\sexpr_0}{\sowner_0}
    }

    \end{mathpar}
  }
  }|]

@Figureref{fig:surface-ownership} extends the surface syntax
 with ownership labels and introduces a single-owner ownership consistency relation.
These labels record the component from which an expression originates.
The extended syntax brings one addition,
 labeled expressions @${\obars{\sexpr}{\sowner}},
 and a requirement that boundary expressions label their inner component.
The single-owner consistency judgment (@${\sownerenv; \sowner \sWLsingle \sexpr}) ensures
 that every subterm of an expression has a unique owner.
This judgment is parameterized by a mapping from variables to labels
 (@${\sownerenv}) and a context label (@${\sowner}).
Every variable reference must occur in a context that matches the variable's
 map entry,
 every labeled expression must match the context,
 and every boundary expressions must have a client name that matches the
 context label.
For example, the expression
 @${\obars{\edynb{\obnd{\sowner_0}{\tnat}{\sowner_1}}{\obars{\svar_0}{\sowner_1}}}{\sowner_0}}
 is consistent under a mapping that contains @${\tann{\svar_0}{\sowner_1}}
 and the @${\sowner_0} context label.
The expression @${\obars{\obars{42}{\sowner_0}}{\sowner_1}}, also written
 @${\obbars{42}{\fconcat{\sowner_0}{\sowner_1}}} (@figureref{fig:evaluation-meta}),
 is inconsistent for any parameters.

Labels correspond to component names but come from a distinct set.
Thus the expression @${(\edynb{\obnd{\sowner_0}{\tnat}{\sowner_1}}{\obars{\svar_0}{\sowner_1}})}
 contains two names, @${\sowner_0} and @${\sowner_1}, and one label @${{}^{\sowner_1}} that
 matches the inner component name.
The distinction separates an implementation from a specification.
A semantics, or implementation, manipulates component names to explain
 errors.
Labels serve as a specification to assess whether a semantics uses component names in a sensible way.
If the two could mix, then the specification would be a biased measure.

Lastly, a surface expression is well-formed
 (@${\fwellformed{\sexpr}{\stoptional}}) if it satisfies a typing judgment---either
 static or dynamic---and single-owner consistency under some labeling and context label @${\sowner_0}.
The theorems below all require well-formed expressions.


@section[#:tag "sec:design:tech:eval"]{Three Evaluation Syntaxes}
@latex-label{sec:design:tech:eval}

Each semantics requires a unique evaluation syntax, but overlaps among
 these six languages motivate three common definitions.
A @emph{higher-order} evaluation syntax supports type-enforcement strategies
 that require wrappers.
A @emph{flat} syntax, with simple checks rather than wrappers,
 supports @|tname|.
And an @emph{erased} syntax supports the compilation of typed and
 untyped code to a common untyped host.

@figure*[
  "fig:evaluation-common"
  @elem{Common evaluation syntax and metafunctions}

  @exact|{
  \lbl{\fbox{\sevallang}~extends \hyperref[fig:surface-language]{\ssurfacelang}}{
    \begin{langarray}
      \eerr & \BNFeq &
        \tagerrorD \mid
        \tagerrorS \mid
        \divisionbyzeroerror \mid
        \boundaryerror{\sbset}{\svalue}
      \\
      \sexpr & \BNFeq &
        \ldots \mid \eerr
      \\
      \stag & \BNFeq &
        \kint \mid \knat \mid \kpair \mid \kfun
      \\
      \ctx & \BNFeq &
        \ctxhole \!\mid
        \eapp{\stoptional}{\ctx\!}{\sexpr} \!\mid
        \eapp{\stoptional}{\svalue\!}{\ctx} \!\mid
        \epair{\ctx}{\sexpr} \!\mid
        \epair{\svalue}{\ctx} \!\mid
        \eunopt{\stoptional}{\!\ctx\!} \mid
      \\ & &
        \ebinopt{\stoptional}{\ctx}{\svalue} \mid
        \ebinopt{\stoptional}{\svalue}{\ctx} \mid
        \edynb{\sbnd}{\ctx} \mid
        \estab{\sbnd}{\ctx}
    \end{langarray}
  }

  \bigskip
\begin{flushleft}
    \(\tagof{\stype_0}
         {} \feq
          \left\{\begin{array}{ll}
            \knat & \quad\mbox{if $\stype_0 \eeq \tnat$}
            \\
            \kint & \quad\mbox{if $\stype_0 \eeq \tint$}
            \\
            \kpair & \quad\mbox{if $\stype_0 \in \tpair{\stype}{\stype}$}
            \\
            \kfun & \quad\mbox{if $\stype_0 \in \tfun{\stype}{\stype}\!\!$}
          \end{array}\right.\)

    \bigskip
    \( \fshallow{\stag_0}{\svalue_0}
         {} \feq
          \left\{\begin{array}{l@{~~}l}
            \makebox[2pt][l]{$\ftrue$}
            \\ & \mbox{\makebox[\widthof{or}][l]{if} $\stag_0 \eeq \knat$ and $\svalue_0 \in \snat$}
            \\ & \mbox{or $\stag_0 \eeq \kint$ and $\svalue_0 \in \sint$}
            \\ & \mbox{or $\stag_0 \eeq \kpair$ and}
            \\ & \quad\, \svalue_0 \in \epair{\svalue}{\svalue} \cup{}
            \\ & \zerowidth{\qquad\, \quad (\emon{\obnd{\sowner}{(\tpair{\stype}{\stype})}{\sowner}}{\svalue})}
            \\ & \mbox{or $\stag_0 \eeq \kfun$ and}
            \\ & \quad\, \svalue_0 \in (\efun{\svar}{\sexpr}) \cup (\efun{\tann{\svar}{\stype}}{\sexpr}) \cup{}
            \\ & \zerowidth{\qquad\, \quad (\emon{\obnd{\sowner}{(\tfun{\stype}{\stype})}{\sowner}}{\svalue})}
            \\
            \zerowidth{\fshallow{\stag_0}{\svalue_1}}
            \\ & \mbox{\makebox[\widthof{or}][l]{if} $\svalue_0 \eeq \ehist{\sbset_0}{\svalue_1}$}
            \\
            \zerowidth{\ffalse}
            \\ & \mbox{otherwise}
          \end{array}\right.  \)

\bigskip
    \( \sdelta(\sunop, \epair{\svalue_0}{\svalue_1})
        {} \feq
          \left\{\begin{array}{l@{~~}l}
            \makebox[2pt][l]{$\svalue_0$}
             & \quad\mbox{if $\sunop \eeq \tinst{\sfst}{\toptional}$}
            \\
            \zerowidth{\svalue_1}
             & \quad\mbox{if $\sunop \eeq \tinst{\ssnd}{\toptional}$}
          \end{array}\right.\)

    \bigskip
    \( \sdelta(\sbinop, \sint_0, \sint_1)
        {} \feq
           \left\{\begin{array}{l@{~~~}l}
             \makebox[2pt][l]{$\sint_0 + \sint_1$}
             \\ & \mbox{if $\sbinop \eeq \tinst{\ssum}{\stoptional}$}
             \\
             \zerowidth{\divisionbyzeroerror}
             \\ & \mbox{if $\sbinop \eeq \tinst{\squotient}{\stoptional}$}
             \\ & \mbox{and $\sint_1 \eeq 0$}
             \\
             \zerowidth{\floorof{\sint_0 / \sint_1}}
             \\ & \mbox{if $\sbinop \eeq \tinst{\squotient}{\stoptional}$}
             \\ & \mbox{and $\sint_1 \neq 0$}
           \end{array}\right.\)
\end{flushleft}
}|]

@figure*[
  "fig:evaluation-meta"
  @elem{Metafunctions for boundaries and labels}

  @exact|{
  \begin{flushleft}
    \(\frev{\sbset_0}
      {} \feq~
        \eset{\obnd{\sowner_1}{\stype_0}{\sowner_0}
      \,{}\mid \obnd{\sowner_0}{\stype_0}{\sowner_1} \in \sbset_0}\)

    \bigskip
    \(\fbsetsenders{\sbset_0} 
      {} \feq~
      \eset{\sowner_1 \mid \obnd{\sowner_0}{\stype_0}{\sowner_1} \in \sbset_0}
    \)

    \bigskip
    \(\frev{\fconcat{\sowner_0}{\fconcat{\cdots}{\sowner_n}}}
      {} \feq~
        {\fconcat{\sowner_n}{\fconcat{\cdots}{\sowner_0}}}\)

    \bigskip
    \(\fvalueowners{\svalue_0}
       {} \feq
      \left\{\begin{array}{ll}
        \eset{\sowner_0} \cup \fvalueowners{\svalue_1}
         & \mbox{if $\svalue_0 \eeq \obars{\svalue_1}{\sowner_0}$}
        \\
        \fvalueowners{\svalue_1}
         & \mbox{if $\svalue_0 \eeq \ehist{\sbset_0}{\svalue_1}$}
        \\
        \eset{}
         & \mbox{otherwise}
      \end{array}\right.\)\\


  \bigskip
  \(\obbars{\sexpr_0}{\fconcat{\sowner_n}{\fconcat{\cdots}{\sowner_1}}} \eeq \sexpr_1
     \quad\sabbreveq\quad
    \sexpr_1 \eeq \obars{\cdots \obars{\sexpr_0}{\sowner_n} \cdots}{\sowner_1}\)
  \end{flushleft}
}|]

@Figureref{fig:evaluation-common} defines common aspects
 of the evaluation syntaxs.
These include errors @${\eerr}, shapes (or, constructors) @${\stag},
 evaluation contexts, and evaluation metafunctions.

A program evaluation may signal four kinds of errors:
@itemlist[#:style 'ordered
@item{
A dynamic tag error (@${\tagerrorD{}}) occurs when
 an untyped redex applies an elimination form to a mis-shaped input.
For example, the first projection of an integer signals a tag error.
}
@item{
An invariant error (@${\tagerrorS{}}) occurs when the shape of a typed
 redex contradicts static typing; a ``tag error'' in typed code is one
 way to reach an invariant error.
One goal of type soundness is to eliminate such contradictions.
}
@item{
A division-by-zero error (@${\divisionbyzeroerror}) may be raised by an
 application of the @${\squotient} primitive;
 a full language will contain many similar primitive errors.
}
@item{
A boundary error (@${\boundaryerror{\sbset}{\svalue}})
 reports a mismatch between two components.
One component, the sender, provided the enclosed value.
A second component rejected the value.
The accompanying set of witness boundaries suggests potential sources for the fault;
 intuitively, this set should include the client--sender boundary.
The error @${\boundaryerror{\eset{\obnd{\sowner_0}{\stype_0}{\sowner_1}}}{\svalue_0}},
 for example,
 says that a mismatch between value @${\svalue_0} and type @${\stype_0} prevented
 the value sent by the @${\sowner_1} component from entering the @${\sowner_0} component.
}]

The four shapes, @${\stag}, correspond both to type constructors and to value
 constructors.
Half of the correpondence is defined by the @${\tagof{\cdot}} metafunction,
 which maps a type to a shape.
The @${\sshallow} metafunction is the other half;
 it checks the top-level shape of a value.

Both metafunctions use an @${\,\cdot \in \cdot\,} judgment,
 which holds if a value is a member of a set.
The claim @${\svalue_0 \in \snat}, for example,  holds when the value @${\svalue_0}
 is a member of the set of natural numbers.
By convention, a variable without a subscript typically refers to a set
 and a term containing a set describes a comprehension.
The term @${(\efun{\svar}{\svalue})}, for instance, describes the
 set @${\eset{(\efun{\svar_i}{\svalue_j}) \mid \svar_i \in \svar \wedge \svalue_j \in \svalue}}
 of all functions that return some value.

The @${\sshallow} metafunction also makes reference to two value constructors
 unique to the higher-order evaluation syntax:
 guard @${(\emon{\sbnd}{\svalue})} and trace @${(\ehist{\sbset}{\svalue})} wrappers.
A guard has a shape determined by the type in its boundary.
A trace is metadata, so @${\sshallow} looks past it.
@Sectionref{sec:design:semantic-framework} informally justifies the design, and
 @figureref{fig:evaluation-ho} formally introduces these wrapper values.

The final components of @figureref{fig:evaluation-common} are the @${\delta}
 metafunctions.
These provide a standard, partial specification of the primitive operations.

@Figureref{fig:evaluation-meta} lists extra metafunctions for boundaries
 and ownership labels.
For boundaries, @${\srev} flips every client and sender name in
 a set of specifications.
Both @|tname| and @|aname| reverse boundaries at function calls.
If a function $f$ crosses one set @${\sbset_0} of boundaries,
 then an input to $f$ crosses the reversed boundaries @${\frev{\sbset_0}}
 before entering the function.
The @${\sbsetsenders} metafunction extracts the sender names from the
 right-hand side of every boundary specification in a set.
For labels, @${\srev} reverses a sequence.
The @${\svalueowners} metafunction collects the labels around an unlabeled
 value stripped of any trace-wrapper metadata.
Guard wrappers are not stripped because they represent boundaries.
Lastly, the abbreviation @${\obbars{\cdot}{\cdot}} captures a list of boundaries.
The term
 @${\obbars{4}{\fconcat{\sowner_0}{\sowner_1}}}
 is short for @${\obars{\obars{4}{\sowner_0}}{\sowner_1}}
 and @${\obbars{5}{\sownerlist_0}} matches @${5} with @${\sownerlist_0} bound to
 the empty list.


@subsection[#:tag "sec:design:tech:eval:HO"]{Higher-Order Syntax, Path-Based Ownership Consistency}
@latex-label{sec:design:tech:eval:HO}

@figure*[
  "fig:evaluation-ho"
  @elem{Higher-Order syntax, typing rules, and ownership consistency}

  @exact|{
  \lbl{\fbox{\syntaxho{}}~extends \hyperref[fig:evaluation-common]{\sevallang}}{
    \begin{langarray}
      \sexpr & \BNFeq &
        \ldots \mid
        \eprehist{\sbset}{\sexpr}
      \\
      \svalue & \BNFeq &
        \sint \mid \snat \mid \epair{\svalue}{\svalue} \mid
        \efun{\svar}{\sexpr} \mid \efun{\tann{\svar}{\stype}}{\sexpr} \mid
        \emon{\sbnd}{\svalue} \mid
        \ehist{\sbset}{\svalue}
    \end{langarray}
  }

  \bigskip
  \lbl{\fbox{$\stypeenv \sWTfull \sexpr : \stype$}~\missingrules{}, extends \hyperref[fig:surface-language]{$\,\stypeenv \sWT \sexpr : \stype$}}{
    \begin{mathpar}
      \inferrule*{
        \stypeenv_0 \sWTfull \svalue_0 : \tdyn
      }{
        \stypeenv_0 \sWTfull \emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0} : \stype_0
      }

      \inferrule*{
      }{
        \stypeenv_0 \sWTfull \eerr : \stype_0
      }

    \end{mathpar}
  }

  \bigskip
  \lbl{\fbox{$\stypeenv \sWTfull \sexpr : \tdyn$}~\missingrules{}, extends \hyperref[fig:surface-language]{$\,\stypeenv \sWT \sexpr : \tdyn$}}{
    \begin{mathpar}
      \inferrule*{
        \stypeenv_0 \sWTfull \svalue_0 : \stype_0
      }{
        \stypeenv_0 \sWTfull \emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0} : \tdyn
      }

      \inferrule*{
        \stypeenv_0 \sWTfull \svalue_0 : \tdyn
      }{
        \stypeenv_0 \sWTfull \ehist{\sbset_0}{\svalue_0} : \tdyn
      }

      \inferrule*{
      }{
        \stypeenv_0 \sWTfull \eerr : \tdyn
      }

    \end{mathpar}
  }

  \lbl{\fbox{$\sownerenv; \sowner \sWLsingle \sexpr$}~\missingrules{}, extends \hyperref[fig:surface-ownership]{$\,\sownerenv; \sowner \sWLsingle \sexpr$}}{\begin{mathpar}
      \inferrule*{
        \sownerenv_0; \sowner_1 \sWLsingle \svalue_0
      }{
        \sownerenv_0; \sowner_0 \sWLsingle \emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      }

      \inferrule*{
        \sownerenv_0; \sowner_0 \sWLsingle \svalue_0
      }{
        \sownerenv_0; \sowner_0 \sWLsingle \ehist{\sbset_0}{\svalue_0}
      }
  \end{mathpar}}
}|]

The higher-order evaluation syntax (@figureref{fig:evaluation-ho}) introduces the two wrapper values
 described in @sectionref{sec:design:semantic-framework}.
A guard @${(\emon{\obnd{\sowner}{\stype}{\sowner}}{\svalue})}
 represents a boundary between two components.
A trace wrapper @${(\ehist{\sbset}{\svalue})} attaches metadata to a value.

Type-enforcement strategies typically use guard wrappers to constrain the
 behavior of a value.
For example, the @|cname| semantics wraps any pair that crosses a boundary
 with a guard; this wrapper validates the elements of the pair upon
 future projections.
Trace wrappers do not constrain behavior.
A traced value simply comes with extra information; namely, a collection of
 the boundaries that the value has previously crossed.

The higher-order typing judgments, @${\stypeenv \sWTfull \sexpr : \stoptional},
 extend the surface typing judgments with rules for wrappers and errors.
Guard wrappers may appear in both typed and untyped code; the rules
 in each case mirror those for boundary expressions.
Trace wrappers may only appear in untyped code;
 this restriction simplifies the @|aname| semantics (@figureref{fig:amnesic-reduction}).
A traced expression is well-formed iff the enclosed value is well-formed.
An error term is well-typed in any context.

@Figureref{fig:evaluation-ho} also extends the single-owner consistency judgment
 to handle wrapped values.
For a guard wrapper, the outer client name must match the context
 and the enclosed value must be single-owner consistent with the inner
 sender name.
For a trace wrapper, the inner value must be single-owner consistent
 relative to the context label.


@subsection[#:tag "sec:design:tech:eval:FO"]{Flat Syntax}
@latex-label{sec:design:tech:eval:FO}

@figure*[
  "fig:evaluation-fo"
  @elem{Flat syntax, typing rules, and ownership consistency}

  @exact|{
    \lbl{\fbox{\syntaxfo{}} \makebox[0pt][l]{extends \hyperref[fig:evaluation-common]{\sevallang}}}{
    \begin{tabular}{l@{~}l}
      \begin{langarray}
        \sexpr & \BNFeq &
          \ldots \mid
          \eloc \mid
          \echecktwo{\stoptional}{\sexpr}{\eloc}
        \\
        \svalue & \BNFeq &
          \sint \mid \snat \mid \eloc
        \\
        \sprevalue & \BNFeq &
          {\efun{\svar}{\sexpr}} \mid
          {\efun{\tann{\svar}{\stype}}{\sexpr}} \mid
          {\epair{\svalue}{\svalue}}
        \\
        \eloc & \BNFeq    & % \locations
          \textrm{\scountable{} set of heap locations}
      \end{langarray}
      &
      \begin{langarray}
        \vstore & \BNFeq &
          \powerset{(\vrecord{\eloc}{\sprevalue})}
        \\
        \bstore & \BNFeq &
          \powerset{(\brecord{\eloc}{\sbset})}
        \\
        \vstoretype & \BNFeq &
          \snil \mid \fcons{\tann{\eloc}{\stag}}{\vstoretype}
      \end{langarray}
    \end{tabular}
    }

    \medskip
    \begin{flushleft}
    \(\fmapref{\vstore_0}{\svalue_0}
       {} \feq
      \left\{\begin{array}{l@{\quad}l}
        \sprevalue_0 & \mbox{if $\svalue_0 \in \eloc$ and $(\vrecord{\svalue_0}{\sprevalue_0}) \in \vstore_0$}
        \\
        \svalue_0 & \mbox{if $\svalue_0 \not\in \eloc$}
      \end{array}\right.\)

    \smallskip
    \(\fmapref{\bstore_0}{\svalue_0}
      {} \feq
      \left\{\begin{array}{l@{\quad}l}
        \sbset_0 & \mbox{if $\svalue_0 \in \eloc$ and $(\brecord{\svalue_0}{\sbset_0}) \in \bstore_0$}
        \\
        \semptymap & \mbox{otherwise}
      \end{array}\right.\)

    \smallskip
    \(\fmapreplace{\bstore_0}{\svalue_0}{\sbset_0}
      {} \feq
      \left\{\begin{array}{l@{\quad}l}
        \zerowidth{\eset{\vrecord{\svalue_0}{\sbset_0}} \cup {(\bstore_0 \setminus (\vrecord{\svalue_0}{\sbset_1}))}}
        \\
        & \mbox{if $\svalue_0 \in \eloc$ and $(\vrecord{\svalue_0}{\sbset_1}) \in \bstore_0$}
        \\
        \bstore_0 & \mbox{otherwise}
      \end{array}\right.\)

    \smallskip
    \(\fmapupdate{\bstore_0}{\svalue_0}{\sbset_0}
       \feq
        \fmapreplace{\bstore_0}{\svalue_0}{\bappend{\sbset_0}{\fmapref{\bstore_0}{\svalue_0}}}\)
  \end{flushleft}

  \medskip
  \lbl{\fbox{$\vstoretype; \stypeenv \sWTtag \sexpr : \stag$}~\missingrules{}}{
    \begin{mathpar}
      \inferrule*{
        \tann{\sloc_0}{\stag_0} \in \vstoretype_0
      }{
        \vstoretype_0; \stypeenv_0 \sWTtag \sloc_0 : \stag_0
      }

      \inferrule*{
        \tann{\svar_0}{\stype_0} \in \stypeenv_0
      }{
        \vstoretype_0; \stypeenv_0 \sWTtag \svar_0 : \ftagof{\stype_0}
      }

      \inferrule*{
        \vstoretype_0; \fcons{\tann{\svar_0}{\tdyn}}{\stypeenv_0} \sWTtag \sexpr_0 : \tdyn
      }{
        \vstoretype_0; \stypeenv_0 \sWTtag \efun{\svar_0}{\sexpr_0} : \kfun
      }

      \inferrule*{
        \vstoretype_0; \fcons{\tann{\svar_0}{\stype_0}}{\stypeenv_0} \sWTtag \sexpr_0 : \stag_0
      }{
        \vstoretype_0; \stypeenv_0 \sWTtag \efun{\tann{\svar_0}{\stype_0}}{\sexpr_0} : \kfun
      }

      \inferrule*{
        \vstoretype_0; \stypeenv_0 \sWTtag \sexpr_0 : \kfun
        \\
        \vstoretype_0; \stypeenv_0 \sWTtag \sexpr_1 : \stag_0
      }{
        \vstoretype_0; \stypeenv_0 \sWTtag \eapp{\stype_0}{\sexpr_0}{\sexpr_1} : \ftagof{\stype_0}
      }

      \inferrule*{
        \vstoretype_0; \stypeenv_0 \sWTtag \sexpr_0 : \kpair
      }{
        \vstoretype_0; \stypeenv_0 \sWTtag \eunopt{\stype_0}{\sexpr_0} : \ftagof{\stype_0}
      }

      \inferrule*{
        \vstoretype_0; \stypeenv_0 \sWT \sexpr_0 : \tdyn
      }{
        \vstoretype_0; \stypeenv_0 \sWT \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sexpr_0} : \ftagof{\stype_0}
      }

      \inferrule*{
        \vstoretype_0; \stypeenv_0 \sWTtag \sexpr_0 : \stag_0
      }{
        \vstoretype_0; \stypeenv_0 \sWTtag \echecktwo{\stype_0}{\sexpr_0}{\eloc_0} : \ftagof{\stype_0}
      }

      \inferrule*{
        \vstoretype_0; \stypeenv_0 \sWTtag \sexpr_0 : \tdyn
      }{
        \vstoretype_0; \stypeenv_0 \sWTtag \echecktwo{\stype_0}{\sexpr_0}{\eloc_0} : \ftagof{\stype_0}
      }

    \end{mathpar}
  }

  \medskip
  \lbl{\fbox{$\vstoretype; \stypeenv \sWTtag \sexpr : \tdyn$}~\missingrules{}}{
    \begin{mathpar}
      \inferrule*{
        \tann{\sloc_0}{\stag_0} \in \vstoretype_0
      }{
        \vstoretype_0; \stypeenv_0 \sWTtag \sloc_0 : \tdyn
      }

      \inferrule*{
        \tann{\svar_0}{\tdyn} \in \stypeenv_0
      }{
        \vstoretype_0; \stypeenv_0 \sWTtag \svar_0 : \tdyn
      }

      \inferrule*{
        \vstoretype_0; \stypeenv_0 \sWTtag \sexpr_0 : \ftagof{\stype_0}
      }{
        \vstoretype_0; \stypeenv_0 \sWTtag \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sexpr_0} : \tdyn
      }

      \inferrule*{
        \vstoretype_0; \stypeenv_0 \sWTtag \sexpr_0 : \tdyn
      }{
        \vstoretype_0; \stypeenv_0 \sWTtag \echecktwo{\tdyn}{\sexpr_0}{\eloc_0} : \tdyn
      }

      \inferrule*{
        \vstoretype_0; \stypeenv_0 \sWTtag \sexpr_0 : \stag_0
      }{
        \vstoretype_0; \stypeenv_0 \sWTtag \echecktwo{\tdyn}{\sexpr_0}{\eloc_0} : \tdyn
      }
    \end{mathpar}
  }
}|]

The flat syntax (@figureref{fig:evaluation-fo})
 supports wrapper-free gradual typing.
A new expression form, @${(\echecktwo{\stoptional}{\sexpr}{\eloc})},
 represents a shape check.
The intended meaning is that the given type must match the value of the
 enclosed expression.
If not, then the location @${\eloc} may be the source of the fault.
Locations are names for the pairs and functions in a program.
These names map to pre-values in a heap (@${\vstore})
 and, more importantly, to sets of boundaries in a blame map (@${\bstore}).
Pairs and functions are now second-class pre-values (@${\sprevalue}) that
 must be allocated before they may be used.

Three meta-functions define heap operations:
 @${\fmapref{\cdot}{\cdot}}, @${\fmapreplace{\cdot}{\cdot}{\cdot}}, and @${\fmapupdate{\cdot}{\cdot}{\cdot}}.
The first gets an item from a finite map,
 the second replaces a blame heap entry,
 and the third extends a blame heap entry.
Because maps are sets, set union suffices to add new entries.

The flat typing judgments check the top-level shape (@${\stag}) of an expression
 and the well-formedness of any subexpressions.
These judgments rely on a store typing (@${\vstoretype})
 to describe heap-allocated values.
These types must be consistent with the actual values on the heap, a standard
 technical device that is spelled out in the appendix.
Untyped functions may appear in a typed context and vice-versa---because
 there are no wrappers to enforce a separation.
Shape-check expressions are valid in typed and untyped contexts.


@subsection[#:tag "sec:design:tech:eval:E"]{Erased Syntax}
@latex-label{sec:design:tech:eval:E}

@figure*[
  "fig:evaluation-eo"
  @elem{Erased evaluation syntax and typing}

  @exact|{
  \lbl{\fbox{\syntaxeo{}}~extends \hyperref[fig:evaluation-common]{\sevallang}}{
    \begin{langarray}
      \svalue & \BNFeq &
        \sint \mid \snat \mid \epair{\svalue}{\svalue} \mid \efun{\svar}{\sexpr} \mid \efun{\tann{\svar}{\stype}}{\sexpr}
    \end{langarray}
  }

  \bigskip
  \lbl{\fbox{$\stypeenv \sWTnone \sexpr : \tdyn$}~\missingrules{}}{
    \begin{mathpar}
      \inferrule*{
        \tann{\svar_0}{\stoptional} \in \stypeenv_0
      }{
        \stypeenv_0 \sWTnone \svar_0 : \tdyn
      }

      \inferrule*{
        \fcons{\tann{\svar_0}{\tdyn}}{\stypeenv_0} \sWTnone \sexpr_0 : \tdyn
      }{
        \stypeenv_0 \sWTnone \efun{\svar_0}{\sexpr_0} : \tdyn
      }

      \inferrule*{
        \fcons{\tann{\svar_0}{\stype_0}}{\stypeenv_0} \sWTnone \sexpr_0 : \tdyn
      }{
        \stypeenv_0 \sWTnone \efun{\tann{\svar_0}{\stype_0}}{\sexpr_0} : \tdyn
      }

      \inferrule*{
        \stypeenv_0 \sWTnone \sexpr_0 : \tdyn
      }{
        \stypeenv_0 \sWTnone \edynb{\sbnd_0}{\sexpr_0} : \tdyn
      }

      \inferrule*{
        \stypeenv_0 \sWTnone \sexpr_0 : \tdyn
      }{
        \stypeenv_0 \sWTnone \estab{\sbnd_0}{\sexpr_0} : \tdyn
      }
    \end{mathpar}
  }
}|]

@Figureref{fig:evaluation-eo} defines an evaluation syntax for type-erased
 programs.
Expressions include error terms; the typing judgment holds
 for any expression without free variables.
Aside from the type annotations left over from the surface syntax, which could be
 removed with a translation step, the result is a conventional dynamically-typed language.


@section[#:tag "sec:design:tech:properties"]{Properties of Interest}
@latex-label{sec:design:tech:properties}

@emph{Type soundness} guarantees that the evaluation of a well-formed expression
 (1) cannot end in an invariant error and (2) preserves an evaluation-language
 image of the surface type.
Note that an invariant error captures the classic idea of going wrong@~citep{m-jcss-1978}.

@exact|{
\begin{definition}[$\sXproj{}$-type soundness]\label{def:ts}
  Let\/ $\sXproj$ map surface types to evaluation types.
  A semantics\/ $\xsym$
  satisfies\/ $\propts{\sXproj}$
  if for all\/ $\fwellformed{\sexpr_0}{\stoptional}$
  one of the following holds:
  \begin{itemize}
    \item
      $\sexpr_0 \rredX \svalue_0$ and\/ ${}\sWTX \svalue_0 : \sXproj(\stoptional)$
    \item
      $\sexpr_0 \rredX \eset{\tagerrorD{}, \divisionbyzeroerror{}} \cup \boundaryerror{\sbset}{\svalue}$
    \item
      $\fdiverge{\sexpr_0}{\rredX}$.
  \end{itemize}
\end{definition}
}|

@|noindent|Three surface-to-evaluation maps (@${\sXproj}) suffice for the evaluation languages:
 an identity map @${\sidproj},
 a type-shape map @${\stagproj} that extends the type-to-shape metafunction from @figureref{fig:evaluation-common},
 and a constant map @${\sdynproj}:

@exact|{
\smallskip
\(\hfill
  \fidproj{\stoptional} \feq \stoptional
  \hfill
  \ftagproj{\stoptional} \feq
    \left\{\begin{array}{ll}
      \tdyn & \mbox{if\/ $\,\stoptional \eeq \tdyn$}
      \\
      \ftagof{\stype_0} & \mbox{if\/ $\,\stoptional \eeq \stype_0$}
    \end{array}\right.
  \hfill
  {\fdynproj{\stoptional} \feq \tdyn}
  \hfill\)
\smallskip
}|

@emph{Complete monitoring} guarantees that the type on each component boundary
 monitors all interactions between client and server components.
The definition of ``all interactions'' comes from the
 path-based ownership propagation laws (@sectionref{sec:design:laws});
 the labels on a value enumerate all partially-responsible components.
Relative to this specification, a reduction that preserves
 single-owner consistency (@figureref{fig:surface-ownership})
 ensures that a value cannot enter a new component without a full type check.

@exact|{
\begin{definition}[complete monitoring]\label{def:cm}
  A semantics\/ $\xsym$
  satisfies\/ $\propcm{}$
  if for all\/ $\fwellformed{\obars{\sexpr_0}{\sowner_0}}{\stoptional}$
  and all\/ $\sexpr_1$
  such that\/ $\sexpr_0 \rredX \sexpr_1$,
  the contractum is single-owner consistent:\/ $\sowner_0 \sWLsingle \sexpr_1$.
\end{definition}
}|

@emph{Blame soundness} and @emph{blame completeness} measure the quality of error messages
 relative to a specification of the components that handled a value during an
  evaluation.
A blame-sound semantics guarantees a subset of the true senders,
 though it may miss some or even all.
A blame-complete semantics guarantees all the true senders,
 though it may include irrelevant information.
A sound and complete semantics reports exactly the components that sent
 the value across a partially-checked boundary.

The standard definitions for blame soundness and blame completeness
 rely on the path-based ownership propagation laws from @sectionref{sec:design:laws}.
Relative to these laws, the definitions relate the sender names in a
 set of boundaries (@figureref{fig:evaluation-meta}) to the true owners of
 the mismatched value.

@exact|{
\begin{definition}[path-based blame soundness and blame completeness]\label{def:blame-path}
  For all well-formed\/ $\sexpr_0$
  such that\/ $\sexpr_0 \rredX \boundaryerror{\sbset_0}{\svalue_0}$:
  \begin{itemize}
    \itemsep0.1ex
    \item
      $\xsym$ satisfies $\propbspath$
      iff
      $\fbsetsenders{\sbset_0} \subseteq \fvalueowners{\svalue_0}$
    \item
      $\xsym$ satisfies $\propbcpath$
      iff
      $\fbsetsenders{\sbset_0} \supseteq \fvalueowners{\svalue_0}$.
  \end{itemize}
\end{definition}
}|

@|noindent|A second useful specification extends the propagation laws to push the owners
 for each location (@${\eloc}) onto the value heap (@${\vstore}).
@Sectionref{sec:design:tech:transient} develops this idea to characterize the blame
 guarantees of the @|tname| semantics.

Lastly, the error preorder relation allows direct behavioral comparisons.
If @${\xsym} and @${\ysym} represent two strategies for type enforcement,
 then @${\xsym \sbehaviorle \ysym} states that the @${\ysym} semantics
 reduces at least as many expressions to a value as the @${\xsym} semantics.

@exact|{
\begin{definition}[error preorder]
  $\xsym \sbehaviorle \ysym$
  iff\/ $\sexpr_0 \rredY \eerr_0$
  implies\/ $\sexpr_0 \rredX \eerr_1$\/
  for all well-formed expressions\/ $\sexpr_0$.
\end{definition}
}|

@|noindent|If two semantics lie below one another on the error preorder, then they report
 type mismatches on exactly the same well-formed expressions.

@exact|{
\begin{definition}[error equivalence]
  $\xsym \sbehavioreq \ysym$
  iff\/ $\xsym \sbehaviorle \ysym$
  and\/ $\ysym \sbehaviorle \xsym$.
\end{definition}
}|


@section[#:tag "sec:design:tech:common-HO"]{Common Higher-Order Notions of Reduction}
@latex-label{sec:design:tech:common-HO}

@figure*[
  "fig:common-reduction"
  @elem{Common notions of reduction for @|nname|, @|cname|, @|fname|, and @|aname|}

  @exact|{
  \begin{minipage}[t]{0.5\columnwidth}
    \lbl{\fbox{$\sexpr \snredsta \sexpr$}}{
      \begin{rrarray}
        \eunopt{\stype_0}{\svalue_0}
        & \!\!\snredsta\!\!
        & \tagerrorS
        \\\sidecond{if {$\svalue_0 \not\in (\emon{\obnd{\sowner}{(\tpair{\stype}{\stype})}{\sowner}}{\svalue})$}}
        \\\sidecond{and {$\sdelta(\sunop, {\svalue_0})$} is undefined}
        \\[0.5ex]
        \eunopt{\stype_0}{\svalue_0}
        & \!\!\snredsta\!\!
        & \sdelta(\sunop, {\svalue_0})
        \\\sidecond{if {$\sdelta(\sunop, {\svalue_0})$} is defined}
        \\[0.5ex]
        \ebinopt{\stype_0}{\svalue_0}{\svalue_1}
        & \!\!\snredsta\!\!
        & \tagerrorS
        \\\sidecond{if {$\sdelta(\sbinop, {\svalue_0}, {\svalue_1})$} is undefined}
        \\[0.5ex]
        \ebinopt{\stype_0}{\svalue_0}{\svalue_1}
        & \!\!\snredsta\!\!
        & \sdelta(\sbinop, {\svalue_0}, {\svalue_1})
        \\\sidecond{if {$\sdelta(\sbinop, {\svalue_0}, {\svalue_1})$} is defined}
        \\[0.5ex]
        \eapp{\stype_0}{\svalue_0}{\svalue_1}
        & \!\!\snredsta\!\!
        & \tagerrorS
        \\\sidecond{if $\svalue_0 \not\in (\efun{\tann{\svar}{\stype}}{\sexpr}) \cup {}$}
        \\\sidecond{\wideas{~}{\svalue_0 \in\in~} $(\emon{\obnd{\sowner}{(\tfun{\stype}{\stype})}{\sowner}}{\svalue})$}
        \\[0.5ex]
        \eapp{\stype_0}{\svalue_0}{\svalue_1}
        & \!\!\snredsta\!\!
        & \esubst{\sexpr_0}{\svar_0}{\svalue_1}
        \\\sidecond{if $\svalue_0 \eeq (\efun{\tann{\svar_0}{\stype_1}}{\sexpr_0})$}
      \end{rrarray}
    }

  \end{minipage}\begin{minipage}[t]{0.5\columnwidth}
    \lbl{\fbox{$\sexpr \snreddyn \sexpr$}}{
      \begin{rrarray}
        \eunopt{\tdyn}{\svalue_0}
        & \!\!\snreddyn\!\!
        & \tagerrorD
        \\\sidecond{if {$\svalue_0 \not\in (\emon{\obnd{\sowner}{(\tpair{\stype}{\stype})}{\sowner}}{\svalue})$}}
        \\\sidecond{and {$\sdelta(\sunop, {\svalue_0})$} is undefined}
        \\[0.5ex]
        \eunopt{\tdyn}{\svalue_0}
        & \!\!\snreddyn\!\!
        & \sdelta(\sunop, {\svalue_0})
        \\\sidecond{if {$\sdelta(\sunop, {\svalue_0})$} is defined}
        \\[0.5ex]
        \ebinopt{\tdyn}{\svalue_0}{\svalue_1}
        & \!\!\snreddyn\!\!
        & \tagerrorD
        \\\sidecond{if {$\sdelta(\sbinop, {\svalue_0}, {\svalue_1})$} is undefined}
        \\[0.5ex]
        \ebinopt{\tdyn}{\svalue_0}{\svalue_1}
        & \!\!\snreddyn\!\!
        & \sdelta(\sbinop, {\svalue_0}, {\svalue_1})
        \\\sidecond{if {$\sdelta(\sbinop, {\svalue_0}, {\svalue_1})$} is defined}
        \\[0.5ex]
        \eapp{\tdyn}{\svalue_0}{\svalue_1}
        & \!\!\snreddyn\!\!
        & \tagerrorD
        \\\sidecond{if $\svalue_0 \not\in (\efun{\svar}{\sexpr}) \cup {}$}
        \\\sidecond{\wideas{~}{\svalue_0 \in\in~} $(\emon{\obnd{\sowner}{(\tfun{\stype}{\stype})}{\sowner}}{\svalue})$}
        \\[0.5ex]
        \eapp{\tdyn}{\svalue_0}{\svalue_1}
        & \!\!\snreddyn\!\!
        & \esubst{\sexpr_0}{\svar_0}{\svalue_1}
        \\\sidecond{if $\svalue_0 \eeq (\efun{\svar_0}{\sexpr_0})$}
      \end{rrarray}
    }
  \end{minipage}
}|]

Four of the semantics build on the higher-order evaluation syntax.
In redexes that do not mix typed and untyped values, these semantics
 share the common behavior specified in @figureref{fig:common-reduction}.
The rules for typed code (@${\snredsta}) handle basic elimination forms
 and raise an invariant error (@${\tagerrorS}) for invalid input.
Type soundness ensures that such errors do not occur.
The rules for untyped code (@${\snreddyn}) raise a tag error for a malformed redex.
Later definitions, for example @figureref{fig:natural-reduction}, combine
 relations via set union to build one large relation to accomodate all redexes.
The full reduction relation is the reflexive-transitive closure of such a set.


@section[#:tag "sec:design:tech:natural"]{@fproperties{@|nname|}}
@latex-label{sec:design:tech:natural}

@figure*[
  "fig:natural-reduction"
  @elem{@|nname| notions of reduction}

  @exact|{
  \lbl{\fbox{\nname{} Syntax}~extends \hyperref[fig:evaluation-ho]{\syntaxho{}}}{
    \begin{langarray}
      \svalue & \BNFeq &
        \sint \mid
        \snat \mid
        \epair{\svalue}{\svalue} \mid
        \efun{\svar}{\sexpr} \mid
        \efun{\tann{\svar}{\stype}}{\sexpr} \mid
        \emon{\obnd{\sowner}{\tfun{\stype}{\stype}}{\sowner}}{\svalue}
    \end{langarray}
  }

  %% 2020-09-01 : KNOWN TYPO technically needs error rules for (unop mon), because not handled in common rules,
  %%  but based on JFP reviews a "missingrules" mark will confuse more than it helps
  \bigskip
  \lbl{\fbox{{$\sexpr \nredNS \sexpr$}}}{
    \begin{rrarray}
      \edynb{\obnd{\sowner_0}{\tfun{\stype_0}{\stype_1}}{\sowner_1}}{\svalue_0}
      & \!\!\nredNS\!\!\!
      & \emon{\obnd{\sowner_0}{\tfun{\stype_0}{\stype_1}}{\sowner_1}}{\svalue_0}
      \\\sidecond{if {$\fshallow{\tagof{\tfun{\stype_0}{\stype_1}}}{\svalue_0}$}}
      \\[0.5ex]
      \edynb{\obnd{\sowner_0}{\tpair{\stype_0}{\stype_1}}{\sowner_1}}{\epair{\svalue_0}{\svalue_1}}
      & \!\!\nredNS\!\!\!
      & \epair{\edynb{\sbnd_0}{\svalue_0}}{\edynb{\sbnd_1}{\svalue_1}}
      \\\sidecond{if {$\fshallow{\tagof{\tpair{\stype_0}{\stype_1}}}{\epair{\svalue_0}{\svalue_1}}$}}
      \\\sidecond{where {$\sbnd_0 \sassign \obnd{\sowner_0}{\stype_0}{\sowner_1}$}
                  and {$\sbnd_1 \sassign \obnd{\sowner_0}{\stype_1}{\sowner_1}$}}
      \\[0.5ex]
      \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sint_0}
      & \!\!\nredNS\!\!\!
      & \sint_0
      \\\sidecond{if {$\fshallow{\tagof{\stype_0}}{\sint_0}$}}
      \\[0.5ex]
      \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \!\!\nredNS\!\!\!
      & \boundaryerror{\eset{\obnd{\sowner_0}{\stype_0}{\sowner_1}}}{\svalue_0}
      \\\sidecond{if {$\neg \fshallow{\tagof{\stype_0}}{\svalue_0}$}}
      \\[1.0ex]
      \eapp{\stype_0}{(\emon{\obnd{\sowner_0}{\tfun{\stype_1}{\stype_2}}{\sowner_1}}{\svalue_0})}{\svalue_1}
      & \!\!\nredNS\!\!\!
      & \edynb{\sbnd_0}{(\eapp{\tdyn}{\svalue_0}{(\estab{\sbnd_1}{\svalue_1})})}
      \\\sidecond{where {$\sbnd_0 \sassign \obnd{\sowner_0}{\stype_2}{\sowner_1}$}
                  and {$\sbnd_1 \sassign \obnd{\sowner_1}{\stype_1}{\sowner_0}$}}
    \end{rrarray}
  }

  \bigskip
  \lbl{\fbox{{$\sexpr \nredND \sexpr$}}}{
    \begin{rrarray}
      \estab{\obnd{\sowner_0}{\tfun{\stype_0}{\stype_1}}{\sowner_1}}{\svalue_0}
      & \!\!\nredND\!\!\!
      & \emon{\obnd{\sowner_0}{\tfun{\stype_0}{\stype_1}}{\sowner_1}}{\svalue_0}
      \\\sidecond{if $\fshallow{\tagof{\stype_0}}{\svalue_0}$}
      \\[0.5ex]
      \estab{\obnd{\sowner_0}{\tpair{\stype_0}{\stype_1}}{\sowner_1}}{\epair{\svalue_0}{\svalue_1}}
      & \!\!\nredND\!\!\!
      & \epair{\estab{\sbnd_0}{\svalue_0}}{\estab{\sbnd_1}{\svalue_1}}
      \\\sidecond{if {$\fshallow{\tagof{\tpair{\stype_0}{\stype_1}}}{\epair{\svalue_0}{\svalue_1}}$}}
      \\\sidecond{where {$\sbnd_0 \sassign \obnd{\sowner_0}{\stype_0}{\sowner_1}$}
                  and {$\sbnd_1 \sassign \obnd{\sowner_0}{\stype_1}{\sowner_1}$}}
      \\[0.5ex]
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sint_0}
      &  \!\!\nredND\!\!\!
      &  \sint_0
      \\\sidecond{if \(\fshallow{\ftagof{\stype_0}}{\sint_0}\)}
      \\[0.5ex]
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      &  \!\!\nredND\!\!\!
      &  \tagerrorS
      \\\sidecond{if \(\neg \fshallow{\ftagof{\stype_0}}{\svalue_0}\)}
      \\[1.0ex]
      \eapp{\tdyn}{(\emon{\obnd{\sowner_0}{\tfun{\stype_0}{\stype_1}}{\sowner_1}}{\svalue_0})}{\svalue_1}
      & \!\!\nredND\!\!\!
      & \estab{\sbnd_0}{(\eapp{\stype_1}{\svalue_0}{(\edynb{\sbnd_1}{\svalue_1})})}
      \\\sidecond{where {$\sbnd_0 \sassign \obnd{\sowner_0}{\stype_1}{\sowner_1}$}
                  and {$\sbnd_1 \sassign \obnd{\sowner_1}{\stype_0}{\sowner_0}$}}
    \end{rrarray}
  }

  \bigskip
  \lbl{\fbox{$\sexpr \rredN \sexpr$} ${}={} \rtclosure{\nredNS, \nredND, \snreddyn, \snredsta}$}{}
}|]


@Figureref{fig:natural-reduction} presents the values and key reduction rules
 for the @|nname| semantics.
Conventional reductions handle primitives and unwrapped functions
  (@${\snreddyn} and @${\snredsta}, @figureref{fig:common-reduction}).

A successful @|nname| reduction yields either an unwrapped value or
 a guard-wrapped function.
Guards arise when a function value reaches a function-type boundary.
Thus, the possible wrapped values are drawn from the following two sets:

@exact|{
\smallskip
\begin{flushleft}
\(\begin{array}[t]{lcl}
  \svaluestat & \BNFeq & \emon{\obnd{\sowner}{(\tfun{\stype}{\stype})}{\sowner}}{(\efun{\svar}{\sexpr})} \\
  & \mid & \emon{\obnd{\sowner}{(\tfun{\stype}{\stype})}{\sowner}}{\svaluedyn}
\end{array}\)

\smallskip
\(\begin{array}[t]{lcl}
  \svaluedyn & \BNFeq & \emon{\obnd{\sowner}{(\tfun{\stype}{\stype})}{\sowner}}{(\efun{\tann{\svar}{\stype}}{\sexpr})} \\
  & \mid & \emon{\obnd{\sowner}{(\tfun{\stype}{\stype})}{\sowner}}{\wideas{\svaluestat}{\svaluedyn}}
\end{array}\)
\end{flushleft}
\smallskip
}|

The presented reduction rules are those relevant to the @|nname| strategy for
 enforcing static types.
When a dynamically-typed value reaches a typed context (@${\sdyn}),
 @|nname| checks the shape of the value against the type.
If the type and value match, @|nname| wraps functions
 and recursively checks the elements of a pair.
Otherwise, @|nname| raises an error at the current boundary.
When a wrapped function receives an argument, @|nname|
 creates two new boundaries:
 one to protect the input to the inner, untyped function
 and one to validate the result.

Reduction in dynamically-typed code (@${\nredND}) follows a dual strategy.
The rules for @${\ssta} boundaries wrap functions and recursively protect
 the contents of pairs.
The application of a wrapped function creates boundaries to validate
 the input to a typed function and to protect the result.

Unsurprisingly, this checking protocol ensures the validity of types in typed
 code and the well-formedness of expressions in untyped code.
The @|nname| approach furthemore enforces boundary types throughout the execution.

@exact|{
\begin{theorem}
  \nname{} satisfies $\propts{\sidproj}$.
\end{theorem}
\begin{proofsketch}
  By progress and preservation lemmas for the higher-order typing judgment ($\sWTfull$).
  For example, if an untyped pair reaches a boundary then a typed step
   ($\nredNS$) makes progress to either a new pair or an error.
  In the former case, the new pair contains two boundary expressions:

  \begin{displayrrarray}
    \edynb{\obnd{\sowner_0}{\tpair{\stype_0}{\stype_1}}{\sowner_1}}{\epair{\svalue_0}{\svalue_1}}
    & \nredNS
    \\\sidestep{\epair{\edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}}{\edynb{\obnd{\sowner_0}{\stype_1}{\sowner_1}}{\svalue_1}}}
  \end{displayrrarray}

  \noindent{}The typing rules for pairs and for $\sdyn$ boundaries validate
   the type of the result.
\end{proofsketch}
}|

@exact|{
\begin{theorem}
  \nname{} satisfies $\propcm{}$.
\end{theorem}
\begin{proofsketch}
  By showing that a lifted variant of the $\rredN$ relation preserves
   single-owner consistency ($\sWLsingle$).
  Full lifted rules for \nname{} appear in an appendix,
   but one can derive the rules by applying the guidelines from \sectionref{sec:design:laws}.
  For example, consider the $\nredND$ rule that wraps a function.
  The lifted version accepts a term with arbitrary ownership labels
   and propagates these labels to the result:

  \begin{displayrrarray}
    \obars{\estab{\obnd{\sowner_0}{(\tfun{\stype_0}{\stype_1})}{\sowner_1}}{\obbars{\svalue_0}{\sownerlist_2}}}{\sowner_3}
    & \nredNDanns & \hspace{3cm}~
    \\\sidestep{\obars{\emon{\obnd{\sowner_0}{(\tfun{\stype_0}{\stype_1})}{\sowner_1}}{\obbars{\svalue_0}{\sownerlist_2}}}{\sowner_3}}
    \\\sidecond{if $\fshallow{\ftagof{\tfun{\stype_0}{\stype_1}}}{\svalue_0}$}
  \end{displayrrarray}

  \noindent{}If the redex satisfies single-owner consistency, then the context label
   matches the client name ($\sowner_3 = \sowner_0$) and the labels inside
   the boundary match the sender name ($\sownerlist_2 = \fconcat{\sowner_1}{\fconcat{\cdots}{\sowner_1}}$).
   Under these premises, the result also satisfies single-owner consistency.
\end{proofsketch}
}|

Complete monitoring implies that the @|nname| semantics detects every mismatch
 between two components---either immediately, or as soon as a function
 computes an incorrect result.
Consequently, every mismatch is due to a single boundary.
Blame soundness and completeness ask whether @|nname| identifies the culprit.

@exact|{
\begin{lemma}\label{lemma:natural-blame}
  If\/ $\sexpr_0$ is well-formed
  and\/ $\sexpr_0 \rredN\!\boundaryerror{\sbset_0}{\svalue_0}$,
  then\/ $\fbsetsenders{\sbset_0}\!=\!\fvalueowners{\svalue_0}$
  and furthermore\/ $\sbset_0$ contains exactly one boundary specification.
\end{lemma}
\begin{proof}
  The sole \nname{} rule that detects a mismatch blames a single boundary:

  \begin{displayrrarray}
    \obars{\sexpr_0}{\sowner_0}
    & \rredN &
    \ctx[\edynb{\obnd{\sowner_1}{\stype_0}{\sowner_2}}{\svalue_0}]
    \\[0.5ex]
    & \rredN &
    \boundaryerror{\eset{\obnd{\sowner_1}{\stype_0}{\sowner_2}}}{\svalue_0}
  \end{displayrrarray}

  \noindent{}Thus $\sbset_0 = \eset{\obnd{\sowner_1}{\stype_0}{\sowner_2}}$ and $\fbsetsenders{\sbset_0} \feq \eset{\sowner_2}$.
  This boundary is the correct one to blame only if it matches the true owner
   of the value; that is, $\fvalueowners{\svalue_0} \feq \eset{\sowner_2}$.
  Complete monitoring guarantees a match via $\sowner_0 \sWLsingle \ctx[\edynb{\obnd{\sowner_1}{\stype_0}{\sowner_2}}{\svalue_0}]$.
\end{proof}
}|

@exact|{
\begin{corollary}
  \nname{} satisfies $\propbspath$\/ and $\propbcpath$.
\end{corollary}
}|


@section[#:tag "sec:design:tech:conatural"]{@fproperties{@|cname|}}
@latex-label{sec:design:tech:conatural}

@figure*[
  "fig:conatural-reduction"
  @elem{@|cname| notions of reduction}

  @exact|{
  \lbl{\fbox{\cname{} Syntax}~extends \hyperref[fig:evaluation-ho]{\syntaxho{}}}{
    \begin{langarray}
      \svalue & \BNFeq &
        \sint \mid
        \snat \mid
        \epair{\svalue}{\svalue} \mid
        \efun{\svar}{\sexpr} \mid
        \efun{\tann{\svar}{\stype}}{\sexpr} \mid
      \\ & &
        \emon{\obnd{\sowner}{\tfun{\stype}{\stype}}{\sowner}}{\svalue} \mid
        \emon{\obnd{\sowner}{\tpair{\stype}{\stype}}{\sowner}}{\svalue}
    \end{langarray}
  }

  \bigskip
  \lbl{\fbox{{$\sexpr \nredCS \sexpr$}}}{
    \begin{rrarray}
      \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \!\!\nredCS\!\!
      & \emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      \\\sidecond{if {$\fshallow{\tagof{\stype_0}}{\svalue_0}$}
                  and {$\svalue_0 \in \epair{\svalue}{\svalue} \cup (\efun{\svar}{\sexpr}) \cup (\emon{\sbnd}{\svalue})$}}
      \\[0.5ex]
      \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sint_0}
      & \!\!\nredCS\!\!
      & \sint_0
      \\\sidecond{if {$\fshallow{\tagof{\stype_0}}{\sint_0}$}}
      \\[0.5ex]
      \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \!\!\nredCS\!\!
      & \boundaryerror{\eset{\obnd{\sowner_0}{\stype_0}{\sowner_1}}}{\svalue_0}
      \\\sidecond{if {$\neg \fshallow{\tagof{\stype_0}}{\svalue_0}$}}
      \\[1.0ex]
      \efst{\stype_0}{(\emon{\obnd{\sowner_0}{\tpair{\stype_1}{\stype_2}}{\sowner_1}}{\svalue_0})}
      & \!\!\nredCS\!\!
      & \edynb{\sbnd_0}{(\efst{\tdyn}{\svalue_0})}
      \\\sidecond{where $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_1}{\sowner_1}$}
      \\[0.5ex]
      \esnd{\stype_0}{(\emon{\obnd{\sowner_0}{\tpair{\stype_1}{\stype_2}}{\sowner_1}}{\svalue_0})}
      & \!\!\nredCS\!\!
      & \edynb{\sbnd_0}{(\esnd{\tdyn}{\svalue_0})}
      \\\sidecond{where $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_2}{\sowner_1}$}
      \\[0.5ex]
      \eapp{\stype_0}{(\emon{\obnd{\sowner_0}{\tfun{\stype_1}{\stype_2}}{\sowner_1}}{\svalue_0})}{\svalue_1}
      & \!\!\nredCS\!\!
      & \edynb{\sbnd_0}{(\eapp{\tdyn}{\svalue_0}{(\estab{\sbnd_1}{\svalue_1})})}
      \\\sidecond{where {$\sbnd_0 \sassign \obnd{\sowner_0}{\stype_2}{\sowner_1}$}
                  and {$\sbnd_1 \sassign \obnd{\sowner_1}{\stype_1}{\sowner_0}$}}
    \end{rrarray}
  }

  \bigskip
  \lbl{\fbox{{$\sexpr \nredCD \sexpr$}}}{
    \begin{rrarray}
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \!\!\nredCD\!\!
      & \emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      \\\sidecond{if $\fshallow{\ftagof{\stype_0}}{\svalue_0}$
                  and {$\svalue_0 \in \epair{\svalue}{\svalue} \cup {(\efun{\tann{\svar}{\stype}}{\sexpr})} \cup {(\emon{\sbnd}{\svalue})}$}}
      \\[0.5ex]
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sint_0}
      &  \!\!\nredCD\!\!
      &  \sint_0
      \\\sidecond{if \(\fshallow{\ftagof{\stype_0}}{\sint_0}\)}
      \\[0.5ex]
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      &  \!\!\nredCD\!\!
      &  \tagerrorS
      \\\sidecond{if \(\neg \fshallow{\ftagof{\stype_0}}{\svalue_0}\)}
      \\[1.0ex]
      \efst{\tdyn}{(\emon{\obnd{\sowner_0}{\tpair{\stype_0}{\stype_1}}{\sowner_1}}{\svalue_0})}
      & \!\!\nredCD\!\!
      & \estab{\sbnd_0}{(\efst{\stype_0}{\svalue_0})}
      \\\sidecond{where $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_0}{\sowner_1}$}
      \\[0.5ex]
      \esnd{\tdyn}{(\emon{\obnd{\sowner_0}{\tpair{\stype_0}{\stype_1}}{\sowner_1}}{\svalue_0})}
      & \!\!\nredCD\!\!
      & \estab{\sbnd_0}{(\esnd{\stype_1}{\svalue_0})}
      \\\sidecond{where $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_1}{\sowner_1}$}
      \\[0.5ex]
      \eapp{\tdyn}{(\emon{\obnd{\sowner_0}{\tfun{\stype_0}{\stype_1}}{\sowner_1}}{\svalue_0})}{\svalue_1}
      & \!\!\nredCD\!\!
      & \estab{\sbnd_0}{(\eapp{\stype_1}{\svalue_0}{(\edynb{\sbnd_1}{\svalue_1})})}
      \\\sidecond{where {$\sbnd_0 \sassign \obnd{\sowner_0}{\stype_1}{\sowner_1}$}
                  and {$\sbnd_1 \sassign \obnd{\sowner_1}{\stype_0}{\sowner_0}$}}
    \end{rrarray}
  }

  \bigskip
  \lbl{\fbox{$\sexpr \rredC \sexpr$} ${}={} \rtclosure{\nredCS, \nredCD, \snreddyn, \snredsta}$}{}
}|]

@Figureref{fig:conatural-reduction} presents the @|cname| strategy.
@|cname| is a lazy variant of the @|nname| approach.
Instead of eagerly validating pairs at a boundary, @|cname| creates a wrapper
 to delay element-checks until they are needed.

Relative to @|nname|, there are two changes in the notions of reduction.
First, the rules for a pair value at a pair-type boundary create guards.
Second, new projection rules handle guarded pairs;
 these rules make a new boundary to validate the projected element.

@|cname| still satisfies both a strong type soundness theorem and complete
 monitoring.
Blame soundness and blame completeness follow from complete monitoring.
Nevertheless, @|cname| and @|nname| can behave differently.

@exact|{
\begin{theorem}
  \cname{} satisfies $\propts{\sidproj}$.
\end{theorem}
\begin{proofsketch}
  By progress and preservation lemmas for the higher-order typing judgment ($\sWTfull$).
  For example, consider the rule that applies a wrapped function in a statically-typed context:

  \begin{displayrrarray}
    \eapp{\stype_0}{(\emon{\obnd{\sowner_0}{(\tfun{\stype_1}{\stype_2})}{\sowner_1}}{\svalue_0})}{\svalue_1}
    & \nredCS
    \\[0.5ex]\sidestep{\edynb{\obnd{\sowner_0}{\stype_2}{\sowner_1}}{(\eapp{\tdyn}{\svalue_0}{(\estab{\obnd{\sowner_1}{\stype_1}{\sowner_2}}{\svalue_1})})}}
  \end{displayrrarray}

  \noindent{}If the redex is well-typed, then $\svalue_1$ has type $\stype_1$
   and the inner $\sstat$ boundary is well-typed.
  Similar reasoning for $\svalue_0$ shows that the untyped application in the
   result is well-typed.
  Thus the $\sdyn$ boundary has type $\stype_2$ which, by inversion on
   the redex, is a subtype of $\stype_0$.
\end{proofsketch}
}|

@exact|{
\begin{theorem}
  \cname{} satisfies $\propcm{}$.
\end{theorem}
\begin{proofsketch}
  $\!\!\!$ By preservation of single-owner consistency for the lifted
   $\rredC$ relation.
  Consider the lifted rule that applies a wrapped function:

  \begin{displayrrarray}
    \obars{\eapp{\stype_0}{\obbars{\emon{\obnd{\sowner_0}{(\tfun{\stype_1}{\stype_2})}{\sowner_1}}{\obars{\svalue_0}{\sowner_2}}}{\sownerlist_3}}{\svalue_1}}{\sowner_4}
    & \nredCSanns
    \\[0.5ex]
    \sidestep{\obars{\edynb{\obnd{\sowner_0}{\stype_2}{\sowner_1}}{\obars{\eapp{\tdyn}{\svalue_0}{(\estab{\obnd{\sowner_1}{\stype_1}{\sowner_0}}{\obars{\svalue_1}{\fconcat{\sowner_4}{\frev{\sownerlist_3}}}})}}{\sowner_2}}}{\fconcat{\sownerlist_3}{\sowner_4}}}
  \end{displayrrarray}

  \noindent{}If the redex satisfies single-owner consistency, then $\sowner_0 = \sownerlist_3 = \sowner_4$
  and $\sowner_1 = \sowner_2$.
  Hence both sequences of labels in the result contain nothing but the context label $\sowner_4$.
\end{proofsketch}
}|

@exact|{
\begin{theorem}
  \cname{} satisfies $\propbspath$\/ and $\propbcpath$.
\end{theorem}
\begin{proofsketch}
  By the same line of reasoning that supports \nname{}; refer to \lemmaref{lemma:natural-blame}.
\end{proofsketch}
}|

@exact|{
\begin{theorem}
  $\nsym{} \sbehaviorle \csym{}$.
\end{theorem}
\begin{proofsketch}
  By a stuttering simulation.
  \nname{} takes extra steps when a pair reaches a boundary because it
   immediately checks the contents; \cname{} creates a guard wrapper.
  \cname{} takes additional steps when eliminating a wrapped pair.
  The appendix defines the simulation relation.

  The pair wrappers in \cname{} imply $\csym{} \not\sbehaviorle \nsym{}$.
  Consider a typed expression that imports an untyped pair
   with an ill-typed first element.

  \begin{displayrrarray}
    \edynb{\obnd{\sowner_0}{\tpair{\tnat}{\tnat}}{\sowner_1}}{\epair{{-2}}{2}}
  \end{displayrrarray}

  \noindent{}\nname{} detects the mismatch at the boundary, but \cname{} will only
    raise an error if the first element is accessed.
\end{proofsketch}
}|


@section[#:tag "sec:design:tech:forgetful"]{@fproperties{@|fname|}}
@latex-label{sec:design:tech:forgetful}

@figure*[
  "fig:forgetful-reduction"
  @elem{@|fname| notions of reduction}

  @exact|{
  \lbl{\fbox{\fname{} Syntax}~extends \hyperref[fig:evaluation-ho]{\syntaxho{}}}{
    \begin{langarray}
      \svalue & \BNFeq &
        \sint \mid
        \snat \mid
        \epair{\svalue}{\svalue} \mid
        \efun{\svar}{\sexpr} \mid
        \efun{\tann{\svar}{\stype}}{\sexpr} \mid
      \\ & &
        \emon{\obnd{\sowner}{\tfun{\stype}{\stype}}{\sowner}}{\svalue} \mid
        \emon{\obnd{\sowner}{\tpair{\stype}{\stype}}{\sowner}}{\svalue}
    \end{langarray}
  }

  \bigskip
  \lbl{\fbox{$\sexpr \nredFS \sexpr$}}{
    \begin{rrarray}
      \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \!\!\nredFS\!\!
      & \emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      \\\sidecond{if $\fshallow{\tagof{\stype_0}}{\svalue_0}$
                  and $\svalue_0 \in {\epair{\svalue}{\svalue}} \cup
                                     (\efun{\svar}{\sexpr}) \cup
                                     (\emon{\sbnd}{\svalue})$}
      \\[0.5ex]
      \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \!\!\nredFS\!\!
      & \sint_0
      \\\sidecond{if $\fshallow{\tagof{\stype_0}}{\svalue_0}$}
      \\[0.5ex]
      \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \!\!\nredFS\!\!
      & \boundaryerror{\eset{\obnd{\sowner_0}{\stype_0}{\sowner_1}}}{\svalue_0}
      \\\sidecond{if $\neg\fshallow{\tagof{\stype_0}}{\svalue_0}$}
      \\[1.0ex]
      \efst{\stype_0}{(\emon{\obnd{\sowner_0}{\tpair{\stype_1}{\stype_2}}{\sowner_1}}{\svalue_0})}
      & \!\!\nredFS\!\!
      & \edynb{\sbnd_0}{(\efst{\tdyn}{\svalue_0})}
      \\\sidecond{where $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_1}{\sowner_1}$}
      \\[0.5ex]
      \esnd{\stype_0}{(\emon{\obnd{\sowner_0}{\tpair{\stype_1}{\stype_2}}{\sowner_1}}{\svalue_0})}
      & \!\!\nredFS\!\!
      & \edynb{\sbnd_0}{(\esnd{\tdyn}{\svalue_0})}
      \\\sidecond{where $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_2}{\sowner_1}$}
      \\[0.5ex]
      \eapp{\stype_0}{(\emon{\obnd{\sowner_0}{\tpair{\stype_1}{\stype_2}}{\sowner_1}}{\svalue_0})}{\svalue_1}
      & \!\!\nredFS\!\!
      & \edynb{\sbnd_0}{(\eapp{\tdyn}{\svalue_0}{(\estab{\sbnd_1}{\svalue_1})})}
      \\\sidecond{where $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_2}{\sowner_1}$
                  and $\sbnd_1 \sassign \obnd{\sowner_1}{\stype_1}{\sowner_0}$}
    \end{rrarray}
  }

  \bigskip
  \lbl{\fbox{$\sexpr \nredFD \sexpr$}}{
    \begin{rrarray}
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \!\!\nredFD\!\!
      & \emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      \\\sidecond{if $\fshallow{\tagof{\stype_0}}{\svalue_0}$
                  and $\svalue_0 \in {\epair{\svalue}{\svalue}} \cup {(\efun{\tann{\svar}{{\stype}}}{\sexpr})}$}
      \\[0.5ex]
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{(\emon{\sbnd_1}{\svalue_0})}
      & \!\!\nredFD\!\!
      & \svalue_0
      \\\sidecond{if $\fshallow{\tagof{\stype_0}}{\svalue_0}$}
      \\\sidecond{and $\svalue_0 \in {\epair{\svalue}{\svalue}} \cup
                                     (\efun{\svar}{\sexpr}) \cup
                                     (\emon{\sbnd}{\epair{\svalue}{\svalue}}) \cup
                                     (\emon{\sbnd}{(\efun{\tann{\svar}{\stype}}{\sexpr})})$}
      \\[0.5ex]
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sint_0}
      & \!\!\nredFD\!\!
      & \sint_0
      \\\sidecond{if $\fshallow{\tagof{\stype_0}}{\sint_0}$}
      \\[0.5ex]
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \!\!\nredFD\!\!
      & \tagerrorS
      \\\sidecond{if $\neg\fshallow{\tagof{\stype_0}}{\svalue_0}$}
      \\[1.0ex]
      \efst{\tdyn}{(\emon{\obnd{\sowner_0}{\tpair{\stype_0}{\stype_1}}{\sowner_1}}{\svalue_0})}
      & \!\!\nredFD\!\!
      & \estab{\sbnd_0}{(\efst{\stype_0}{\svalue_0})}
      \\\sidecond{where $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_0}{\sowner_1}$}
      \\[0.5ex]
      \esnd{\tdyn}{(\emon{\obnd{\sowner_0}{\tpair{\stype_0}{\stype_1}}{\sowner_1}}{\svalue_0})}
      & \!\!\nredFD\!\!
      & \estab{\sbnd_0}{(\esnd{\stype_1}{\svalue_0})}
      \\\sidecond{where $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_1}{\sowner_1}$}
      \\[0.5ex]
      \eapp{\tdyn}{(\emon{\obnd{\sowner_0}{\tfun{\stype_0}{\stype_1}}{\sowner_1}}{\svalue_0})}{\svalue_1}
      & \!\!\nredFD\!\!
      & \estab{\sbnd_0}{(\eapp{\stype_1}{\svalue_0}{(\edynb{\sbnd_1}{\svalue_1})})}
      \\\sidecond{where $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_1}{\sowner_1}$
                  and $\sbnd_1 \sassign \obnd{\sowner_1}{\stype_0}{\sowner_0}$}
    \end{rrarray}
  }

  \bigskip
  \lbl{\fbox{$\sexpr \rredF \sexpr$} ${}={} \rtclosure{\nredFS, \nredFD, \snreddyn, \snredsta}$}{}
}|]

The @|fname| semantics (@figureref{fig:forgetful-reduction})
 creates wrappers to enforce pair and function types,
 but strictly limits the number of wrappers on any one value.
An untyped value acquires at most one wrapper.
A typed value acquires at most two wrappers: one to protect itself from
 inputs, and a second to reflect the expectations of its current client:

@exact|{
\smallskip
\(\hfill
  \begin{array}[t]{lcl}
    \svaluestat
     & \BNFeq & \emon{\sbnd}{\epair{\svalue}{\svalue}} \\
     & \mid   & \emon{\sbnd}{\efun{\svar}{\sexpr}} \\
     & \mid   & \emon{\sbnd}{(\emon{\sbnd}{\epair{\svalue}{\svalue}})} \\
     & \mid   & \zerowidth{\emon{\sbnd}{(\emon{\sbnd}{\efun{\tann{\svar}{\stype}}{\sexpr}})}}
  \end{array}
  \hfill
  \begin{array}[t]{lcl}
    \svaluedyn
    & \BNFeq & \emon{\sbnd}{\epair{\svalue}{\svalue}} \\
    & \mid & \emon{\sbnd}{\efun{\tann{\svar}{\stype}}{\sexpr}}
  \end{array}
\hfill\)
\smallskip
}|

@|fname| enforces this two-wrapper limit by removing the outer wrapper of
 any guarded value that exits typed code.
Re-entering typed code makes a new wrapper,
 but these wrappers do not
 accumulate because a value cannot enter typed code twice in a row; it
 must first exit typed code and lose one wrapper.

Removing outer wrappers does not affect the type soundness of untyped code;
 all well-formed values match @${\tdyn}, with or without wrappers.
Type soundness for typed code is guaranteed by the temporary outer wrappers.
Complete monitoring is lost, however, because the removal of a wrapper
 creates a joint-ownership situation.
Similarly, @|fname| lies above @|cname| and @|nname| in the error preorder.

When a type mismatch occurs, @|fname| blames one boundary.
Though sound, this one boundary is generally not enough information to
 find the source of the problem.
So, @|fname| fails to satisfy blame completeness.

@exact|{
\begin{theorem}\label{thm:F-TS}
  \fname{} satisfies $\propts{\sidproj}$.
\end{theorem}
\begin{proofsketch}
  By progress and preservation lemmas for the higher-order typing judgment ($\sWTfull$).
  The most interesting proof case shows that dropping a guard wrapper does not
   break type soundness.
  Suppose that a pair $\svalue_0$ with static type $\tpair{\tint}{\tint}$ crosses
   two boundaries and re-enters typed code at a different type.

  \begin{displayrrarray}
    \edynb{\obnd{\sowner_0}{(\tpair{\tnat}{\tnat})}{\sowner_1}}{(\estab{\obnd{\sowner_1}{\tpair{\tint}{\tint}}{\sowner_2}}{\svalue_0})}
    & \rredF
    \\[0.5ex]\sidestep{\emon{\obnd{\sowner_0}{(\tpair{\tnat}{\tnat})}{\sowner_1}}{(\emon{\obnd{\sowner_1}{\tpair{\tint}{\tint}}{\sowner_2}}{\svalue_0})}}
  \end{displayrrarray}

  \noindent{}No matter what value $\svalue_0$ is, the result is well-typed because
   the context trusts the outer wrapper.
  If this double-wrapped value---call it $\svalue_2$---crosses another boundary,
   \fname{} drops the outer wrapper.
  Nevertheless, the result is a sound dynamically-typed value:

  \begin{displayrrarray}
    \estab{\obnd{\sowner_3}{(\tpair{\tnat}{\tnat})}{\sowner_0}}{\svalue_2}
    & \rredF
    \\[0.5ex]\sidestep{\emon{\obnd{\sowner_1}{\tpair{\tint}{\tint}}{\sowner_2}}{\svalue_0}}
  \end{displayrrarray}

  When this single-wrapped wrapped pair reenters a typed context,
   it again gains a wrapper to document the context's expectation:

  \begin{displayrrarray}
    \edynb{\obnd{\sowner_4}{(\tpair{\stype_1}{\stype_2})}{\sowner_3}}{(\emon{\obnd{\sowner_1}{\tpair{\tint}{\tint}}{\sowner_2}}{\svalue_0})}
    & \rredF
    \\[0.5ex]\sidestep{\emon{\obnd{\sowner_4}{(\tpair{\stype_1}{\stype_2})}{\sowner_3}}{(\emon{\obnd{\sowner_1}{\tpair{\tint}{\tint}}{\sowner_2}}{\svalue_0})}}
  \end{displayrrarray}

  \noindent{}The new wrapper preserves soundness.
\end{proofsketch}
}|

@exact|{
\begin{theorem}\label{thm:F-CM}
  \fname{} does not satisfy $\propcm{}$.
\end{theorem}
\begin{proof}
  Consider the lifted variant of the $\ssta$ rule that removes an outer guard wrapper:

  \begin{displayrrarray}
    \obars{\estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\obbars{\emon{\sbnd_1}{\svalue_0}}{\sownerlist_2}}}{\sowner_3}
    & \nredFDanns &
    \obbars{\svalue_0}{\fconcat{\sownerlist_2}{\sowner_3}}
    \\\sidecond{if $\fshallow{\tagof{\stype_0}}{(\emon{\sbnd_1}{\svalue_0})}$}
  \end{displayrrarray}

  \noindent{}Suppose $\sowner_0 \neq \sowner_1$.
  If the redex satisfies single-owner consistency, then $\sownerlist_2$ contains
   $\sowner_1$ and $\sowner_3 = \sowner_0$.
  Thus the rule creates a contractum with two distinct labels.
\end{proof}
}|

@exact|{
\begin{theorem}
  \fname{} satisfies\/ $\propbspath$.
\end{theorem}
\begin{proof}
  By a preservation lemma for a weakened version of the $\sWLsingle$ judgment,
   which is defined in the appendix.
  The judgment asks whether the owners on a value contain at least the name
   of the current component.
  \fname{} easily satisfies this invariant because the ownership
   guidelines (\sectionref{sec:design:laws}) never drop an un-checked label.
  Thus, when a boundary error occurs:

  \begin{displayrrarray}
    \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
    & \nredFS
    & \boundaryerror{\eset{\obnd{\sowner_0}{\stype_0}{\sowner_1}}}{\svalue_0}
    \\\sidecond{if $\neg\fshallow{\tagof{\stype_0}}{\svalue_0}$}
  \end{displayrrarray}

  \noindent{}the sender name $\sowner_1$ matches one of the ownership labels on $\svalue_0$.
\end{proof}
}|

@exact|{
\begin{theorem}
  \fname{} does not satisfy\/ $\propbcpath$.
\end{theorem}
\begin{proof}
  The proof of \theoremref{thm:F-CM} shows how a pair value can acquire two labels.
  A function can gain owners in a similar fashion, and reach an incompatible boundary:

  {\newcommand{\theexvalue}{\obbars{\efun{\svar_0}{\svar_0}}{\fconcat{\sowner_0}{\sowner_1}}}
  \begin{displayrrarray}
    \edynb{\obnd{\sowner_2}{\tint}{\sowner_1}}{\theexvalue}
    & \nredFSanns
    \\\sidestep{\boundaryerror{\eset{\obnd{\sowner_2}{\tint}{\sowner_1}}}{\theexvalue}}
  \end{displayrrarray}}

  \noindent{}In this example, the error does not point to component $\sowner_0$.
\end{proof}
}|

@exact|{
\begin{theorem}
  $\csym{} \sbehaviorle \fsym{}$.
\end{theorem}
\begin{proofsketch}
  By a stuttering simulation.
  \cname{} can take extra steps at an elimination form to unwrap an arbitrary
   number of wrappers; \fname{} has at most two to unwrap.

  In the other direction, $\fsym{} \not\sbehaviorle \csym{}$ because \fname{} drops checks.
  Let:

  \begin{displayrrarray}
    \sexpr_0 = \estab{\sbnd_0}{(\edynb{\obnd{\sowner_0}{(\tfun{\tnat}{\tnat})}{\sowner_1}}{(\efun{\svar_0}{\svar_0})})}
    \\[0.5ex]
    \sexpr_1 = \eapp{\tdyn}{\sexpr_0}{\epair{2}{8}}
  \end{displayrrarray}

  \noindent{}Then $\sexpr_1 \rredF {\epair{2}{8}}$ and \cname{} raises a boundary error.
\end{proofsketch}
}|


@section[#:tag "sec:design:tech:transient"]{@fproperties{@|tname|}}
@latex-label{sec:design:tech:transient}

@figure*[
  "fig:transient-reduction"
  @elem{@|tname| notions of reduction}

  @exact|{
  \lbl{\fbox{\tname{} Syntax}~extends \hyperref[fig:evaluation-fo]{\syntaxfo}}{
    \begin{langarray}
      \svalue & \BNFeq &
        \sint \mid \snat \mid \eloc
    \end{langarray}
  }

  \bigskip
  \lbl{\fbox{$\conf{\sexpr}{\vstore}{\bstore} \nredTX \conf{\sexpr}{\vstore}{\bstore}$}~\missingrules{}}{
    \begin{rrarray}
      \conf{(\edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0})}{\vstore_0}{\bstore_0}
      & \nredTX
      & \conf{\svalue_0}{\vstore_0}{(\fmapupdate{\bstore_0}{\svalue_0}{\eset{\obnd{\sowner_0}{\stype_0}{\sowner_1}}})}
      \\\sidecond{if $\fshallow{\ftagof{\stype_0}}{\fmapref{\vstore_0}{\svalue_0}}$}
      \\[0.5ex]
      \conf{(\edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0})}{\vstore_0}{\bstore_0}
      & \nredTX
      & \conf{\boundaryerror{\eset{\obnd{\sowner_0}{\stype_0}{\sowner_1}}}{\svalue_0}}{\vstore_0}{\bstore_0}
      \\\sidecond{if $\neg\fshallow{\ftagof{\stype_0}}{\fmapref{\vstore_0}{\svalue_0}}$}
      \\[0.5ex]
      \conf{(\estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0})}{\vstore_0}{\bstore_0}
      & \nredTX
      & \conf{\svalue_0}{\vstore_0}{(\fmapupdate{\bstore_0}{\svalue_0}{\eset{\obnd{\sowner_0}{\stype_0}{\sowner_1}}})}
      \\\sidecond{if $\fshallow{\ftagof{\stype_0}}{\fmapref{\vstore_0}{\svalue_0}}$}
      \\[0.5ex]
      \conf{(\estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0})}{\vstore_0}{\bstore_0}
      & \nredTX
      & \conf{\tagerrorS}{\vstore_0}{\bstore_0}
      \\\sidecond{if $\neg\fshallow{\ftagof{\stype_0}}{\fmapref{\vstore_0}{\svalue_0}}$}
      \\[0.5ex]
      \conf{(\echecktwo{\tdyn}{\svalue_0}{\eloc_0})}{\vstore_0}{\bstore_0}
      & \nredTX
      & \conf{\svalue_0}{\vstore_0}{\bstore_0}
      \\[0.5ex]
      \conf{(\echecktwo{\stype_0}{\svalue_0}{\eloc_0})}{\vstore_0}{\bstore_0}
      &  \nredTX
      & \conf{\svalue_0}{\vstore_0}{(\fmapupdate{\bstore_0}{\svalue_0}{\fmapref{\bstore_0}{\eloc_0}})}
      \\\sidecond{if \(\fshallow{\ftagof{\stype_0}}{\fmapref{\vstore_0}{\svalue_0}}\)}
      \\[0.5ex]
      \conf{(\echecktwo{\stype_0}{\svalue_0}{\eloc_0})}{\vstore_0}{\bstore_0}
      & \nredTX
      \\\sidestep{ \conf{\boundaryerror{\bappend{\fmapref{\bstore_0}{\svalue_0}}{\fmapref{\bstore_0}{\eloc_0}}}{\svalue_0}}{\vstore_0}{\bstore_0} }
      \\\sidecond{if \(\neg \fshallow{\ftagof{\stype_0}}{\fmapref{\vstore_0}{\svalue_0}}\)}
      \\[1.0ex]

      \conf{(\eunopt{\stoptional}{\eloc_0})}{\vstore_0}{\bstore_0}
      &  \nredTX
      \\\sidestep{ \conf{(\echecktwo{\stoptional}{\sdelta(\sunop, \vstore_0(\eloc_0))}{\eloc_0})}{\vstore_0}{\bstore_0} }
      \\\sidecond{if $\sdelta(\sunop, \vstore_0(\eloc_0))$ is defined}
      \\[0.5ex]
      \conf{(\ebinopt{\toptional}{\sint_0}{\sint_1})}{\vstore_0}{\bstore_0}
      &  \nredTX
      &  \conf{\sdelta(\sbinop, \sint_0, \sint_1)}{\vstore_0}{\bstore_0}
      \\\sidecond{if \(\sdelta(\sbinop, \sint_0, \sint_1)\) is defined}
      \\[0.5ex]
      \conf{(\eapp{\stype_0}{\eloc_0}{\svalue_0})}{\vstore_0}{\bstore_0}
      & \nredTX
      & \conf{(\echecktwo{\stype_0}{\esubst{\sexpr_0}{\svar_0}{\svalue_0}}{\eloc_0})}{\vstore_0}{\bstore_1}
      \\\sidecond{if \(\fmapref{\vstore_{0}}{\eloc_0}=\efun{\svar_0}{\sexpr_0}\)}
      \\\sidecond{and $\bstore_1 = \fmapupdate{\bstore_0}{\svalue_0}{\frev{\fmapref{\bstore_0}{\eloc_0}}}$}
      \\[0.5ex]
      \conf{(\eapp{\tdyn}{\eloc_0}{\svalue_0})}{\vstore_0}{\bstore_0}
      &  \nredTX
      & \conf{(\esubst{\sexpr_0}{\svar_0}{\svalue_0})}{\vstore_0}{\bstore_0}
      \\\sidecond{if \(\fmapref{\vstore_{0}}{\eloc_0}=\efun{\svar_0}{\sexpr_0}\)}
      \\[0.5ex]
      \conf{(\eapp{\stoptional}{\eloc_0}{\svalue_0})}{\vstore_0}{\bstore_0}
      &  \nredTX
      & \conf{(\echecktwo{\stoptional}{\esubst{\sexpr_0}{\svar_0}{\svalue_0}}{\eloc_0})}{\vstore_0}{\bstore_1}
      \\\sidecond{if $\fmapref{\vstore_{0}}{\eloc_0}=\efun{\tann{\svar_0}{{\stype_0}}}{\sexpr_0}$
                  and $\fshallow{\ftagof{\stype_0}}{\fmapref{\vstore_0}{\svalue_0}}$}
      \\\sidecond{and $\bstore_1 = \fmapupdate{\bstore_0}{\svalue_0}{\frev{\bstore_0(\eloc_0)}}$}
      \\[0.5ex]
      \conf{(\eapp{\stoptional}{\eloc_0}{\svalue_0})}{\vstore_0}{\bstore_0}
      &  \nredTX
      & \conf{\boundaryerror{\frev{\fmapref{\bstore_0}{\eloc_0}}}{\svalue_0}}{\vstore_0}{\bstore_1}
      \\\sidecond{if $\fmapref{\vstore_{0}}{\eloc_0}=\efun{\tann{\svar_0}{{\stype_0}}}{\sexpr_0}$
                  and $\neg \fshallow{\ftagof{\stype_0}}{\fmapref{\vstore_0}{\svalue_0}}$}
      \\\sidecond{where $\bstore_1 = \fmapupdate{\bstore_0}{\svalue_0}{\frev{\bstore_0(\eloc_0)}}$}
      \\[1.0ex]

      \conf{\sprevalue_0}{\vstore_0}{\bstore_0}
      &  \nredTX
      \\\sidestep{ \conf{\eloc_0}{(\eset{\vrecord{\eloc_0}{\sprevalue_0}} \cup \vstore_0)}{(\eset{\brecord{\eloc_0}{\semptymap}} \cup {\bstore_0})} }
      \\\sidecond{where $\ffresh{\eloc_0}{\vstore_0\mbox{ and }\bstore_0}$}

    \end{rrarray}
  }

  \bigskip
  \lbl{\fbox{$\sexpr; \vstore; \bstore \rredT \sexpr; \vstore; \bstore$} ${}={} \rastar_{\tsym}$}{
    where $\conf{\ctx[\sexpr_0]}{\vstore_0}{\bstore_0} ~\mathrel{\tsym}~ \conf{\ctx[\sexpr_1]}{\vstore_1}{\bstore_1}$
    if $\conf{\sexpr_0}{\vstore_0}{\bstore_0} \nredTX \conf{\sexpr_1}{\vstore_1}{\bstore_1}$
  }
}|]

The @|tname| semantics in @figureref{fig:transient-reduction} builds on the flat evaluation syntax (@figureref{fig:evaluation-fo});
 it stores pairs and functions on a heap
 as indicated by the syntax of @figureref{fig:evaluation-fo},
 and aims to enforce type constructors (@${\stag}, or @${\tagof{\stype}})
 through shape checks.
For every pre-value @${\sprevalue} stored on a heap @${\vstore}, there is a
 corresponding entry in a blame map @${\bstore} that points to a set of boundaries.
The blame map provides information if
 a mismatch occurs, following Reticulated Python@~citep{vss-popl-2017,v-thesis-2019}.

Unlike for the higher-order-checking semantics, there is significant overlap between the
 @|tname| rules for typed and untyped redexes.
Thus @figureref{fig:transient-reduction} presents one notion of reduction.
The first group of rules in @figureref{fig:transient-reduction} handle boundary
 expressions and check expressions.
When a value reaches a boundary, @|tname| matches its shape against the
 expected type.
If successful, the value crosses the boundary and its blame map records
 the fact; otherwise, the program halts.
For a @${\sdyn} boundary, the result is a boundary error.
For a @${\ssta} boundary, the mismatch reflects an invariant error in typed
 code.
Check expressions similarly match a value against a type-shape.
On success, the blame map gains the boundaries associated with
 the location @${\eloc_0} from which the value originated.
On failure, these same boundaries may help the programmer diagnose the fault.

The second group of rules handle primitives and application.
Pair projections and function applications must be followed by a check
 in typed contexts to enforce the type annotation at the elimination form.
In untyped contexts, a check for the dynamic type embeds a possibly-typed subexpression.
The binary operations are not elimination forms, so they are not followed by
 a check.
Applications of typed functions additionally check the input value against
 the function's domain type.
If successful, the blame map records the check.
Otherwise, @|tname| reports the boundaries associated with the function@~citep{vss-popl-2017}.
Note that untyped functions may appear in typed contexts, and vice-versa.

Applications of untyped functions in untyped code do not update the
 blame map.
This allows an implementation to insert all checks by rewriting typed
 code at compile-time, leaving untyped code as is.
Protected typed code can then interact with any untyped libraries.

Not shown in @figureref{fig:transient-reduction} are rules for elimination
 forms that halt the program.
When @${\sdelta} is undefined or when a non-function is applied, the result
 is either an invariant error or a tag error depending on the context.

@|tname| shape checks do not guarantee full type soundness,
 complete monitoring, or the standard blame soundness and completeness.
They do, however, preserve the top-level shape of all values in typed code.
Furthermore, @|tname| satisfies a heap-based notion of blame soundness.
Blame completeness fails because @|tname| does not update the blame map when an
 untyped function is applied in an untyped context.

@exact|{
\begin{theorem}
  \tname{} does not satisfy $\propts{\sidproj}$.
\end{theorem}
\begin{proofsketch}
  Let $\sexpr_0 = \edynb{\obnd{\sowner_0}{(\tfun{\tnat}{\tnat})}{\sowner_1}}{(\efun{\svar_0}{{-4}})}$.
  \begin{itemize}
    \item $\sWT \sexpr_0 : \tfun{\tnat}{\tnat}~$ in the surface syntax, but
    \item $\conf{\sexpr_0}{\semptymap}{\semptymap} \rredT \conf{\eloc_0}{\vstore_0}{\bstore_0}$, where $\fmapref{\vstore_0}{\eloc_0} = (\efun{\svar_0}{{-4}})$
  \end{itemize}
  and $\not\sWTfull (\efun{\svar_0}{{-4}}) : \tfun{\tnat}{\tnat}$.
\end{proofsketch}
}|

@exact|{
\begin{theorem}
  \tname{} satisfies $\propts{\stagproj}$.
\end{theorem}
\begin{proofsketch}
  Recall that $\stagproj$ maps types to type shapes and the unitype to itself.
  The proof depends on progress and preservation lemmas for the flat typing judgment ($\sWTtag$).
  Although \tname{} lets any well-shaped value cross a boundary, the check
   expressions that appear after elimination forms preserve soundness.
  Suppose that an untyped function crosses a boundary and eventually computes
   an ill-typed result:

  \begin{displayrrarray}
    \conf{(\eapp{\tint}{\eloc_0}{4})}{\vstore_0}{\bstore_0}
    & \nredTX
    & \conf{(\echecktwo{\tint}{{\epair{4}{\esum{\tdyn}{4}{1}}}}{\eloc_0})}{\vstore_0}{\bstore_1}
    \\\sidecond{if \(\fmapref{\vstore_{0}}{\eloc_0}=\efun{\svar_0}{\epair{\svar_0}{\esum{\tdyn}{\svar_0}{1}}}\)}
    \\\sidecond{and $\bstore_1 = \fmapupdate{\bstore_0}{\svalue_0}{\frev{\fmapref{\bstore_0}{\eloc_0}}}$}
  \end{displayrrarray}

  \noindent{}The check expression guards the context.
\end{proofsketch}
}|

@exact|{
\begin{theorem}
  \tname{} does not satisfy $\propcm{}$.
\end{theorem}
\begin{proof}
  A structured value can cross any boundary with a matching shape, regardless
   of the deeper type structure.
  For example, the following annotated rule adds a new label to a pair:

  \begin{displayrrarray}
    \conf{\obars{\edynb{\obnd{\sowner_0}{\tpair{\stype_0}{\stype_1}}{\sowner_1}}{\obbars{\eloc_0}{\sownerlist_2}}}{\sowner_3}}{\vstore_0}{\bstore_0}
    & \nredTXanns &
    \conf{\obbars{\eloc_0}{\fconcat{\sownerlist_2}{\sowner_3}}}{\vstore_0}{\bstore_1}
    \\\sidecond{where $\fmapref{\vstore_0}{\eloc_0} \in \epair{\svalue}{\svalue}$}
  \end{displayrrarray}

\end{proof}
}|

@exact|{
\begin{theorem}\label{thm:T-BS}
  \tname{} does not satisfy $\propbspath$.
\end{theorem}
\begin{proof}
  Let component $\sowner_0$ define a function $f_0$ and export it to
   components $\sowner_1$ and $\sowner_2$.
  If component $\sowner_2$ triggers a type mismatch, as sketched below,
   then \tname{} blames both component $\sowner_2$ and the irrelevant $\sowner_1$:

  \begin{center}
    \begin{tikzpicture}[
      triangle/.style = {fill=black!04, regular polygon, regular polygon sides=3}
    ]
      \node (0) {$\sowner_1$};
      \node (1) [right of=0,xshift=4mm] {$\sowner_0$};
      \node (2) [right of=1,xshift=1.2cm] {$\sowner_2$};
      \node (3) [right of=2,xshift=1mm] {};

      \node (3) [below of=1,yshift=2mm] {\hphantom{$\sowner_1$}};
      \draw[dashed] (1.north west) -- (3.south west);
      \node (4) [below of=2,yshift=2mm] {\hphantom{$\sowner_2$}};
      \draw[dashed] (2.north west) -- (4.south west);

      \node[draw,triangle,shape border rotate=180,inner sep=2pt] (5) [right of=1,xshift=-1mm,yshift=-3mm] {$f$};
      \node (6) [right of=0,xshift=-5mm,yshift=-3mm] {\scalebox{1.4}{\bigcheckmark}};
      \draw[->] (5) -- (6);
      \node (7) [outer sep=2pt,right of=2,xshift=-5mm,yshift=-3mm] {\scalebox{1.8}{!}};
      \draw[->] (5) -- (7);
    \end{tikzpicture}
  \end{center}

  The following term expresses this scenario using a let-expression to
   abbreviate untyped function application:

   {\newcommand{\theexampleint}{5}
    \newcommand{\theexampletype}{(\tfun{\tint}{\tint})}
    \begin{displayrrarray}
      \!\!\!\!\!\!\!\!\!(\eletdecl{f_0}{(\efun{\svar_0}{\epair{\svar_0}{\svar_0}})} \!\!\!\!
      \\
      \!\!\!\!\!\!\!\!\!  ~\eletdecl{f_1}{(\estab{\obnd{\sowner_0}{\theexampletype}{\sowner_1}}{\obars{\edynb{\obnd{\sowner_1}{\theexampletype}{\sowner_0}}{\obars{f_0}{\sowner_0}}}{\sowner_1}})} \!\!\!\!
      \\
      \!\!\!\!\!\!\!\!\!  ~\conf{\estab{\obnd{\sowner_0}{\tint}{\sowner_2}}{\obars{\eapp{\tint}{(\edynb{\obnd{\sowner_2}{\theexampletype}{\sowner_0}}{\obars{f_0}{\sowner_0}})}{\theexampleint}}{\sowner_2}})^{\raisedsowner}}{\emptyset}{\emptyset} \!\!\!\!
    \end{displayrrarray}}

  \noindent{}Reduction ends in a boundary error that blames three components.
\end{proof}
}|

@exact|{
\begin{theorem}\label{thm:T-BC}
  \tname{} does not satisfy $\propbcpath$.
\end{theorem}
\begin{proof}
  An untyped function application in untyped code does not update the blame map:

  \begin{displayrrarray}
    \conf{(\eapp{\tdyn}{\eloc_0}{\svalue_0})}{\vstore_0}{\bstore_0}
    &  \nredTX
    & \conf{(\esubst{\sexpr_0}{\svar_0}{\svalue_0})}{\vstore_0}{\bstore_0}
    \\\sidecond{if \(\fmapref{\vstore_{0}}{\eloc_0}=\efun{\svar_0}{\sexpr_0}\)}
  \end{displayrrarray}

  \noindent{}Such applications lead to incomplete blame when the function
   has previously crossed a type boundary.
  To illustrate, the term below uses an untyped identity
   function $f_1$ to coerce the type of another function ($f_0$).
  After the coercion, an application leads to type mismatch.

  {\newcommand{\theexamplefun}{\efun{\svar_0}{\svar_0}}
  \newcommand{\typea}{(\tfun{\tint}{\tint})}
  \newcommand{\typeb}{(\tfun{\tint}{\tpair{\tint}{\tint}})}
  \newcommand{\typeatxt}{\stype_0}
  \newcommand{\typebtxt}{\stype_1}
  \newcommand{\deepfowners}{\fconcat{\sowner_2}{\fconcat{\sowner_1}{\fconcat{\sowner_0}{\fconcat{\sowner_3}{\fconcat{\sowner_4}{\fconcat{\sowner_3}{\fconcat{\sowner_0}{\sowner_5}}}}}}}}
  \begin{displayrrarray}
    \!\!\!\!\!\!\!\!\!(\eletdecl{f_0}{\estab{\obnd{\sowner_0}{\typeatxt}{\sowner_1}}{(\edynb{\obnd{\sowner_1}{\typeatxt}{\sowner_2}}{(\theexamplefun)})}}\!\!\!\!
    \\[0.7ex]
    \!\!\!\!\!\!\!\!\!~\eletdecl{f_1}{\estab{\obnd{\sowner_0}{(\tfun{\typeatxt}{\typebtxt})}{\sowner_3}}{(\edynb{\obnd{\sowner_3}{(\tfun{\typeatxt}{\typebtxt})}{\sowner_4}}{(\efun{\svar_1}{\svar_1})})}}\!\!\!\!
    \\[0.7ex]
    \!\!\!\!\!\!\!\!\!~\estab{\obnd{\sowner_0}{(\tpair{\tint}{\tint})}{\sowner_5}}{}\!\!\!\!
    \\[0.5ex]\quad\conf{\zeroheight{\obars{\eapp{\tpair{\tint}{\tint}}{(\edynb{\obnd{\sowner_5}{\typebtxt}{\sowner_0}}{\obars{\eapp{\tdyn}{f_1}{f_0}}{\sowner_0}})}{42}}{\sowner_5})^{\raisedsowner}}}{\semptymap}{\semptymap}
  \end{displayrrarray}}

  \noindent{}Reduction ends in a boundary error that does not report the crucial labels $\sowner_3$ and $\sowner_4$.
\end{proof}
}|

The results so far paint a negative picture of the wrapper-free @|tname| semantics.
It fails @${\propcm{}} and  @${\propbcpath{}} because it has no interposition mechanism to
 keep track of type implications for untyped code.
Additionally, its heap-based approach to blame fails @${\propbspath{}} because the
 blame heap conflates different paths in a program.
If several clients use the same library function and one client encounters
 a type mismatch, everyone gets blamed.

We have struggled to find a positive characterization of @|tname|'s blame behavior.
Complete monitoring and blame completeness appear unattainable, even in a
 weakened form, because @|tname| has no control over untyped code.
Blame soundness, however, is possible under a relaxed specification of ownership
 that adds one guideline to the ``natural laws'' from @sectionref{sec:design:laws}:

@exact|{
\begin{enumerate}
  \item[8.] \label{law:heap}
    If an address gains a label, then so does the associated pre-value on the heap.
   \subitem\hfill
     \(\begin{array}[t]{l}
       \oconf{\sfst~{\obars{\eloc_0}{\sowner_0}}}{\vstore_0}{\bstore_0}{\lstore_0}
       \rrarrow
       \oconf{\obars{\eloc_1}{\sowner_0}}{\vstore_0}{\bstore_0}{\fmapupdate{\lstore_0}{\eloc_1}{\eset{\sowner_0}}}
       \\\mbox{\quad\emph{where} $\fmapref{\vstore_0}{\eloc_0} = \epair{\eloc_1}{\eloc_2}$}
     \end{array}\)
   \subitem\hfill
     \emph{\Lawref{law:pos} propagates the outer label, which goes up to}
   \subitem\hfill
     \emph{the ownership heap ($\lstore$).}
\end{enumerate}
}|

@|noindent|Intuitively, the new specification pushes all ownership labels onto the heap.
Rather than push to the value heap (@${\vstore}) directly, though, the
 extended model of @|tname| in the appendix introduces a parallel store
 (@${\lstore}) analogous to the blame heap.

Merging ownership labels on the heap is a non-compositional behavior.
A programmer cannot reason about a local expression without thinking about
 how the rest of the codebase may introduce new owners.
Because of this whole-program action, it is unclear whether the weakened
 notions of blame that follow are useful guarantees to strive for.
Nevertheless, they help characterize @|tname|.

@exact|{
\begin{definition}[heap-based blame soundness and blame completeness]\label{def:blame-heap}
  For all well-formed\/ $\sexpr_0$
  such that\/ $\sexpr_0 \rredX \oconf{\boundaryerror{\sbset_0}{\svalue_0}}{\vstore_0}{\bstore_0}{\lstore_0}$~\emph{:}
  \begin{itemize}
    \itemsep0.1ex
    \item
      $\xsym$ satisfies $\propbsheap$ 
      iff
      $\fbsetsenders{\sbset_0} \subseteq \fvalueowners{\svalue_0} \cup \fmapref{\lstore_0}{\svalue_0}$
    \item
      $\xsym$ satisfies $\propbcheap$
      iff
      $\fbsetsenders{\sbset_0} \supseteq \fvalueowners{\svalue_0} \cup \fmapref{\lstore_0}{\svalue_0}$.
  \end{itemize}
\end{definition}
}|

@figure*[
  "fig:heap-blame"
  @elem{Heap-based ownership consistency for @|tname|}

  @exact|{
   \lbl{\fbox{$\lstore; \sownerenv; \sowner \sWLheap \sexpr; \bstore$}~\missingrules{}}{\begin{mathpar}
    \inferrule*{
      \fbsetsenders{\fmapref{\bstore_0}{\sloc_0}} \subseteq \fmapref{\lstore_0}{\sloc_0}
    }{
      \lstore_0; \sownerenv_0; \sowner_0 \sWLheap \sloc_0; \bstore_0
    }

    \inferrule*{
      \lstore_0; \sownerenv_0; \sowner_0 \sWLheap \sexpr_0; \bstore_0
      \\
      \fbsetsenders{\fmapref{\bstore_0}{\eloc_0}} \subseteq \fmapref{\lstore_0}{\eloc_0}
    }{
      \lstore_0; \sownerenv_0; \sowner_0 \sWLheap \echecktwo{\stype_0}{\sexpr_0}{\eloc_0}; \bstore_0
    }
   \end{mathpar}}
  }|]

@exact{
\begin{theorem}\leavevmode
  \tname{} satisfies $\propbsheap$.
\end{theorem}
\begin{proofsketch}
  By a preservation lemma for the $\sWLheap$ judgment sketched in @figureref{fig:heap-blame}
   and fully-defined in the appendix.
  The judgment ensures that the blame map records a subset of the true owners on each heap-allocated value.
  One subtle case of the proof concerns function application, because the unlabeled
   rule appears to blame a typed function (at address $\eloc_0$) for an unrelated incompatible value:

  \begin{displayrrarray}
      \conf{(\eapp{\stoptional}{\eloc_0}{\svalue_0})}{\vstore_0}{\bstore_0}
      &  \nredTX
      & \conf{\boundaryerror{\frev{\fmapref{\bstore_0}{\eloc_0}}}{\svalue_0}}{\vstore_0}{\bstore_1}
      \\\sidecond{if $\fmapref{\vstore_{0}}{\eloc_0}=\efun{\tann{\svar_0}{{\stype_0}}}{\sexpr_0}$
                  and $\neg \fshallow{\ftagof{\stype_0}}{\fmapref{\vstore_0}{\svalue_0}}$}
      \\\sidecond{where $\bstore_1 = \fmapupdate{\bstore_0}{\svalue_0}{\frev{\bstore_0(\eloc_0)}}$}
  \end{displayrrarray}

  \noindent{}But, the value is not unrelated because the shape check happens
   when this value meets the function's type annotation; that is,
   after the function receives the input value.
  By \lawref{law:neg}, the correct labeling matches the following outline:

  \begin{displayrrarray}
    \obars{\eapp{\stoptional}{\obbars{\eloc_0}{\sownerlist_0}}{\obbars{\svalue_0}{\sownerlist_1}}}{\sowner_0}; \ldots
    &  \nredTXanns & \hspace{2cm}~
    \\\sidestep{ \obars{\boundaryerror{\ldots}{\obbars{\svalue_0}{\fconcat{\sownerlist_1}{\fconcat{\sowner_0}{\frev{\sownerlist_0}}}}}}{\sowner_0}; \ldots }
  \end{displayrrarray}

  \noindent{}Additionally blaming $\fmapref{\bstore_0}{\svalue_0}$
   seems like a useful change to the original \tname{} semantics because it
   offers more information.
  Thanks to heap-based ownership, the technical justification is that
   adding these boundaries preserves the $\propbsheap$\/ property.
\end{proofsketch}
}

@exact|{
\begin{theorem}\leavevmode
  \tname{} does not satisfy $\propbcheap$.
\end{theorem}
\begin{proof}
  Blame completeness fails because \tname{} does not update the blame map
   during an untyped function application.
  Refer to the proof of \theoremref{thm:T-BC} for an example.
\end{proof}
}|

@exact|{
\begin{theorem}\label{thm:T-preorder}
  $\fsym{} \sbehaviorle \tsym{}$.
\end{theorem}
\begin{proofsketch}
  Indirectly, via $\tsym{} \sbehavioreq \asym{}$ (\theoremref{thm:TAsim})
   and $\fsym{} \sbehaviorle \asym{}$ (\theoremref{thm:FAsim}).
\end{proofsketch}
}|


@section[#:tag "sec:design:tech:amnesic"]{@fproperties{@|aname|}}
@latex-label{sec:design:tech:amnesic}

@figure*[
  "fig:amnesic-reduction"
  @elem{@|aname| notions of reduction (1/2)}

  @exact|{
  \lbl{\fbox{\aname{} Syntax}~extends \hyperref[fig:evaluation-ho]{\syntaxho{}}}{
    \begin{langarray}
      \svalue & \BNFeq &
        \sint \mid
        \snat \mid
        \epair{\svalue}{\svalue} \mid
        \efun{\svar}{\sexpr} \mid
        \efun{\tann{\svar}{\stype}}{\sexpr} \mid
      \\ & &
        \emon{\obnd{\sowner}{\tfun{\stype}{\stype}}{\sowner}}{\svalue} \mid
        \emon{\obnd{\sowner}{\tpair{\stype}{\stype}}{\sowner}}{\svalue} \mid
        \ehist{\sbset}{\svalue}
    \end{langarray}
  }

  \bigskip
  \lbl{\fbox{$\sexpr \nredAS \sexpr$}~\missingrules{}}{
    \begin{rrarray}
      \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \nredAS
      & \emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      \\\sidecond{if $\fshallow{\tagof{\stype_0}}{\svalue_0}$}
      \\\sidecond{and $\fremtrace{\svalue_0} \in {\epair{\svalue}{\svalue}} \cup
                                     (\efun{\tann{\svar}{{\stype}}}{\sexpr}) \cup
                                     (\emon{\sbnd}{\svalue})$}
      \\[0.5ex]
      \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \nredAS
      & \svalue_0
      \\\sidecond{if $\fshallow{\tagof{\stype_0}}{\svalue_0}$
                  and $\fremtrace{\svalue_0} \in \sint$}
      \\[0.5ex]
      \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \nredAS
      \\\sidestep{ \boundaryerror{\funion{\eset{\obnd{\sowner_0}{\stype_0}{\sowner_1}}}{\sbset_0}}{\svalue_0} }
      \\\sidecond{if $\neg\fshallow{\tagof{\stype_0}}{\svalue_0}$
                  and $\sbset_0 \eeq \fgettrace{\svalue_0}$}
      \\[1.0ex]
      \efst{\stype_0}{(\emon{\obnd{\sowner_0}{\stype_1}{\sowner_1}}{\svalue_0})}
      & \nredAS
      & \edynb{\sbnd_0}{(\efst{\tdyn}{\svalue_0})}
      \\\sidecond{where $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_0}{\sowner_1}$}
      \\[0.5ex]
      \esnd{\stype_0}{(\emon{\obnd{\sowner_0}{\stype_1}{\sowner_1}}{\svalue_0})}
      & \nredAS
      & \edynb{\sbnd_0}{(\esnd{\tdyn}{\svalue_0})}
      \\\sidecond{where $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_0}{\sowner_1}$}
      \\[0.5ex]
      \eapp{\stype_0}{(\emon{\obnd{\sowner_0}{\tfun{\stype_1}{\stype_2}}{\sowner_1}}{\svalue_0})}{\svalue_1}
      & \nredAS
      \\\sidestep{ \edynb{\sbnd_0}{(\eapp{\tdyn}{\svalue_0}{(\estab{\sbnd_1}{\svalue_1})})} }
      \\\sidecond{where $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_0}{\sowner_1}$
                  and $\sbnd_1 \sassign \obnd{\sowner_1}{\stype_1}{\sowner_0}$}
    \end{rrarray}
  }
}|]

@figure*[
  "fig:amnesic-reduction2"
  @elem{@|aname| notions of reduction (2/2)}

  @exact|{
  \lbl{\fbox{$\sexpr \nredAD \sexpr$}~\missingrules{}}{
    \begin{rrarray}
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \nredAD
      & \emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      \\\sidecond{if $\fshallow{\tagof{\stype_0}}{\svalue_0}$
                  and $\svalue_0 \in {\epair{\svalue}{\svalue}} \cup {(\efun{\tann{\svar}{{\stype}}}{\sexpr})}$}
      \\[0.5ex]
      \estab{\sbnd_0}{(\emon{\sbnd_1}{(\ehopt{\sbset_0}{\svalue_0})})}
      & \nredAD
      & \eprehist{(\funion{\eset{\sbnd_0, \sbnd_1}}{\sbset_0})}{\svalue_0}
      \\\sidecond{if $\sbnd_0 \eeq \obnd{\sowner_0}{\stype_0}{\sowner_1}$
                  and $\fshallow{\tagof{\stype_0}}{\svalue_0}$}
      \\\sidecond{and $\svalue_0 \in {\epair{\svalue}{\svalue}} \cup (\efun{\svar}{\sexpr}) \cup (\emon{\sbnd}{(\efun{\tann{\svar}{\stype}}{\sexpr})}) \cup (\emon{\sbnd}{\epair{\svalue}{\svalue}})$}
      \\[0.5ex]
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sint_0}
      & \nredAD
      & \sint_0
      \\\sidecond{if $\fshallow{\tagof{\stype_0}}{\sint_0}$}
      \\[0.5ex]
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \nredAD
      & \tagerrorS
      \\\sidecond{if $\neg\fshallow{\tagof{\stype_0}}{\svalue_0}$}
      \\[1.0ex]
      \efst{\tdyn}{(\ehopt{\sbset_0}{(\emon{\obnd{\sowner_0}{\tpair{\stype_0}{\stype_1}}{\sowner_1}}{\svalue_0})})}
      & \nredAD
      \\\sidestep{ \eprehist{\sbset_0}{(\estab{\sbnd_0}{(\efst{\stype_0}{\svalue_0})})} }
      \\\sidecond{where $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_0}{\sowner_1}$}
      \\[0.5ex]
      \esnd{\tdyn}{(\ehopt{\sbset_0}{(\emon{\obnd{\sowner_0}{\tpair{\stype_0}{\stype_1}}{\sowner_1}}{\svalue_0})})}
      & \nredAD
      \\\sidestep{ \eprehist{\sbset_0}{(\estab{\sbnd_0}{(\esnd{\stype_1}{\svalue_0})})} }
      \\\sidecond{where $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_1}{\sowner_1}$}
      \\[0.5ex]
      \eapp{\tdyn}{(\ehopt{\sbset_0}{(\emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0})})}{\svalue_1}
      & \nredAD
      \\\sidestep{ \eprehist{\sbset_0}{(\estab{\sbnd_0}{(\eapp{\stype_2}{\svalue_0}{\sexpr_0})})} }
      \\\sidecond{where $\stype_0 = \tfun{\stype_1}{\stype_2}$
                  and $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_2}{\sowner_1}$
                  and $\sbnd_1 \sassign \obnd{\sowner_1}{\stype_1}{\sowner_0}$}
      \\\sidecond{and $\sexpr_0 \sassign (\edynb{\sbnd_1}{(\faddtrace{\frev{\sbset_0}}{\svalue_1})})$}
      \\[1.0ex]
      \eprehist{\sbset_0}{\svalue_0}
      & \nredAD
      & \svalue_1
      \\\sidecond{where $\svalue_1 \sassign \faddtrace{\sbset_0}{\svalue_0}$}
    \end{rrarray}
  }

  \bigskip
  \lbl{\fbox{$\sexpr \rredA \sexpr$} ${}={} \rtclosure{\nredAS, \nredAD, \snreddyn, \snredsta}$}{}
}|]

The @|aname| semantics employs the same dynamic checks as @|tname| but
 offers path-based blame information.
Whereas @|tname| indirectly tracks blame through heap addresses,
 @|aname| uses trace wrappers to keep boundaries alongside the value
 at hand.

@|aname| bears a strong resemblance to the @|fname|
 semantics.
Both use guard wrappers in the same way, keeping a sticky ``inner'' wrapper
 around typed values and a temporary ``outer'' wrapper in typed contexts.
There are two crucial differences:
@itemlist[
  @item{
    When @|aname| removes a guard wrapper, it saves the boundary specification
     in a trace wrapper.
    The number of boundaries in a trace can grow without bound, but the
     number of wrappers around a value is limited to three.
  }@item{
    At elimination forms, @|aname| checks only the context's type annotation.
    Suppose an untyped function enters typed code at one type and is
     used at a supertype:
     @exact{\\\hfill{}\qquad @${\eapp{\tint}{(\emon{\obnd{\sowner_0}{(\tfun{\tnat}{\tnat})}{\sowner_1}}{\efun{\svar_0}{{-7}}})}{2}} \hfill{}\\}
    @|aname| runs this application without error but @|fname| raises a boundary error.
  }]

@|noindent|Thus, the following wrapped values can occur at run-time.
Note that @${(\ehopt{\sbset}{\sexpr})} is short for an expression that may or may not have a trace wrapper (@figureref{fig:amnesic-meta}).

@exact|{
\smallskip
\(\hfill
\begin{array}[t]{lcl}
  \svaluestat
   & \BNFeq & \emon{\sbnd}{(\ehopt{\sbset}{\epair{\svalue}{\svalue}})} \\[1pt]
   & \mid   & \emon{\sbnd}{(\ehopt{\sbset}{\efun{\svar}{\sexpr}})} \\[1pt]
   & \mid   & \emon{\sbnd}{(\ehopt{\sbset}{(\emon{\sbnd}{\epair{\svalue}{\svalue}})})} \\[1pt]
   & \mid   & \zerowidth{\emon{\sbnd}{(\ehopt{\sbset}{(\emon{\sbnd}{\efun{\tann{\svar}{\stype}}{\sexpr}})})}}
\end{array}
\hfill
\begin{array}[t]{lcl}
  \svaluedyn
  & \BNFeq & \ehist{\sbset}{\sint} \\
  & \mid & \ehist{\sbset}{\epair{\svalue}{\svalue}} \\
  & \mid & \ehist{\sbset}{\efun{\svar}{\sexpr}} \\
  & \mid & \ehopt{\sbset}{(\emon{\sbnd}{\epair{\svalue}{\svalue}})} \\
  & \mid & \ehopt{\sbset}{(\emon{\sbnd}{\efun{\tann{\svar}{\stype}}{\sexpr}})}
\end{array}
\hfill\)
\smallskip
}|

The elimination rules for guarded pairs show the clearest
 difference between checks in @|aname| and @|fname|.
@|aname| ignores the type in the guard.
@|fname| ignores the type annotation on the primitive operation.

@figure*[
  "fig:amnesic-meta"
  @elem{Metafunctions for @|aname|}

  @exact|{
  \begin{minipage}[t]{0.5\columnwidth}
    \(\faddtrace{\sbset_0}{\svalue_0}
      \\ {} \feq
       \left\{\begin{array}{ll}
          \svalue_0
          \\ & \mbox{if $\sbset_0 \eeq \emptyset$}
          \\
          \zerowidth{\ehist{(\sbset_0 \cup \sbset_1)\,}{\svalue_1}}
          \\ & \mbox{if $\svalue_0 \eeq \ehist{\sbset_1}{\svalue_1}$}
          \\
          \zerowidth{\ehist{\sbset_0}{\svalue_0}}
          \\ & \mbox{if $\svalue_0 \not\in \ehist{\sbset}{\svalue}$ and $\sbset_0 \neq \emptyset\!\!\!\!$}
       \end{array}\right.\)

  \end{minipage}\begin{minipage}[t]{0.5\columnwidth}
    \(\fgettrace{\svalue_0}
      \\ {} \feq
        \left\{\begin{array}{ll}
          \sbset_0
          & \mbox{if $\svalue_0 \eeq \ehist{\sbset_0}{\svalue_1}$}
          \\
          \emptyset
          & \mbox{if $\svalue_0 \not\in \ehist{\sbset}{\svalue}$}
        \end{array}\right.\)

    \bigskip
    \(\fremtrace{\svalue_0}
      \\ {} \feq
        \left\{\begin{array}{ll}
          \svalue_1
          & \mbox{if $\svalue_0 \eeq \ehist{\sbset_0}{\svalue_1}$}
          \\
          \svalue_0
          & \mbox{if $\svalue_0 \not\in \ehist{\sbset}{\svalue}$}
        \end{array}\right.\)
  \end{minipage}

  \[(\ehopt{\sbset_0}{\svalue_0}) \eeq \svalue_1
     {} \sabbreveq
      \mbox{$\fremtrace{\svalue_1} \eeq \svalue_0$ and $\fgettrace{\svalue_1} \eeq \sbset_0$} \]
}|]

@Figureref{fig:amnesic-reduction}
 defines three metafunctions and one abbreviation for trace wrappers.
The metafunctions extend, retrieve, and remove the boundaries associated
 with a value.
The abbreviation lets reduction rules accept optionally-traced values.

@|aname| satisfies full type soundness thanks to guard wrappers
 and fails complete monitoring because it drops wrappers.
This is no surprise, since @|aname| creates and removes guard
 wrappers in the same manner as @|fname|.
Unlike the @|fname| semantics, @|aname| uses trace wrappers to remember
 the boundaries that a value has crossed.
This information leads to sound and complete blame messages.

@exact|{
\begin{theorem}\label{thm:A-TS}
  \aname{} satisfies $\propts{\sidproj}$.
\end{theorem}
\begin{proofsketch}
  By progress and preservation lemmas for the higher-order typing judgment ($\sWTfull$).
  \aname{} creates and drops wrappers in the same manner as \fname{} (\theoremref{thm:F-TS}),
   so the only interesting proof cases concern elimination forms.
  For example, when \aname{} extracts an element from a guarded pair it ignores
   the type in the guard ($\tpair{\stype_1}{\stype_2}$):

  \begin{displayrrarray}
    \efst{\stype_0}{(\emon{\obnd{\sowner_0}{\tpair{\stype_1}{\stype_2}}{\sowner_1}}{\svalue_0})}
    & \nredAS
    & \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{(\efst{\tdyn}{\svalue_0})}
  \end{displayrrarray}

  \noindent{}The new boundary enforces the context's assumption ($\stype_0$)
   instead, but we know that $\stype_0$ is a supertype of $\stype_1$ by the
   higher-order typing judgment.
\end{proofsketch}
}|

@exact|{
\begin{theorem}
  \aname{} does not satisfy $\propcm{}$.
\end{theorem}
\begin{proofsketch}
  Removing a wrapper creates a value with more than one label:

  \begin{displayrrarray}
    \obars{\estab{\obnd{\sowner_0}{(\tfun{\stype_0}{\stype_1})}{\sowner_1}}{\obbars{\emon{\sbnd_1}{\obbars{\ehist{\sbset_0}{\obbars{\efun{\svar_0}{\svar_0}}{\sownerlist_2}}}{\sownerlist_3}}}{\sownerlist_4}}}{\sowner_5}
      & \nredADanns
    \\[1ex]\sidestep{\obbars{\eprehist{(\funion{\eset{\obnd{\sowner_0}{(\tfun{\stype_0}{\stype_1})}{\sowner_1}, \sbnd_1}}{\sbset_0})}{\obbars{\efun{\svar_0}{\svar_0}}{\sownerlist_2}}}{\fconcat{\sownerlist_3}{\fconcat{\sownerlist_4}{\sowner_5}}}}
  \end{displayrrarray}

\end{proofsketch}
}|

@figure*[
  "fig:path-blame"
  @elem{Path-based ownership consistency for trace wrappers}

  @exact|{
  \lbl{\fbox{$\sownerenv; \sowner \sWLpath \sexpr$}~\missingrules{}, extends \hyperref[fig:surface-ownership]{$\,\sownerenv; \sowner \sWLsingle \sexpr$}}{\begin{mathpar}
      \inferrule*{
        \sbset_0 = \eset{\fconcat{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\fconcat{\cdots}{\obnd{\sowner_{n-1}}{\stype_{n-1}}{\sowner_n}}}}
        \\
        \sownerenv_0; \sowner_n \sWLpath \svalue_0
      }{
        \sownerenv_0; \sowner_0 \sWLpath \obars{\ehist{\sbset_0}{\obbars{\svalue_0}{\sowner_n \cdots \sowner_1}}}{\sowner_0}
      }
  \end{mathpar}}
  }|]

@exact{
\begin{theorem}
  \aname{} satisfies $\propbspath$\/ and $\propbcpath$.
\end{theorem}
\begin{proofsketch}
  By progress and preservation lemmas for a path-based consistency judgment, $\sWLpath$, that weakens
   single-owner consistency to allow multiple labels around a trace-wrapped value.
  Unlike the heap-based consistency for \tname{}, which requires an entirely
   new judgment, path-based consistency only replaces the rules for trace wrappers
   (shown in @figureref{fig:path-blame}) and trace expressions.
  Now consider the guard-dropping rule:

  \begin{displayrrarray}
    \obars{\estab{\obnd{\sowner_0}{(\tfun{\stype_0}{\stype_1})}{\sowner_1}}{\obbars{\emon{\sbnd_1}{\obbars{\ehist{\sbset_0}{\obbars{\efun{\svar_0}{\svar_0}}{\sownerlist_2}}}{\sownerlist_3}}}{\sownerlist_4}}}{\sowner_5}
      & \nredADanns
    \\[1ex]\sidestep{\obbars{\eprehist{(\funion{\eset{\obnd{\sowner_0}{(\tfun{\stype_0}{\stype_1})}{\sowner_1}, \sbnd_1}}{\sbset_0})}{\obbars{\efun{\svar_0}{\svar_0}}{\sownerlist_2}}}{\fconcat{\sownerlist_3}{\fconcat{\sownerlist_4}{\sowner_5}}}}
  \end{displayrrarray}

  \noindent{}Path-consistency for the redex implies that $\sownerlist_3$ and $\sownerlist_4$
   match the component names on the boundary $\sbnd_1$, and that the client side
   of $\sbnd_1$ matches the outer sender $\sowner_1$.
  Thus the new labels on the result match the sender names on the two new
   boundaries in the trace.
\end{proofsketch}
}

@exact|{
\begin{theorem}\label{thm:TAsim}
  $\tsym{} \sbehavioreq \asym{}$.
\end{theorem}
\begin{proofsketch}
  By a stuttering simulation between \tname{} and \aname{}.
  \aname{} may take extra steps at an elimination form and to combine traces
   into one wrapper.
  \tname{} takes extra steps to place pre-values on the heap and to conservatively
   check the result of elimination forms.
  In fact, @|aname| and @|tname| behave exactly the same aside from bookkeeping
   to create wrappers and track blame.
\end{proofsketch}
}|

@exact|{
\begin{theorem}\label{thm:FAsim}
  $\fsym{} \sbehaviorle \asym{}$.
\end{theorem}
\begin{proofsketch}
  $\!\!$ By a lock-step bisimulation.
  The only difference between \fname{} and \aname{} comes from subtyping.
  \fname{} uses wrappers to enforce the type on a boundary.
  \aname{} uses boundary types only for an initial shape check, and instead uses the static types
   in typed code to guide checks at elimination forms.
  In the following $\asym{} \not\sbehaviorle \fsym{}$ example, a boundary declares one type and an elimination
   form requires a weaker type:

  \begin{displayrrarray}
    \efst{\tint}{(\edynb{\obnd{\sowner_0}{(\tpair{\tnat}{\tnat})}{\sowner_1}}{\epair{{-4}}{4}})}
  \end{displayrrarray}

  \noindent{}Since ${{-4}}$ is an integer, \aname{} reduces to a value.
  \fname{} detects an error.
\end{proofsketch}
}|


@section[#:tag "sec:design:tech:erasure"]{@fproperties{@|ename|}}
@latex-label{sec:design:tech:erasure}

@figure*[
  "fig:erasure-reduction"
  @elem{@|ename| notions of reduction}

  @exact|{
  \lbl{\fbox{\ename{} Syntax}~extends \hyperref[fig:evaluation-eo]{\syntaxeo{}}}{
    \begin{langarray}
      \svalue & \BNFeq &
        \sint \mid \snat \mid \epair{\svalue}{\svalue} \mid \efun{\svar}{\sexpr} \mid \efun{\tann{\svar}{\stype}}{\sexpr}
    \end{langarray}
  }

  \bigskip
  \lbl{\fbox{{$\sexpr \nredEX \sexpr$}}}{
    \begin{rrarray}
      \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \nredEX
      & \svalue_0
      \\[0.5ex]
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \nredEX
      & \svalue_0
      \\[1.0ex]
      \eunopt{\stype_0}{\svalue_0}
      & \nredEX
      & \boundaryerror{\emptyset}{\svalue_0}
      \\\sidecond{if {$\sdelta(\sunop, {\svalue_0})$} is undefined}
      \\[0.5ex]
      \eunopt{\tdyn}{\svalue_0}
      & \nredEX
      & \tagerrorD
      \\\sidecond{if {$\sdelta(\sunop, {\svalue_0})$} is undefined}
      \\[0.5ex]
      \eunopt{\stoptional}{\svalue_0}
      & \nredEX
      & \sdelta(\sunop, {\svalue_0})
      \\\sidecond{if {$\sdelta(\sunop, {\svalue_0})$} is defined}
      \\[0.5ex]
      \ebinopt{\stype_0}{\svalue_0}{\svalue_1}
      & \nredEX
      & \boundaryerror{\emptyset}{\svalue_i}
      \\\sidecond{if {$\sdelta(\sbinop, {\svalue_0}, {\svalue_1})$} is undefined and $\svalue_i \not\in \tint$}
      \\[0.5ex]
      \ebinopt{\tdyn}{\svalue_0}{\svalue_1}
      & \nredEX
      & \tagerrorD
      \\\sidecond{if {$\sdelta(\sbinop, {\svalue_0}, {\svalue_1})$} is undefined}
      \\[0.5ex]
      \ebinopt{\stoptional}{\svalue_0}{\svalue_1}
      & \nredEX
      & \sdelta(\sbinop, {\svalue_0}, {\svalue_1})
      \\\sidecond{if {$\sdelta(\sbinop, {\svalue_0}, {\svalue_1})$} is defined}
      \\[0.5ex]
      \eapp{\stype_0}{\svalue_0}{\svalue_1}
      & \nredEX
      & \boundaryerror{\emptyset}{\svalue_0}
      \\\sidecond{if $\svalue_0 \not\in (\efun{\svar}{\sexpr}) \cup (\efun{\tann{\svar}{\stype}}{\sexpr})$}
      \\[0.5ex]
      \eapp{\tdyn}{\svalue_0}{\svalue_1}
      & \nredEX
      & \tagerrorD
      \\\sidecond{if $\svalue_0 \not\in (\efun{\svar}{\sexpr}) \cup (\efun{\tann{\svar}{\stype}}{\sexpr})$}
      \\[0.5ex]
      \eapp{\stoptional}{(\efun{\tann{\svar_0}{\stype_0}}{\sexpr_0})}{\svalue_0}
      & \nredEX
      & \esubst{\sexpr_0}{\svar_0}{\svalue_0}
      \\[0.5ex]
      \eapp{\stoptional}{(\efun{\svar_0}{\sexpr_0})}{\svalue_0}
      & \nredEX
      & \esubst{\sexpr_0}{\svar_0}{\svalue_0}
    \end{rrarray}
  }
}|]

@Figureref{fig:erasure-reduction} presents the values and notions of reduction
 for the @|ename| semantics.
@|ename| ignores all types at runtime.
As the first two reduction rules show, any value may cross any boundary.
When an incompatible value reaches an elimination form, the result depends
 on the context.
In untyped code, the redex steps to a standard tag error.
In typed code, however, the malformed redex indicates that an ill-typed
 value crossed a boundary.
Thus @|ename| ends with a boundary error at the last possible moment;
 these errors come with no information because there is no record of the
 relevant boundary.

@exact|{
\begin{theorem}
  \ename{} satisfies neither $\propts{\sidproj}$\/ nor $\propts{\stagproj}$.
\end{theorem}
\begin{proof}
  Dynamic-to-static boundaries are unsound.
  An untyped function, for example, can enter a typed context that expects an integer:

  \begin{displayrrarray}
    \edynb{\obnd{\sowner_0}{\tint}{\sowner_1}}{(\efun{\svar_0}{42})}
    & \nredEX
    & (\efun{\svar_0}{42})
  \end{displayrrarray}

\end{proof}
}|

@exact|{
\begin{theorem}
  \ename{} satisfies $\propts{\sdynproj}$.
\end{theorem}
\begin{proofsketch}
  By progress and preservation lemmas for the erased, or dynamic-typing, judgment ($\sWTnone$).
  Given well-formed input, every $\nredEX$ rule yields a dynamically-typed result.
\end{proofsketch}
}|

@exact|{
\begin{theorem}
  \ename{} does not satisfy $\propcm{}$.
\end{theorem}
\begin{proofsketch}
  A static-to-dynamic boundary can create a value with multiple labels:

  \begin{displayrrarray}
    \obars{\estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\obars{\svalue_0}{\sowner_2}}}{\sowner_3}
    & \nredEXanns &
    \obbars{\svalue_0}{\fconcat{\sowner_2}{\sowner_3}}
  \end{displayrrarray}

\end{proofsketch}
}|

@exact|{
\begin{theorem}\leavevmode
  \begin{itemize}
    \item \ename{} satisfies\/ $\propbspath$.
    \item \ename{} does not satisfy\/ $\propbcpath$.
  \end{itemize}
\end{theorem}
\begin{proofsketch}
  An \ename{} boundary error blames an empty set, for example:

  \begin{displayrrarray}
    \efst{\tint}{(\efun{\svar_0}{\svar_0})}
    & \nredEX &
    \boundaryerror{\emptyset}{(\efun{\svar_0}{\svar_0})}
  \end{displayrrarray}

  \noindent{}The empty set is trivially sound and incomplete.
\end{proofsketch}
}|

@exact|{
\begin{theorem}
  $\asym{} \sbehaviorle \esym{}$.
\end{theorem}
\begin{proofsketch}
  By a stuttering simulation.
  \aname{} takes extra steps at elimination forms, to enforce types, and
   to create trace wrappers.

  As a countexample showing $\asym{} \not\sbehaviorle \esym{}$, the following
   applies an untyped function:

  \begin{displayrrarray}
    \eapp{\tnat}{(\edynb{\obnd{\sowner_0}{(\tfun{\tnat}{\tnat})}{\sowner_1}}{(\efun{\svar_0}{{-9}})})}{4}
  \end{displayrrarray}

  \noindent{}\aname{} checks for a natural-number result and errors,
   but \ename{} checks nothing.
\end{proofsketch}
}|

