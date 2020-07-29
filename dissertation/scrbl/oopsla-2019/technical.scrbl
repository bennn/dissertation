#lang greenman-thesis/include
@(require greenman-thesis/oopsla-2019/main)

@title[#:tag "sec:design:technical"]{Technical Development}

@; TODO  first-order => shallow !!!

@; % All definitions, minimal descriptions.
@; % TODO
@; % - draw HO / FO / EO connection ...
@; %     SURFACE
@; %     | | | | \ \
@; %     H C F T A E
@; %     ^ ^ ^   ^   higher-order
@; %           ^     first-order
@; %               ^ erasure
@; % - see appendix for lifted reductions


@; % TODO better introduction
This section presents all the technical details: the model,
 the semantics, and the properties that each semantics satisfies.
Full proofs appear in an earlier technical report@~cite{gfd-tr-2019}.

@; %Quick overview:
@; %\begin{itemize}
@; %  \item
@; %    common surface language, typing rules, and ownership rules
@; %  \item
@; %    three evaluation-language extensions, three corresponding typing judgments, two corresponding ownership judgments
@; %  \item
@; %    six semantics and their properties
@; %\end{itemize}

@section[#:tag "sec:design:surface-language"]{Surface Language, Types, and Ownership}

@figure*[
  "fig:surface-language"
  @elem{Surface syntax and typing rules}

  @exact|{
  \lbl{\fbox{Surface Syntax}}{
    \begin{tabular}{ll}
      \begin{langarray}
        \sexpr & \BNFeq &
          \svar \mid \sint \mid \snat \mid
          \epair{\sexpr}{\sexpr} \mid
          \efun{\svar}{\sexpr} \mid
          \efun{\tann{\svar}{\stype}}{\sexpr} \mid
        \\ & &
          \eapp{\stoptional}{\sexpr}{\sexpr} \mid
          \eunopt{\stoptional}{\sexpr} \mid
        \\ & &
          \ebinopt{\stoptional}{\sexpr}{\sexpr} \mid
          \edynb{\sbnd}{\sexpr} \mid
          \estab{\sbnd}{\sexpr}
        \\
        \stype & \BNFeq &
          \tint \mid \tnat \mid \tfun{\stype}{\stype} \mid \tpair{\stype}{\stype}
        \\
        \stoptional & \BNFeq &
          \stype \mid \tdyn
        \\
        \sunop & \BNFeq &
          \sfst \mid \ssnd
        \\
        \sbinop & \BNFeq &
          \ssum \mid \squotient
      \end{langarray}
      &
      \begin{langarray}
        \sbnd & \BNFeq &
          \obnd{\sowner}{\stype}{\sowner}
        \\
        \sblist & \BNFeq &
          \cdot \mid \fconcat{\sbnd}{\sblist}
        \\
        \sbset & \BNFeq &
          \powerset{\sbnd}
        \\
        \sowner & \BNFeq &
          \textrm{\scountable{} set of names}
        \\
        \stypeenv & \BNFeq &
          \snil \mid \fcons{\tann{\svar}{\stoptional}}{\stypeenv}
        \\
        \sint & \BNFeq & \integers
        \\
        \snat & \BNFeq & \naturals
      \end{langarray}
    \end{tabular}
  }

  \smallskip
  \lbl{\fbox{$\stypeenv \sWT \sexpr : \stype$} \missingrules{}}{
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
        \\
        \sDelta(\sunop, \stype_1) \subteq \stype_0
      }{
        \stypeenv_0 \sWT \eunopt{\stype_0}{\sexpr_0} : \stype_0
      }

      \inferrule*{
        \stypeenv_0 \sWT \sexpr_0 : \tdyn
      }{
        \stypeenv_0 \sWT \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sexpr_0} : \stype_0
      }

      \inferrule*{
        \stypeenv_0 \sWT \sexpr_0 : \stype_1
        \\
        \stype_1 \subteq \stype_0
      }{
        \stypeenv_0 \sWT \sexpr_0 : \stype_0
      }

    \end{mathpar}
  }

  \smallskip
  \lbl{\fbox{$\stypeenv \sWT \sexpr : \tdyn$} \missingrules{}}{
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

@Figure-ref{fig:surface-language} presents the syntax and typing judgments for
 the common surface language sketched in @sectionref{sec:design:basic:surface}.
Expressions @${\sexpr} include variables, integers, pairs, functions,
 primitive operations, applications, and boundary expressions.
The primitive operations consist of pair projections
 and arithmetic functions.
A @${\sdyn} boundary expression embeds a dynamically-typed expression into
 a statically-typed context,
 and a @${\ssta} boundary expression embeds a typed expression in an untyped context.

A type specification @${\stoptional} is either a static type @${\stype}
 or the symbol @${\tdyn} for untyped code.
Fine-grained mixtures of @${\stype} and @${\tdyn}, such as @${\tpair{\tint}{\tdyn}},
 are gramatically invalid.
Instead, the model describes two parallel surface languages
 that are connected only through boundary expressions.
A statically-typed expression @${\sexpr_0} is one where the judgment
 @${\stypeenv_0 \sWT \sexpr_0 : \stype_0} holds for some type environment
 and type.
A dynamically-typed expression @${\sexpr_1} is one for which
 @${\stypeenv_1 \sWT \sexpr_1 : \tdyn} holds for some environment.
The static typing judgment relies on a notion of subtyping,
 based on the relation @${\tnat \subteq \tint},
 and a metafunction @${\sDelta} that
 determines the output type of a primitive operation.

Every function application and operator application comes with a type
 specification @${\stoptional} for the expected result.
These annotations serve two purposes:
 to determine the behavior of the @|tname| and @|aname| semantics,
 and to disambiguate statically-typed and dynamically-typed redexes.
An implementation could easily infer valid annotations.
The model keeps them explicit to more easily formulate examples where subtyping
 affects behavior; for instance, the terms @${\eunopt{\tnat}{\sexpr_0}} and
 @${\eunopt{\tint}{\sexpr_0}} may give different results for the same input
 expression.


@figure*[
  "fig:surface-ownership"
  @elem{Ownership syntax and single-owner consistency}
  @exact|{
  \begin{minipage}[t]{0.50\columnwidth}
    \lbl{\fbox{Ownership Syntax}}{
      \begin{langarray}
        \sexpr & \BNFeq &
          \ldots \mid
          \obars{\sexpr}{\sowner} \mid
          \edyn{\sbnd}{\obars{\sexpr}{\sowner}} \mid
          \esta{\sbnd}{\obars{\sexpr}{\sowner}}
        \\
        \sowner & \BNFeq &
          \textrm{\scountable{} set}
        \\
        \sownerenv & \BNFeq &
          \snil \mid \fcons{\tann{\svar}{\sowner}}{\sownerenv}
      \end{langarray}
    }
  \end{minipage}\begin{minipage}[t]{0.50\columnwidth}
    % TODO exists erasure?
    \lbl{\fbox{$\fwellformed{\sexpr}{\stoptional}$}}{\(\begin{array}{l@{}l} % well-formed expression
      \fwellformed{\obars{\sexpr_0}{\sowner_0}}{\stype_0} &
      \mbox{ iff } \sowner_0 \sWLsingle \obars{\sexpr_0}{\sowner_0}
      \mbox{ and} \sWT \sexpr_0 : \stype_0\!\!
      \\
      \fwellformed{\obars{\sexpr_0}{\sowner_0}}{\tdyn} &
      \mbox{ iff } \sowner_0 \sWLsingle \obars{\sexpr_0}{\sowner_0}
      \mbox{ and} \sWT \sexpr_0 : \tdyn\!\!
    \end{array}\)}
  \end{minipage}

  \smallskip
  \lbl{\fbox{$\sownerenv; \sowner \sWLsingle \sexpr$} \missingrules{}}{\begin{mathpar}
      \inferrule*{
        \tann{\svar_0}{\sowner_0} \in \sownerenv_0
      }{
        \sownerenv_0; \sowner_0 \sWLsingle \svar_0
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

%      \inferrule*{
%        \sownerenv_0; \sowner_0 \sWLsingle \sexpr_0
%      }{
%        \sownerenv_0; \sowner_0 \sWLsingle \eunopt{\stoptional}{\sexpr_0}
%      }
%
%      \inferrule*{
%        \sownerenv_0; \sowner_0 \sWLsingle \sexpr_0
%        \\
%        \sownerenv_0; \sowner_0 \sWLsingle \sexpr_1
%      }{
%        \sownerenv_0; \sowner_0 \sWLsingle \eapp{\stoptional}{\sexpr_0}{\sexpr_1}
%      }
%
      \inferrule*{
        \sownerenv_0; \sowner_0 \sWLsingle \sexpr_0
      }{
        \sownerenv_0; \sowner_0 \sWLsingle \obars{\sexpr_0}{\sowner_0}
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

    \end{mathpar}
  }

}|]

@Figure-ref{fig:surface-ownership} extends the surface language
 with ownership labels and introduces a single-owner ownership consistency relation.
These labels record the component from which an expression originates.

The extended syntax brings one addition,
 labeled expressions @${\obars{\sexpr}{\sowner}},
 and a requirement that that boundary expressions label their inner compenent.
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
The expression @${\obars{\obars{42}{\sowner_0}}{\sowner_1}} is inconsistent
 for any parameters.

Labels correspond to component names but come from a distinct set.
Thus the expression @${(\edynb{\obnd{\sowner_0}{\tnat}{\sowner_1}}{\obars{\svar_0}{\sowner_1}})}
 contains two names, @${\sowner_0} and @${\sowner_1}, and one label @${{}^{\sowner_1}} that
 matches the inner component name.
The distinction separates an implementation from a specification.
A semantics/implementation manipulates component names to explain
 errors.
Labels serve as a specification to judge whether a semantics uses component names in a sensible way.

Lastly, a surface expression is well-formed
 (@${\fwellformed{\sexpr}{\stoptional}}) if it satisfies a typing judgment---either
 static or dynamic---and single-owner consistency under some labeling and context label @${\sowner_0}.
The theorems below all require well-formed expressions.


@section[#:tag "sec:design:tech:eval"]{Three Evaluation Languages}

Each semantics requires a unique evaluation language, but overlaps among
 these six languages motivate three common definitions.
A @emph{higher-order} evaluation language supports type-enforcement strategies
 that require wrappers.
A @emph{first-order} language, with simple checks rather than wrappers,
 supports @|tname|.
And an @emph{erasure} language supports the compilation of typed and
 untyped code to a common untyped host.

@figure*[
  "fig:evaluation-common"
  @elem{Common evaluation syntax and Metafunctions}
  @exact|{
  \lbl{\fbox{Common Evaluation Syntax}}{ % extends Surface
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
        \ctxhole \mid
        \eapp{\stoptional}{\ctx}{\sexpr} \mid
        \eapp{\stoptional}{\svalue}{\ctx} \mid
        \ldots \mid
        \edynb{\sbnd}{\ctx} \mid
        \estab{\sbnd}{\ctx}
    \end{langarray}
  }

  \bigskip
  \begin{minipage}[t]{0.5\columnwidth}
    \(\tagof{\stype_0}
        \\ {} \feq
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
        \\ {} \feq
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
  \end{minipage}\begin{minipage}[t]{0.50\columnwidth}
    \( \sdelta(\sunop, \epair{\svalue_0}{\svalue_1})
        \\ {} \feq
          \left\{\begin{array}{l@{~~}l}
            \makebox[2pt][l]{$\svalue_0$}
             & \quad\mbox{if $\sunop \eeq \tinst{\sfst}{\toptional}$}
            \\
            \zerowidth{\svalue_1}
             & \quad\mbox{if $\sunop \eeq \tinst{\ssnd}{\toptional}$}
          \end{array}\right.\)

    \bigskip
    \( \sdelta(\sbinop, \sint_0, \sint_1)
        \\ {} \feq
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
  \end{minipage}

}|]

@figure*[
  "fig:evaluation-meta"
  @elem{Metafunctions for boundaries and labels}
  @exact|{
  \begin{minipage}[t]{0.46\columnwidth}
    \(\frev{\sbset_0}
      \\ {} \feq
        \eset{\obnd{\sowner_1}{\stype_0}{\sowner_0}
      \,{}\mid \obnd{\sowner_0}{\stype_0}{\sowner_1} \in \sbset_0}\)

    \bigskip

    \(\begin{array}[t]{l}
      \fbsetsenders{\sbset_0} 
      \\ {} \feq
      \eset{\sowner_1 \mid \obnd{\sowner_0}{\stype_0}{\sowner_1} \in \sbset_0}
    \end{array}\)

  \end{minipage}\begin{minipage}[t]{0.54\columnwidth}
    \(\frev{\fconcat{\sowner_0}{\fconcat{\ldots}{\sowner_n}}}
      \\ {} \feq
        {\fconcat{\sowner_n}{\fconcat{\ldots}{\sowner_0}}}\)

    \bigskip
    \(\begin{array}[t]{l}
    \fvalueowners{\svalue_0}
    \\ {} \feq
      \left\{\begin{array}{ll}
        \eset{\sowner_0} \cup \fvalueowners{\svalue_1}
         & \mbox{if $\svalue_0 \eeq \obars{\svalue_1}{\sowner_0}$}
        \\
        \fvalueowners{\svalue_1}
         & \mbox{if $\svalue_0 \eeq \ehist{\sbset_0}{\svalue_1}$}
        \\
        \eset{}
         & \mbox{otherwise}
      \end{array}\right.\end{array}\)

  \end{minipage}

    \[\obbars{\sexpr_0}{\fconcat{\sowner_n}{\fconcat{\ldots}{\sowner_1}}} \eeq \sexpr_1
       \quad\sabbreveq\quad
      \sexpr_1 \eeq \obars{\cdots \obars{\sexpr_0}{\sowner_n} \cdots}{\sowner_1}\]

}|]

@Figure-ref{fig:evaluation-common} defines common aspects
 of the evaluation languages.
These include errors @${\eerr}, shapes (or, constructors) @${\stag},
 evaluation contexts, and evaluation metafunctions.

A program evaluation may signal four kinds of errors.
First, a dynamic tag error (@${\tagerrorD{}}) occurs when
 an untyped redex applies an elimination form to a mis-shaped input.
For example, the first projection of an integer signals a tag error.
Second, an invariant error (@${\tagerrorS{}}) occurs when the shape of a typed
 redex contradicts static typing; a ``tag error'' in typed code is one
 way to reach an invariant error.
One goal of type soundness is to eliminate such contradictions.
Third, a division-by-zero error (@${\divisionbyzeroerror}) may be raised by an
 application of the @${\squotient} primitive;
 a full language may contain many more partial primitives.
Lastly, a boundary error (@${\boundaryerror{\sbset}{\svalue}})
 reports a mismatch between two components.
One component, the sender, provided the enclosed value.
A second client component rejected the value.
The accompanying set of witness boundaries suggests potential sources for the fault;
 intuitively, this set should include the client--sender boundary.
As an example, the error @${\boundaryerror{\eset{\obnd{\sowner_0}{\stype_0}{\sowner_1}}}{\svalue_0}}
 says that a mismatch between value @${\svalue_0} and type @${\stype_0} prevented
 the value sent by the @${\sowner_1} component from entering the @${\sowner_0} component.
@; %% Note that a boundary error is comparable to a blame error@~citep{wf-esop-2009};
@; %%  however, a boundary error emphasizes that
@; %%  either an untyped component or a type specification may be at fault.
@; %%  Type specifications---whether written by a programmer or inferred@~citep{cc-snapl-2019}---can have bugs.

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
 of all constant functions.

The @${\sshallow} metafunction also makes reference to two value constructors
 unique to the higher-order evaluation language:
 @${(\emon{\sbnd}{\svalue})} and @${(\ehist{\sblist}{\svalue})}.
@Figure-ref{fig:evaluation-ho} formally introduces these guard and trace wrapper values.

The final components of @figure-ref{fig:evaluation-common} are the @${\delta}
 metafunctions.
These provide a standard and partial specification of the primitive operations.

@Figure-ref{fig:evaluation-meta} defines additional metafunctions for boundaries
 and ownership labels.
For boundaries, @${\srev} flips every client and sender name in
 a set of specifications.
Both @|tname| and @|aname| use this function when a value flows
 along a back-channel; if a function crosses one set @${\sbset_0} of boundaries,
 then an input to this function crosses the reversed boundaries @${\frev{\sbset_0}}
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


@subsection{Higher-Order Language, Path-Based Ownership Consistency}

@figure*[
  "fig:evaluation-ho"
  @elem{Higher-Order syntax, typing rules, and ownership consistency}
  @exact|{
  \lbl{\fbox{\syntaxho{}}}{ % ~extends Common Evaluation Syntax
    \begin{langarray}
      \sexpr & \BNFeq &
        \ldots \mid
        \eprehist{\sblist}{\sexpr}
      \\
      \svalue & \BNFeq &
        \sint \mid \snat \mid \epair{\svalue}{\svalue} \mid
        \efun{\svar}{\sexpr} \mid \efun{\tann{\svar}{\stype}}{\sexpr} \mid
        \emon{\sbnd}{\svalue} \mid
        \ehist{\sblist}{\svalue}
    \end{langarray}
  }

  \smallskip
  \lbl{\fbox{$\stypeenv \sWTfull \sexpr : \stype$} \missingrules{}}{ %~extends {$\stypeenv \sWT \sexpr : \stype$}
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

  \smallskip
  \lbl{\fbox{$\stypeenv \sWTfull \sexpr : \tdyn$} \missingrules{}}{ % ~extends {$\stypeenv \sWT \sexpr : \tdyn$}
    \begin{mathpar}
      \inferrule*{
        \stypeenv_0 \sWTfull \svalue_0 : \stype_0
      }{
        \stypeenv_0 \sWTfull \emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0} : \tdyn
      }

      \inferrule*{
        \stypeenv_0 \sWTfull \svalue_0 : \tdyn
      }{
        \stypeenv_0 \sWTfull \ehist{\sblist_0}{\svalue_0} : \tdyn
      }

      %\inferrule*{
      %  \stypeenv_0 \sWTfull \sexpr_0 : \tdyn
      %}{
      %  \stypeenv_0 \sWTfull \eprehist{\sblist_0}{\sexpr_0} : \tdyn
      %}
      %
      \inferrule*{
      }{
        \stypeenv_0 \sWTfull \eerr : \tdyn
      }

    \end{mathpar}
  }

  \lbl{\fbox{$\sownerenv; \sowner \sWLsingle \sexpr$} \missingrules{}}{\begin{mathpar}
      \inferrule*{
        \sownerenv_0; \sowner_1 \sWLsingle \svalue_0
      }{
        \sownerenv_0; \sowner_0 \sWLsingle \emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      }

      \inferrule*{
        \sownerenv_0; \sowner_0 \sWLsingle \svalue_0
      }{
        \sownerenv_0; \sowner_0 \sWLsingle \ehist{\sblist_0}{\svalue_0}
      }
  \end{mathpar}}

  %\lbl{\fbox{$\sownerenv; \sowner \sWLpath \sexpr$} \missingrules{}}{\begin{mathpar}
  %    \inferrule*{
  %      \tann{\svar_0}{\sowner_0} \in \sownerenv_0
  %    }{
  %      \sownerenv_0; \sowner_0 \sWLpath \svar_0
  %    }

  %    \inferrule*{
  %      \sownerenv_0; \sowner_1 \sWLpath \svalue_0
  %    }{
  %      \sownerenv_0; \sowner_0 \sWLpath \emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\obars{\svalue_0}{\sowner_1}}
  %    }

  %    \inferrule*{
  %      \sownerenv_0; \sowner_1 \sWLpath \sexpr_0
  %    }{
  %      \sownerenv_0; \sowner_0 \sWLpath \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\obars{\sexpr_0}{\sowner_1}}
  %    }

  %    \inferrule*{
  %      \sownerenv_0; \sowner_1 \sWLpath \sexpr_0
  %    }{
  %      \sownerenv_0; \sowner_0 \sWLpath \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\obars{\sexpr_0}{\sowner_1}}
  %    }

  %    \inferrule*{
  %      \sblist_0 = \obnd{\sowner_0}{\stype_0}{\sowner_1} \cdots \obnd{\sowner_{n-1}}{\stype_{n-1}}{\sowner_n}
  %      \\
  %      \sownerenv_0; \sowner_n \sWLpath \svalue_0
  %    }{
  %      \sownerenv_0; \sowner_0 \sWLpath \obars{\ehist{\sblist_0}{\obbars{\svalue_0}{\sowner_n \cdots \sowner_1}}}{\sowner_0}
  %    }
  %\end{mathpar}}

}|]

The higher-order evaluation language (@figure-ref{fig:evaluation-ho}) introduces the two wrapper values
 described in @sectionref{sec:design:semantic-framework}.
A guard wrapper @${(\emon{\obnd{\sowner}{\stype}{\sowner}}{\svalue})}
 represents a boundary between two components.
A trace wrapper @${(\ehist{\sblist}{\svalue})} attaches metadata to a value.

In prior work, we used the name @emph{monitor wrapper} and value constructor
  @${\mathsf{mon}}@~citep{gf-icfp-2018,gfd-oopsla-2019}.
The name \emph{guard wrapper} better matches \citet{dtf-esop-2012}, where
 @${\mathsf{mon}} constructs an expression and @${\wrapfont{G}}
 constructs a wrapper value.

Type-enforcement strategies typically use guard wrappers to constrain the
 behavior of a value.
For example, the @|cname| semantics wraps any pair that crosses a boundary
 with a guard; this wrapper validates the elements of the pair upon
 future projections.
The @|aname| semantics is an exception.
@; %% TODO explain why A is different?
Trace wrappers do not constrain behavior.
A traced value simply comes with extra information; namely, a collection of
 the boundaries that the value has previously crossed.

The higher-order typing judgments, @${\stypeenv \sWTfull \sexpr : \stoptional},
 extend the surface typing judgments with rules for wrappers and errors.
Guard wrappers may appear in both typed and untyped code; the rules
 in each case mirror those for boundary expressions.
Trace wrappers may only appear in untyped code;
 this restriction simplifies the @|aname| semantics (@figure-ref{fig:amnesic-reduction}).
A traced expression is well-formed iff the enclosed value is well-formed.
An error term is well-typed in any context.

@; %% TODO explain 'extends'
@Figure-ref{fig:evaluation-ho} also extends the single-owner consistency judgment
 to handle wrapped values.
For a guard wrapper, the outer client name must match the context
 and the enclosed value must be single-owner consistent with the inner
 sender name.
For a trace wrapper, the inner value must be single-owner consistent
 relative to the context label.
@; %%  after all, the boundaries listed in a trace are only descriptive metadata.


@subsection{First-Order Language, Heap-Based Ownership Consistency}

@figure*[
  "fig:evaluation-fo"
  @elem{First-Order syntax, typing rules, and ownership consistency}
  @exact|{
  \begin{minipage}[t]{0.50\columnwidth}
    \lbl{\fbox{\syntaxfo{}}}{ %~extends Common Evaluation Syntax
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
        \\
        \vstore & \BNFeq &
          \powerset{(\vrecord{\eloc}{\sprevalue})}
        \\
        \bstore & \BNFeq &
          \powerset{(\brecord{\eloc}{\sbset})}
        \\
        \vstoretype & \BNFeq &
          \snil \mid \fcons{\tann{\eloc}{\stoptional}}{\vstoretype}
      \end{langarray}
    }
  \end{minipage}\begin{minipage}[t]{0.50\columnwidth}
    \(\fmapref{\vstore_0}{\svalue_0}
      \\ {} \feq
      \left\{\begin{array}{l@{\quad}l}
        \sprevalue_0 & \mbox{if $\svalue_0 \in \eloc$ and $(\vrecord{\svalue_0}{\sprevalue_0}) \in \vstore_0$}
        \\
        \svalue_0 & \mbox{if $\svalue_0 \not\in \eloc$}
      \end{array}\right.\)

    \smallskip
    \(\fmapref{\bstore_0}{\svalue_0}
      \\ {} \feq
      \left\{\begin{array}{l@{\quad}l}
        \sbset_0 & \mbox{if $\svalue_0 \in \eloc$ and $(\brecord{\svalue_0}{\sbset_0}) \in \bstore_0$}
        \\
        \semptymap & \mbox{otherwise}
      \end{array}\right.\)

    \smallskip
    \(\fmapreplace{\bstore_0}{\svalue_0}{\sbset_0}
      \\ {} \feq
      \left\{\begin{array}{l@{\quad}l}
        \zerowidth{\eset{\vrecord{\svalue_0}{\sbset_0}} \cup {(\bstore_0 \setminus (\vrecord{\svalue_0}{\sbset_1}))}}
        \\
        & \mbox{if $\svalue_0 \in \eloc$ and $(\vrecord{\svalue_0}{\sbset_1}) \in \bstore_0$}
        \\
        \bstore_0 & \mbox{otherwise}
      \end{array}\right.\)

    \smallskip
    \(\fmapupdate{\bstore_0}{\svalue_0}{\sbset_0}
       {} \feq
        \fmapreplace{\bstore_0}{\svalue_0}{\bappend{\sbset_0}{\fmapref{\bstore_0}{\svalue_0}}}\)
  \end{minipage}

  \smallskip
  \lbl{\fbox{$\vstoretype; \stypeenv \sWTtag \sexpr : \stag$} \missingrules{}}{
    \begin{mathpar}
%      \inferrule*{
%        \tann{\sloc_0}{\stype_0} \in \vstoretype_0
%      }{
%        \vstoretype_0; \stypeenv_0 \sWTtag \sloc_0 : \ftagof{\stype_0}
%      }
%
%      \inferrule*{
%        \tann{\svar_0}{\stype_0} \in \stypeenv_0
%      }{
%        \vstoretype_0; \stypeenv_0 \sWTtag \svar_0 : \ftagof{\stype_0}
%      }
%
%%      \inferrule*{
%%        \vstoretype_0; \fcons{\tann{\svar_0}{\stype_0}}{\stypeenv_0} \sWTtag \sexpr_0 : \stag_0
%%      }{
%%        \vstoretype_0; \stypeenv_0 \sWTtag \efun{\tann{\svar_0}{\stype_0}}{\sexpr_0} : \kfun
%%      }
%%
%      \inferrule*{
%        \vstoretype_0; \fcons{\tann{\svar_0}{\tdyn}}{\stypeenv_0} \sWTtag \sexpr_0 : \tdyn
%      }{
%        \vstoretype_0; \stypeenv_0 \sWTtag \efun{\svar_0}{\sexpr_0} : \kfun
%      }
%
      \inferrule*{
        \vstoretype_0; \stypeenv_0 \sWTtag \sexpr_0 : \stag_0
        \\
        \sDelta(\sunop, \stag_0) \subteq \ftagof{\stype_0}
      }{
        \vstoretype_0; \stypeenv_0 \sWTtag \eunopt{\stype_0}{\sexpr_0} : \ftagof{\stype_0}
      }

%      \inferrule*{
%        \vstoretype_0; \stypeenv_0 \sWTtag \sexpr_0 : \kfun
%        \\
%        \vstoretype_0; \stypeenv_0 \sWTtag \sexpr_1 : \stag_0
%      }{
%        \vstoretype_0; \stypeenv_0 \sWTtag \eapp{\stype_0}{\sexpr_0}{\sexpr_1} : \ftagof{\stype_0}
%      }
%
      \inferrule*{
        \vstoretype_0; \stypeenv_0 \sWT \sexpr_0 : \tdyn
      }{
        \vstoretype_0; \stypeenv_0 \sWT \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sexpr_0} : \ftagof{\stype_0}
      }

      \inferrule*{
        \vstoretype_0; \stypeenv_0 \sWTtag \sexpr_0 : \tdyn
      }{
        \vstoretype_0; \stypeenv_0 \sWTtag \echecktwo{\stype_0}{\sexpr_0} : \ftagof{\stype_0}
      }

      \inferrule*{
        \vstoretype_0; \stypeenv_0 \sWTtag \sexpr_0 : \stag_0
      }{
        \vstoretype_0; \stypeenv_0 \sWTtag \echecktwo{\stype_0}{\sexpr_0} : \ftagof{\stype_0}
      }

    \end{mathpar}
  }

  \smallskip
  \lbl{\fbox{$\vstoretype; \stypeenv \sWTtag \sexpr : \tdyn$} \missingrules{}}{
    \begin{mathpar}
      \inferrule*{
        \tann{\sloc_0}{\tdyn} \in \vstoretype_0
      }{
        \vstoretype_0; \stypeenv_0 \sWTtag \sloc_0 : \tdyn
      }

      \inferrule*{
        \tann{\svar_0}{\tdyn} \in \stypeenv_0
      }{
        \vstoretype_0; \stypeenv_0 \sWTtag \svar_0 : \tdyn
      }

%      \inferrule*{
%        \vstoretype_0; \fcons{\tann{\svar_0}{\tdyn}}{\stypeenv_0} \sWTtag \sexpr_0 : \tdyn
%      }{
%        \vstoretype_0; \stypeenv_0 \sWTtag \efun{\svar_0}{\sexpr_0} : \tdyn
%      }
%
%      \inferrule*{
%        \vstoretype_0; \fcons{\tann{\svar_0}{\stype_0}}{\stypeenv_0} \sWTtag \sexpr_0 : \stag_0
%      }{
%        \vstoretype_0; \stypeenv_0 \sWTtag \efun{\tann{\svar_0}{\stype_0}}{\sexpr_0} : \tdyn
%      }
%
      \inferrule*{
        \vstoretype_0; \stypeenv_0 \sWTtag \sexpr_0 : \ftagof{\stype_0}
      }{
        \vstoretype_0; \stypeenv_0 \sWTtag \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sexpr_0} : \tdyn
      }

      \inferrule*{
        \vstoretype_0; \stypeenv_0 \sWTtag \sexpr_0 : \tdyn
      }{
        \vstoretype_0; \stypeenv_0 \sWTtag \echecktwo{\tdyn}{\sexpr_0} : \tdyn
      }

      \inferrule*{
        \vstoretype_0; \stypeenv_0 \sWTtag \sexpr_0 : \stag_0
      }{
        \vstoretype_0; \stypeenv_0 \sWTtag \echecktwo{\tdyn}{\sexpr_0} : \tdyn
      }
    \end{mathpar}
  }

%%% heap extension
%\smallskip
%\lbl{\fbox{$\vstoretype \subteq \vstoretype$} \missingrules{}}{
%  \begin{mathpar}
%    \inferrule*{
%      \vstoretype_0 \subseteq \vstoretype_1
%    }{
%      \vstoretype_0 \subteq \vstoretype_1
%    }
%  \end{mathpar}
%}

%%% heap typing
%\smallskip
%\lbl{\fbox{$\vstoretype \subteq \vstoretype$} \missingrules{}}{
%  \begin{mathpar}
%    \inferrule*{
%      \fforall{(\vrecord{\eloc_0}{\svalue_0}) \in \vstore_0}{\fforall{(\vrecord{\eloc_0}{\stoptional}) \in \vstoretype_0}{\vstoretype_0 \sWTtag \svalue_0 : \tagof{\stoptional}}}
%    }{
%      \vstoretype_0 \sWTtag \vstore_0
%    }
%  \end{mathpar}
%}

}|]

The first-order language (@figure-ref{fig:evaluation-fo})
 supports wrapper-free gradual typing.
@; %% TODO facts, not judgments
@; %Without wrappers it is far more difficult to detect and report errors;
@; % nevertheless, the first-order @|tname| semantics satisfies a form of
@; % type soundness and can provide useful hints in the event of a run-time
@; % type mismatch.
@; %
A new expression form, @${(\echecktwo{\stoptional}{\sexpr}{\eloc})},
 represents a shape check.
The intended meaning is that the given type must match the value of the
 enclosed expression.
If not, then the location @${\eloc} may be the source of the fault.
Locations are names for the pairs and functions in a program.
These names map to values in a heap (@${\vstore})
 and, more importantly, to sets of boundaries in a blame map (@${\bstore}).
@; %% TODO reword
@; %To be clear, heaps and locations are part of the model only to enable the blame maps;
@; % a blame map keeps information that would otherwise be stored in across wrappers.

Three meta-functions define heap operations:
 @${\fmapref{\cdot}{\cdot}}, @${\fmapreplace{\cdot}{\cdot}{\cdot}}, and @${\fmapupdate{\cdot}{\cdot}{\cdot}}.
The first gets an item from a finite map,
 the second replaces a blame heap entry,
 and the third extends a blame heap entry.
Because maps are sets, set union suffices to add new entries.

The first-order typing judgments ({@${\vstoretype; \stypeenv \sWTtag \sexpr : \stoptional}})
 check the top-level shape of an expression and the well-formedness of any
 subexpressions.
These judgments rely on a store typing (@${\vstoretype})
 to describe heap-allocated values;
 of course, these types must be consistent with the actual values on the heap.
Untyped functions may appear in a typed context and vice-versa---because
 there are no wrappers to enforce a separation.
Shape-check expressions are valid in typed and untyped contexts.

@; %% TODO example?


@subsection{Erasure Language}

@figure*[
  "fig:evaluation-eo"
  @elem{Erasure evaluation typing}
  @exact|{
  \lbl{\fbox{\syntaxeo{}}}{
    \begin{langarray}
      \svalue & \BNFeq &
        \sint \mid \snat \mid \epair{\svalue}{\svalue} \mid \efun{\svar}{\sexpr} \mid \efun{\tann{\svar}{\stype}}{\sexpr}
    \end{langarray}
  }

  \smallskip
  \lbl{\fbox{$\stypeenv \sWTnone \sexpr : \tdyn$} \missingrules{}}{
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

@Figure-ref{fig:evaluation-eo} defines an evaluation language for type-erased
 programs.
Expressions include error terms; the typing judgment holds
 for any expression without free variables.
Aside from the type annotations left over from the surface language, which could be
 removed with a translation step, the result is a conventional dynamically-typed language.


@section{Properties of Interest}

@emph{Type soundness} guarantees that the evaluation of a well-formed expression
 (1) cannot end in an invariant error and (2) preserves an evaluation-language
 image of the surface type.

@exact|{
\begin{definition}[$\sXproj{}$-type soundness]\label{def:ts}
  A semantics\/ $\xsym$
  satisfies\/ $\propts{\sXproj}$
  if for all\/ $\fwellformed{\sexpr_0}{\stoptional}$
  one of the following holds:
  \begin{itemize}
    \item
      $\sexpr_0 \rredX \svalue_0$ and\/ ${}\sWTX \svalue_0 : \sXproj(\stoptional)$
    \item
      $\sexpr_0 \rredX \eset{\tagerrorD{}, \divisionbyzeroerror{}} \cup \boundaryerror{\sblist}{\svalue}$
    \item
      $\fdiverge{\sexpr_0}{\rredX}$
  \end{itemize}
\end{definition}
}|

@exact{\noindent{}}Three surface-to-evaluation maps suffice for the evaluation languages:
 an identity map @${\sidproj},
 a type-shape map @${\stagproj} that extends the type-to-shape metafunction from @figure-ref{fig:evaluation-common},
 and a constant dynamic map @${\sdynproj}:

@exact|{
\smallskip
\(\hfill
  \sidproj(\stoptional) \feq \stoptional
  \hfill
  \ftagproj{\stoptional} \feq
    \left\{\begin{array}{ll}
      \tdyn & \mbox{if $\stoptional \eeq \tdyn$}
      \\
      \ftagof{\stype_0} & \mbox{if $\stoptional \eeq \stype_0$}
    \end{array}\right.
  \hfill
  {\sdynproj(\stoptional) \feq \tdyn}
  \hfill\)
\smallskip
}|

@emph{Complete monitoring} guarantees that the type on each component boundary
 monitors all interactions between client and server components.
The definition of ``all interactions'' comes from the
 path-based ownership propagation laws (@sectionref{sec:design:laws});
 the labels on a value list all partially-responsible components.
@; %% labels l0 l1 ... ln are l0=current l1...ln=past un-verified
Relative to this specification, a reduction that preserves
 single-owner consistency (@figure-ref{fig:surface-ownership})
 ensures that a value cannot enter a new component without a full type check.
 
@exact|{
\begin{definition}[complete monitoring]\label{def:cm}
  A semantics\/ $\xsym$
  satisfies\/ $\propcm{}$
  if for all\/ $\fwellformed{\obars{\sexpr_0}{\sowner_0}}{\stoptional}$
  and all results\/ $\sexpr_1$
  such that\/ $\sexpr_0 \rredX \sexpr_1$,
  the result is single-owner consistent:\/ $\sowner_0 \sWLsingle \sexpr_1$.
\end{definition}
}|

@emph{Blame soundness} and @emph{blame completeness} measure the quality of error messages
 relative to a specification of the components that handled a value during an
  evaluation.
A blame-sound semantics guarantees a subset of the true senders,
 though it may miss some or all.
@; %% TODO explain "senders" earlier?
A blame-complete semantics guarantees all the true senders,
 though it may include irrelevant information.
A sound and complete semantics reports exactly the components that sent
 the value across a partially-checked boundary.

The standard definitions for blame soundness and blame completeness
 rely on the path-based ownership propagation laws from @sectionref{sec:design:laws}.
Relative to these laws, the definitions relate the sender names in a
 set of boundaries (@figure-ref{fig:evaluation-meta}) to the true owners of
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
      $\fbsetsenders{\sbset_0} \supseteq \fvalueowners{\svalue_0}$
  \end{itemize}
\end{definition}
}|

A second useful specification extends the propagation laws to push the owners
 for each location (@${\eloc}) onto the value heap (@${\vstore}) 
@Sectionref{sec:design:tech:transient} develops this idea to characterize the blame
 guarantees of the @|tname| semantics.

Lastly, the error preorder relation allows direct behavioral comparisons.
If @${\xsym} and @${\ysym} represent two strategies for type enforcement,
 then @${\xsym \sbehaviorle \ysym} states that the @${\ysym} semantics
 reduces at least as many expressions to a value as the @${\xsym} semantics.

@exact|{
\begin{definition}[error preorder]
  $\xsym \sbehaviorle \ysym$
  iff\/ $\sexpr_0 \rredY \eerr$
  implies\/ $\sexpr_0 \rredX \eerr$
  for all well-formed expressions\/ $\sexpr_0$.
\end{definition}
}|

@exact{\noindent{}}If two semantics lie below one another on the error preorder, then they report
 type mismatches on exactly the same well-formed expressions.

@exact|{
\begin{definition}[error equivalence]
  $\xsym \sbehavioreq \ysym$
  iff\/ $\xsym \sbehaviorle \ysym \sbehaviorle \xsym$
\end{definition}
}|


@section[#:tag "sec:design:tech:natural"]{@exact{\fproperties{\nname{}}}}

@figure*[
  "fig:natural-reduction"
  @elem{@|nname| notions of reduction}
  @exact|{
  \lbl{\fbox{\nname{} Syntax}~extends \syntaxho{}}{
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

  \smallskip
  \lbl{\fbox{{$\sexpr \nredNS \sexpr$}}}{
    \begin{rrarray}
      \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \nredNS
      & \emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      \\\sidecond{if {$\fshallow{\tagof{\stype_0}}{\svalue_0}$}
                  and {$\svalue_0 \in (\efun{\svar}{\sexpr}) \cup (\emon{\sbnd}{\svalue})$}}
      \\[0.5ex]
      \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\epair{\svalue_0}{\svalue_1}}
      & \nredNS
      & \epair{\edynb{\sbnd_0}{\svalue_0}}{\edynb{\sbnd_1}{\svalue_1}}
      \\\sidecond{if {$\fshallow{\tagof{\stype_0}}{\epair{\svalue_0}{\svalue_1}}$}}
      \\\sidecond{where {$\sbnd_0 \sassign \obnd{\sowner_0}{\ftypefst{\stype_0}}{\sowner_1}$}
                  and {$\sbnd_1 \sassign \obnd{\sowner_0}{\ftypesnd{\stype_0}}{\sowner_1}$}}
      \\[0.5ex]
      \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sint_0}
      & \nredNS
      & \sint_0
      \\\sidecond{if {$\fshallow{\tagof{\stype_0}}{\sint_0}$}}
      \\[0.5ex]
      \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \nredNS
      & \boundaryerror{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      \\\sidecond{if {$\neg \fshallow{\tagof{\stype_0}}{\svalue_0}$}}
      \\[1.0ex]
      \eapp{\stype_0}{(\emon{\obnd{\sowner_0}{\stype_1}{\sowner_1}}{\svalue_0})}{\svalue_1}
      & \nredNS
      & \edynb{\sbnd_0}{(\eapp{\tdyn}{\svalue_0}{(\estab{\sbnd_1}{\svalue_1})})}
      \\\sidecond{where {$\sbnd_0 \sassign \obnd{\sowner_0}{\ftypecod{\stype_1}}{\sowner_1}$}
                  and {$\sbnd_1 \sassign \obnd{\sowner_1}{\ftypedom{\stype_1}}{\sowner_0}$}}
    \end{rrarray}
  }

  \smallskip
  \lbl{\fbox{{$\sexpr \nredND \sexpr$}}}{
    \begin{rrarray}
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \nredND
      & \emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      \\\sidecond{if $\fshallow{\stype_0}{\svalue_0}$
                  and {$\svalue_0 \in {(\efun{\tann{\svar}{\stype}}{\sexpr})} \cup {(\emon{\sbnd}{\svalue})}$}}
      \\[0.5ex]
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\epair{\svalue_0}{\svalue_1}}
      & \nredND
      & \epair{\estab{\sbnd_0}{\svalue_0}}{\estab{\sbnd_1}{\svalue_1}}
      \\\sidecond{if {$\fshallow{\tagof{\stype_0}}{\epair{\svalue_0}{\svalue_1}}$}}
      \\\sidecond{where {$\sbnd_0 \sassign \obnd{\sowner_0}{\ftypefst{\stype_0}}{\sowner_1}$}
                  and {$\sbnd_1 \sassign \obnd{\sowner_0}{\ftypesnd{\stype_0}}{\sowner_1}$}}
      \\[0.5ex]
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sint_0}
      &  \nredND
      &  \sint_0
      \\\sidecond{if \(\fshallow{\ftagof{\stype_0}}{\sint_0}\)}
      \\[0.5ex]
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      &  \nredND
      &  \tagerrorS
      \\\sidecond{if \(\neg \fshallow{\ftagof{\stype_0}}{\svalue_0}\)}
      \\[1.0ex]
      \eapp{\tdyn}{(\emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0})}{\svalue_1}
      & \nredND
      & \estab{\sbnd_0}{(\eapp{\ftypecod{\stype_0}}{\svalue_0}{(\edynb{\sbnd_1}{\svalue_1})})}
      \\\sidecond{where {$\sbnd_0 \sassign \obnd{\sowner_0}{\ftypecod{\stype_0}}{\sowner_1}$}
                  and {$\sbnd_1 \sassign \obnd{\sowner_1}{\ftypedom{\stype_0}}{\sowner_0}$}}
    \end{rrarray}
  }

  \smallskip
  \lbl{\fbox{$\sexpr \rredN \sexpr$} ${}={} \rtclosure{\nredNS, \nredND, \snreddyn, \snredsta}$}{}

}|]

@Figure-ref{fig:natural-reduction} presents the values and key reduction rules
 for the @|nname| semantics.
Conventional reductions handle primitives and unwrapped functions
  (@${\snreddyn} and @${\snredsta}, @figure-ref{fig:common-reduction}).

A successful @|nname| reduction yields either an unwrapped value or
 a guard-wrapped function.
Guards arise when a function value reaches a function-type boundary.
Thus, the possible wrapped values are drawn from the following two sets:

@exact|{
\smallskip
\(\hfill
\begin{array}[t]{lcl}
  \svaluestat & \BNFeq & \emon{\obnd{\sowner}{(\tfun{\stype}{\stype})}{\sowner}}{(\efun{\svar}{\sexpr})} \\
  & \mid & \emon{\obnd{\sowner}{(\tfun{\stype}{\stype})}{\sowner}}{\svaluedyn}
\end{array}
\hfill
\begin{array}[t]{lcl}
  \svaluedyn & \BNFeq & \emon{\obnd{\sowner}{(\tfun{\stype}{\stype})}{\sowner}}{(\efun{\tann{\svar}{\stype}}{\sexpr})} \\
  & \mid & \emon{\obnd{\sowner}{(\tfun{\stype}{\stype})}{\sowner}}{\wideas{\svaluestat}{\svaluedyn}}
\end{array}
\hfill\)
\smallskip
}|

The reduction rules focus on the @|nname| strategy for enforcing
 static types.
When a dynamically-typed value reaches a typed context (@${\sdyn}),
 @|nname| checks the shape of the value against the type.
If the type and value match, @|nname| wraps functions
 and recursively checks the elements of a pair.
Otherwise, @|nname| raises an error at the current boundary.
When a wrapped function receives an argument, @|nname|
 creates two new boundaries:
 one to protect the input to the inner, untyped function
 and one to validate the result.
@; %% TODO illustrate? or point back to picture

Reduction in dynamically-typed code (@${\nredND}) follows a dual strategy.
The rules for @${\ssta} boundaries wrap functions and recursively protect
 the contents of pairs.
The application of a wrapped function creates boundaries to validate
 the input to a typed function and to protect the result.

Unsurprisingly, this checking protocol ensures the validity of types in typed
 code and the well-formedness of expressions in untyped code.
The @|nname| approach additionally keeps boundary types honest throughout the
 execution.

@exact|{
\begin{theorem}
  \nname{} satisfies $\propts{\sidproj}$
\end{theorem}
\begin{proofsketch}
  When an untyped pair reaches a pair-type boundary, \nname{} creates
   a new pair and two new boundary expressions:

  \begin{displayrrarray}
    \edynb{\obnd{\sowner_0}{\tpair{\stype_0}{\stype_1}}{\sowner_1}}{\epair{\svalue_0}{\svalue_1}}
    & \nredNS
    & \epair{\edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}}{\edynb{\obnd{\sowner_0}{\stype_1}{\sowner_1}}{\svalue_1}}
  \end{displayrrarray}

  \noindent{}The new pair has type $\tpair{\stype_0}{\stype_1}$ by the typing rule for
   boundary expressions.
\end{proofsketch}
}|

@exact|{
\begin{theorem}
  \nname{} satisfies $\propcm{}$
\end{theorem}
\begin{proofsketch}
  Consider the labeled variant of $\ssta$ rule that wraps a function:

  \begin{displayrrarray}
    \obars{\estab{\obnd{\sowner_0}{(\tfun{\stype_0}{\stype_1})}{\sowner_1}}{\obbars{\svalue_0}{\sownerlist_2}}}{\sowner_3}
    & \nredND &
    \obars{\emon{\obnd{\sowner_0}{(\tfun{\stype_0}{\stype_1})}{\sowner_1}}{\obbars{\svalue_0}{\sownerlist_2}}}{\sowner_3}
    \\\sidecond{if $\fshallow{\ftagof{\tfun{\stype_0}{\stype_1}}}{\svalue_0}$}
  \end{displayrrarray}

  \noindent{}If the redex satisfies single-owner consistency, then the labels inside
   the boundary match the sender name ($\sownerlist_2 = \fconcat{\sowner_1}{\fconcat{\sowner_1}{\ldots}}$).
  The new guard inherits the context label.
  No other laws apply; the wrapper clearly preserves ownership consistency.
\end{proofsketch}
}|

Complete monitoring implies that the @|nname| semantics detects every mismatch
 between two components---either immediately, or as soon as a function
 computes an incorrect result.
Blame soundness and completeness are consequently easy to achieve because
 one boundary is responsible for any mismatch.
The only question is whether @|nname| correctly identifies this boundary.

@exact|{
\begin{lemma}\label{lemma:natural-blame}
  If\/ $\sexpr_0$ is well-formed
  and\/ $\sexpr_0 \rredN\!\boundaryerror{\sbset_0}{\svalue_0}$,
  then\/ $\fbsetsenders{\sbset_0}\!=\!\fvalueowners{\svalue_0}$
  and furthermore\/ $\sbset_0$ contains exactly one boundary specification.
\end{lemma}
\begin{proof}
  The \nname{} rule that detects a mismatch blames a single boundary:

  \begin{displayrrarray}
    \obars{\sexpr_0}{\sowner_0}
    & \rredN &
    \ctx[\edynb{\obnd{\sowner_1}{\stype_0}{\sowner_2}}{\svalue_0}]
    \\[0.5ex]
    & \rredN &
    \boundaryerror{\eset{\obnd{\sowner_1}{\stype_0}{\sowner_2}}}{\svalue_0}
  \end{displayrrarray}

  \noindent{}Thus $\sbset_0 = \eset{\obnd{\sowner_1}{\stype_0}{\sowner_2}}$ and $\fbsetsenders{\sbset_0} \feq \eset{\sowner_2}$.
  Complete monitoring implies $\sowner_0 \sWLsingle \ctx[\edynb{\obnd{\sowner_1}{\stype_0}{\sowner_2}}{\svalue_0}]$,
   and therefore $\fvalueowners{\svalue_0} \feq \eset{\sowner_2}$.
\end{proof}
}|

@exact|{
\begin{corollary}
  \nname{} satisfies $\propbspath$\/ and $\propbcpath$
\end{corollary}
}|


@section{Intermission: Common Higher-Order Notions of Reduction}

@figure*[
  "fig:common-reduction"
  @elem{Common notions of reduction for @|nname|, @|cname|, @|fname|, and @|aname|}
  @exact|{
  \begin{minipage}[t]{0.5\columnwidth}
    \lbl{\fbox{$\sexpr \snredsta \sexpr$}}{
      \begin{rrarray}
        \eunopt{\stype_0}{\svalue_0}
        & \snredsta
        & \tagerrorS
        \\\sidecond{if {$\svalue_0 \not\in (\emon{\obnd{\sowner}{(\tpair{\stype}{\stype})}{\sowner}}{\svalue})$}}
        \\\sidecond{and {$\sdelta(\sunop, {\svalue_0})$} is undefined}
        \\[0.5ex]
        \eunopt{\stype_0}{\svalue_0}
        & \snredsta
        & \sdelta(\sunop, {\svalue_0})
        \\\sidecond{if {$\sdelta(\sunop, {\svalue_0})$} is defined}
        \\[0.5ex]
        \ebinopt{\stype_0}{\svalue_0}{\svalue_1}
        & \snredsta
        & \tagerrorS
        \\\sidecond{if {$\sdelta(\sbinop, {\svalue_0}, {\svalue_1})$} is undefined}
        \\[0.5ex]
        \ebinopt{\stype_0}{\svalue_0}{\svalue_1}
        & \snredsta
        & \sdelta(\sbinop, {\svalue_0}, {\svalue_1})
        \\\sidecond{if {$\sdelta(\sbinop, {\svalue_0}, {\svalue_1})$} is defined}
        \\[0.5ex]
        \eapp{\stype_0}{\svalue_0}{\svalue_1}
        & \snredsta
        & \tagerrorS
        \\\sidecond{if $\svalue_0 \not\in (\efun{\tann{\svar}{\stype}}{\sexpr}) \cup {}$}
        \\\sidecond{\wideas{~}{\svalue_0 \in\in~} $(\emon{\obnd{\sowner}{(\tfun{\stype}{\stype})}{\sowner}}{\svalue})$}
        \\[0.5ex]
        \eapp{\stype_0}{\svalue_0}{\svalue_1}
        & \snredsta
        & \esubst{\sexpr_0}{\svar_0}{\svalue_1}
        \\\sidecond{if $\svalue_0 \eeq (\efun{\tann{\svar_0}{\stype_1}}{\sexpr_0})$}
      \end{rrarray}
    }

  \end{minipage}\begin{minipage}[t]{0.5\columnwidth}
    \lbl{\fbox{$\sexpr \snreddyn \sexpr$}}{
      \begin{rrarray}
        \eunopt{\tdyn}{\svalue_0}
        & \snreddyn
        & \tagerrorD
        \\\sidecond{if {$\svalue_0 \not\in (\emon{\obnd{\sowner}{(\tpair{\stype}{\stype})}{\sowner}}{\svalue})$}}
        \\\sidecond{and {$\sdelta(\sunop, {\svalue_0})$} is undefined}
        \\[0.5ex]
        \eunopt{\tdyn}{\svalue_0}
        & \snreddyn
        & \sdelta(\sunop, {\svalue_0})
        \\\sidecond{if {$\sdelta(\sunop, {\svalue_0})$} is defined}
        \\[0.5ex]
        \ebinopt{\tdyn}{\svalue_0}{\svalue_1}
        & \snreddyn
        & \tagerrorD
        \\\sidecond{if {$\sdelta(\sbinop, {\svalue_0}, {\svalue_1})$} is undefined}
        \\[0.5ex]
        \ebinopt{\tdyn}{\svalue_0}{\svalue_1}
        & \snreddyn
        & \sdelta(\sbinop, {\svalue_0}, {\svalue_1})
        \\\sidecond{if {$\sdelta(\sbinop, {\svalue_0}, {\svalue_1})$} is defined}
        \\[0.5ex]
        \eapp{\tdyn}{\svalue_0}{\svalue_1}
        & \snreddyn
        & \tagerrorD
        \\\sidecond{if $\svalue_0 \not\in (\efun{\svar}{\sexpr}) \cup {}$}
        \\\sidecond{\wideas{~}{\svalue_0 \in\in~} $(\emon{\obnd{\sowner}{(\tfun{\stype}{\stype})}{\sowner}}{\svalue})$}
        \\[0.5ex]
        \eapp{\tdyn}{\svalue_0}{\svalue_1}
        & \snreddyn
        & \esubst{\sexpr_0}{\svar_0}{\svalue_1}
        \\\sidecond{if $\svalue_0 \eeq (\efun{\svar_0}{\sexpr_0})$}
      \end{rrarray}
    }
  \end{minipage}

}|]

The above presentation of the @|nname| semantics references two notions of reduction,
 @${\snredsta} and @${\snreddyn}.
These are defined in @figure-ref{fig:common-reduction};
 they handle elimination forms for unwrapped values.
When a primitive operation receives simple values, the @${\sdelta} metafunction
 (@figure-ref{fig:evaluation-common}) determines the result.
The application of an unwrapped function proceeds by substitution.

The rules for typed code (@${\snredsta}) raise an invariant error
 (@${\tagerrorS}) if an elimination form receives invalid input.
In untyped code (@${\snreddyn}), a malformed redex leads to a tag error.


@section[#:tag "sec:design:tech:conatural"]{@exact{\fproperties{\cname{}}}}

@figure*[
  "fig:conatural-reduction"
  @elem{@|cname| notions of reduction}
  @exact|{
  \lbl{\fbox{\cname{} Syntax}~extends \syntaxho{}}{
    \begin{langarray}
      \svalue & \BNFeq &
        \sint \mid
        \snat \mid
        \epair{\svalue}{\svalue} \mid
        \efun{\svar}{\sexpr} \mid
        \efun{\tann{\svar}{\stype}}{\sexpr} \mid
        \emon{\obnd{\sowner}{\tfun{\stype}{\stype}}{\sowner}}{\svalue} \mid
        \emon{\obnd{\sowner}{\tpair{\stype}{\stype}}{\sowner}}{\svalue}
    \end{langarray}
  }

  \smallskip
  \lbl{\fbox{{$\sexpr \nredCS \sexpr$}}}{
    \begin{rrarray}
      \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \nredCS
      & \emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      \\\sidecond{if {$\fshallow{\tagof{\stype_0}}{\svalue_0}$}
                  and {$\svalue_0 \in \epair{\svalue}{\svalue} \cup (\efun{\svar}{\sexpr}) \cup (\emon{\sbnd}{\svalue})$}}
      \\[0.5ex]
      \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sint_0}
      & \nredCS
      & \sint_0
      \\\sidecond{if {$\fshallow{\tagof{\stype_0}}{\sint_0}$}}
      \\[0.5ex]
      \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \nredCS
      & \boundaryerror{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      \\\sidecond{if {$\neg \fshallow{\tagof{\stype_0}}{\svalue_0}$}}
      \\[1.0ex]
      \efst{\stype_0}{(\emon{\obnd{\sowner_0}{\stype_1}{\sowner_1}}{\svalue_0})}
      & \nredCS
      & \edynb{\sbnd_0}{(\efst{\tdyn}{\svalue_0})}
      \\\sidecond{where $\sbnd_0 \sassign \obnd{\sowner_0}{\ftypefst{\stype_1}}{\sowner_1}$}
      \\[0.5ex]
      \esnd{\stype_0}{(\emon{\obnd{\sowner_0}{\stype_1}{\sowner_1}}{\svalue_0})}
      & \nredCS
      & \edynb{\sbnd_0}{(\esnd{\tdyn}{\svalue_0})}
      \\\sidecond{where $\sbnd_0 \sassign \obnd{\sowner_0}{\ftypesnd{\stype_1}}{\sowner_1}$}
      \\[0.5ex]
      \eapp{\stype_0}{(\emon{\obnd{\sowner_0}{\stype_1}{\sowner_1}}{\svalue_0})}{\svalue_1}
      & \nredCS
      & \edynb{\sbnd_0}{(\eapp{\tdyn}{\svalue_0}{(\estab{\sbnd_1}{\svalue_1})})}
      \\\sidecond{where {$\sbnd_0 \sassign \obnd{\sowner_0}{\ftypecod{\stype_1}}{\sowner_1}$}
                  and {$\sbnd_1 \sassign \obnd{\sowner_1}{\ftypedom{\stype_1}}{\sowner_0}$}}
    \end{rrarray}
  }

  \smallskip
  \lbl{\fbox{{$\sexpr \nredCD \sexpr$}}}{
    \begin{rrarray}
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \nredCD
      & \emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      \\\sidecond{if $\fshallow{\ftagof{\stype_0}}{\svalue_0}$
                  and {$\svalue_0 \in \epair{\svalue}{\svalue} \cup {(\efun{\tann{\svar}{\stype}}{\sexpr})} \cup {(\emon{\sbnd}{\svalue})}$}}
      \\[0.5ex]
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sint_0}
      &  \nredCD
      &  \sint_0
      \\\sidecond{if \(\fshallow{\ftagof{\stype_0}}{\sint_0}\)}
      \\[0.5ex]
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      &  \nredCD
      &  \tagerrorS
      \\\sidecond{if \(\neg \fshallow{\ftagof{\stype_0}}{\svalue_0}\)}
      \\[1.0ex]
      \efst{\tdyn}{(\emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0})}
      & \nredCD
      & \estab{\sbnd_0}{(\efst{\stype_1}{\svalue_0})}
      \\\sidecond{where $\stype_1 \sassign \ftypefst{\stype_0}$
                  and $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_1}{\sowner_1}$}
      \\[0.5ex]
      \esnd{\tdyn}{(\emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0})}
      & \nredCD
      & \estab{\sbnd_0}{(\esnd{\stype_1}{\svalue_0})}
      \\\sidecond{where $\stype_1 \sassign \ftypesnd{\stype_0}$
                  and $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_1}{\sowner_1}$}
      \\[0.5ex]
      \eapp{\tdyn}{(\emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0})}{\svalue_1}
      & \nredCD
      & \estab{\sbnd_0}{(\eapp{\ftypecod{\stype_0}}{\svalue_0}{(\edynb{\sbnd_1}{\svalue_1})})}
      \\\sidecond{where {$\sbnd_0 \sassign \obnd{\sowner_0}{\ftypecod{\stype_0}}{\sowner_1}$}
                  and {$\sbnd_1 \sassign \obnd{\sowner_1}{\ftypedom{\stype_0}}{\sowner_0}$}}
    \end{rrarray}
  }

  \smallskip
  \lbl{\fbox{$\sexpr \rredC \sexpr$} ${}={} \rtclosure{\nredCS, \nredCD, \snreddyn, \snredsta}$}{}

}|]

@Figure-ref{fig:conatural-reduction} presents the @|cname| strategy.
As the name suggests, @|cname| is a lazy variant of the @|nname| approach.
Instead of eagerly validating pairs at a boundary, @|cname| creates a wrapper
 to delay element-checks until they are needed.

@; %% TODO be more precise ... pair-mon can only contain pair ... need 4 terminals
@; %\smallskip
@; %\(\hfill
@; %\begin{array}[t]{lcl}
@; %  \svaluestat & \BNFeq & \emon{\obnd{\sowner}{(\tfun{\stype}{\stype})}{\sowner}}{\efun{\svar}{\sexpr}}
@; %  \\ & \mid & \emon{\obnd{\sowner}{(\tfun{\stype}{\stype})}{\sowner}}{\svaluedyn}
@; %  \\ & \mid & \emon{\obnd{\sowner}{(\tpair{\stype}{\stype})}{\sowner}}{\svaluedyn}
@; %\end{array}
@; %\hfill
@; %\begin{array}[t]{lcl}
@; %  \svaluedyn & \BNFeq & \emon{\obnd{\sowner}{(\tfun{\stype}{\stype})}{\sowner}}{\efun{\tann{\svar}{\stype}}{\sexpr}}
@; %  \\ & \mid & \emon{\obnd{\sowner}{(\tfun{\stype}{\stype})}{\sowner}}{\wideas{\svaluestat}{\svaluedyn}}
@; %  \\ & \mid & \emon{\obnd{\sowner}{(\tpair{\stype}{\stype})}{\sowner}}{\wideas{\svaluestat}{\svaluedyn}}
@; %\end{array}
@; %\hfill\)

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
  \cname{} satisfies $\propts{\sidproj}$
\end{theorem}
\begin{proofsketch}
  Consider the typed rule that applies a wrapped function:

  \begin{displayrrarray}
    \eapp{\stype_0}{(\emon{\obnd{\sowner_0}{(\tfun{\stype_1}{\stype_2})}{\sowner_1}}{\svalue_0})}{\svalue_1}
    & \nredCS
    \\[0.5ex]\sidecond{\(\edynb{\obnd{\sowner_0}{\stype_2}{\sowner_1}}{(\eapp{\tdyn}{\svalue_0}{(\estab{\obnd{\sowner_1}{\stype_1}{\sowner_2}}{\svalue_1})})}\)}
  \end{displayrrarray}

  \noindent{}If the redex is well-typed, then $\svalue_1$ has type $\stype_1$
   and the inner static-to-dynamic boundary is well-formed.
  Similar reasoning for $\svalue_0$ shows that the application is well-formed.
  Thus the result has type $\stype_2$ which, by inversion on the redex, is a subtype of
   the target $\stype_0$.
\end{proofsketch}
}|

@exact|{
\begin{theorem}
  \cname{} satisfies $\propcm{}$
\end{theorem}
\begin{proofsketch}
  Consider the typed, labeled rule that applies a wrapped function:

  \begin{displayrrarray}
    \obars{\eapp{\stype_0}{\obbars{\emon{\obnd{\sowner_0}{(\tfun{\stype_1}{\stype_2})}{\sowner_1}}{\obars{\svalue_0}{\sowner_2}}}{\sownerlist_3}}{\svalue_1}}{\sowner_4}
    & \nredCS
    \\[0.5ex]
    \sidecond{\(\obars{\edynb{\obnd{\sowner_0}{\stype_2}{\sowner_1}}{\obars{\eapp{\tdyn}{\svalue_0}{(\estab{\obnd{\sowner_1}{\stype_1}{\sowner_0}}{\obars{\svalue_1}{\fconcat{\sowner_4}{\frev{\sownerlist_3}}}})}}{\sowner_2}}}{\fconcat{\sownerlist_3}{\sowner_4}}\)}
  \end{displayrrarray}

  \noindent{}If the redex satisfies single-owner consistency, then $\sowner_0 = \sownerlist_3 = \sowner_4$
  and $\sowner_1 = \sowner_2$.
  Hence the concatenated labels on the result collapse to the context label $\sowner_4$.
\end{proofsketch}
}|

@exact|{
\begin{theorem}
  \cname{} satisfies $\propbspath$\/ and $\propbcpath$
\end{theorem}
\begin{proofsketch}
  The same reasoning that holds for the \nname{} semantics applies here; refer to \lemmaref{lemma:natural-blame}.
\end{proofsketch}

\begin{theorem}
  $\nsym{} \sbehaviorle \csym{}$
\end{theorem}
\begin{proofsketch}
  Let $\sexpr_0$\/ import an untyped pair into a typed context.

  \begin{displayrrarray}
    \sexpr_0 = \edynb{\obnd{\sowner_0}{\tpair{\tnat}{\tnat}}{\sowner_1}}{\epair{{-2}}{2}}
  \end{displayrrarray}

  \noindent{}The first element of the pair does not match the expected type, and the
   \nname{} approach is certain to detect this mismatch.
  \cname{}, however, can only detect the mistake if the first element is accessed.
\end{proofsketch}
}|



@section[#:tag "sec:design:tech:forgetful"]{@exact{\fproperties{\fname{}}}}

@figure*[
  "fig:forgetful-reduction"
  @elem{@|fname| notions of reduction}
  @exact|{
  \lbl{\fbox{\fname{} Syntax}~extends \syntaxho{}}{
    \begin{langarray}
      \svalue & \BNFeq &
        \sint \mid
        \snat \mid
        \epair{\svalue}{\svalue} \mid
        \efun{\svar}{\sexpr} \mid
        \efun{\tann{\svar}{\stype}}{\sexpr} \mid
        \emon{\obnd{\sowner}{\tfun{\stype}{\stype}}{\sowner}}{\svalue} \mid
        \emon{\obnd{\sowner}{\tpair{\stype}{\stype}}{\sowner}}{\svalue}
    \end{langarray}
  }

  \smallskip
  \lbl{\fbox{$\sexpr \nredFS \sexpr$}}{
    \begin{rrarray}
      \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \nredFS
      & \emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      \\\sidecond{if $\fshallow{\tagof{\stype_0}}{\svalue_0}$
                  and $\svalue_0 \in {\epair{\svalue}{\svalue}} \cup
                                     (\efun{\svar}{\sexpr}) \cup
                                     (\emon{\sbnd}{\svalue})$}
      \\[0.5ex]
      \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \nredFS
      & \sint_0
      \\\sidecond{if $\fshallow{\tagof{\stype_0}}{\svalue_0}$}
      \\[0.5ex]
      \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \nredFS
      & \boundaryerror{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      \\\sidecond{if $\neg\fshallow{\tagof{\stype_0}}{\svalue_0}$}
      \\[1.0ex]
      \efst{\stype_0}{(\emon{\obnd{\sowner_0}{\stype_1}{\sowner_1}}{\svalue_0})}
      & \nredFS
      & \edynb{\sbnd_0}{(\efst{\tdyn}{\svalue_0})}
      \\\sidecond{where $\sbnd_0 \sassign \obnd{\sowner_0}{\ftypefst{\stype_1}}{\sowner_1}$}
      \\[0.5ex]
      \esnd{\stype_0}{(\emon{\obnd{\sowner_0}{\stype_1}{\sowner_1}}{\svalue_0})}
      & \nredFS
      & \edynb{\sbnd_0}{(\esnd{\tdyn}{\svalue_0})}
      \\\sidecond{where $\sbnd_0 \sassign \obnd{\sowner_0}{\ftypesnd{\stype_1}}{\sowner_1}$}
      \\[0.5ex]
      \eapp{\stype_0}{(\emon{\obnd{\sowner_0}{\stype_1}{\sowner_1}}{\svalue_0})}{\svalue_1}
      & \nredFS
      & \edynb{\sbnd_0}{(\eapp{\tdyn}{\svalue_0}{(\estab{\sbnd_1}{\svalue_1})})}
      \\\sidecond{where $\sbnd_0 \sassign \obnd{\sowner_0}{\ftypecod{\stype_1}}{\sowner_1}$
                  and $\sbnd_1 \sassign \obnd{\sowner_1}{\ftypedom{\stype_1}}{\sowner_0}$}
    \end{rrarray}
  }

  \smallskip
  \lbl{\fbox{$\sexpr \nredFD \sexpr$}}{
    \begin{rrarray}
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \nredFD
      & \emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      \\\sidecond{if $\fshallow{\tagof{\stype_0}}{\svalue_0}$
                  and $\svalue_0 \in {\epair{\svalue}{\svalue}} \cup {(\efun{\tann{\svar}{{\stype}}}{\sexpr})}$}
      \\[0.5ex]
      \estab{\sbnd_0}{(\emon{\sbnd_1}{\svalue_0})}
      & \nredFD
      & \svalue_0
      \\\sidecond{if $\sbnd_0 \eeq \obnd{\sowner_0}{\stype_0}{\sowner_1}$
                  and $\fshallow{\tagof{\stype_0}}{\svalue_0}$}
      \\\sidecond{and $\svalue_0 \in {\epair{\svalue}{\svalue}} \cup
                                     (\efun{\svar}{\sexpr}) \cup
                                     (\emon{\sbnd}{\epair{\svalue}{\svalue}}) \cup
                                     (\emon{\sbnd}{(\efun{\tann{\svar}{\stype}}{\sexpr})})$}
      \\[0.5ex]
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sint_0}
      & \nredFD
      & \sint_0
      \\\sidecond{if $\fshallow{\tagof{\stype_0}}{\sint_0}$}
      \\[0.5ex]
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \nredFD
      & \tagerrorS
      \\\sidecond{if $\neg\fshallow{\tagof{\stype_0}}{\svalue_0}$}
      \\[1.0ex]
      \efst{\tdyn}{(\emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0})}
      & \nredFD
      & \estab{\sbnd_0}{(\efst{\stype_1}{\svalue_0})}
      \\\sidecond{where $\stype_1 \sassign \ftypefst{\stype_0}$
                  and $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_1}{\sowner_1}$}
      \\[0.5ex]
      \esnd{\tdyn}{(\emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0})}
      & \nredFD
      & \estab{\sbnd_0}{(\esnd{\stype_1}{\svalue_0})}
      \\\sidecond{where $\stype_1 \sassign \ftypesnd{\stype_0}$
                  and $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_1}{\sowner_1}$}
      \\[0.5ex]
      \eapp{\tdyn}{(\emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0})}{\svalue_1}
      & \nredFD
      & \estab{\sbnd_0}{(\eapp{\stype_2}{\svalue_0}{(\edynb{\sbnd_1}{\svalue_1})})}
      \\\sidecond{where $\stype_2 \sassign \ftypecod{\stype_0}$
                  and $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_2}{\sowner_1}$
                  and $\sbnd_1 \sassign \obnd{\sowner_1}{\ftypedom{\stype_0}}{\sowner_0}$}
    \end{rrarray}
  }

  \smallskip
  \lbl{\fbox{$\sexpr \rredF \sexpr$} ${}={} \rtclosure{\nredFS, \nredFD, \snreddyn, \snredsta}$}{}

}|]

The @|fname| semantics (@figure-ref{fig:forgetful-reduction})
 creates wrappers to enforce pair and function types,
 but strictly limits the number of wrappers on any one value.
An untyped value acquires at most one wrapper.
A typed value acquires at most two wrappers: one to protect itself from
 inputs, and a second to reflect the expectations of its current client.
@; %% TODO explain 'reflect'
@;  %% TODO introduce grammar

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
 any guarded value that flows to untyped code.
An untyped-to-typed boundary always makes a new wrapper, but these wrappers do not
 accumulate because a value cannot enter typed code twice in a row; a
 newly-wrapped value must exit typed code before it can gain another wrapper.

Removing outer wrappers does not affect the type soundness of untyped code;
 all well-formed values match @${\tdyn}, with or without wrappers.
Type soundness for typed code is guaranteed by the temporary outer wrappers.
Complete monitoring is lost, however, because removing a wrapper does
 not remove any labels.
Similarly, @|fname| lies above @|cname| and @|nname| in the error preorder.

When a type mismatch occurs, @|fname| blames one boundary.
Though sound, this one boundary is generally not enough information to
 diagnose the fault.
So, @|fname| fails to satisfy blame completeness.

@exact|{
\begin{theorem}\label{thm:F-TS}
  \fname{} satisfies $\propts{\sidproj}$
\end{theorem}
\begin{proofsketch}
  Suppose that a pair $\svalue_0$ with static type $\stype_0 \eeq \tpair{\tint}{\tint}$ crosses
   two boundaries and re-enters typed code at a different type.

  \begin{displayrrarray}
    \edynb{\obnd{\sowner_0}{(\tpair{\tnat}{\tnat})}{\sowner_1}}{(\estab{\obnd{\sowner_1}{\stype_0}{\sowner_2}}{\svalue_0})}
    & \rredF
    \\[0.5ex]\sidecond{\(\emon{\obnd{\sowner_0}{(\tpair{\tnat}{\tnat})}{\sowner_1}}{(\emon{\obnd{\sowner_1}{\stype_0}{\sowner_2}}{\svalue_0})}\)}
  \end{displayrrarray}

  \noindent{}No matter  value $\svalue_0$ is, the result is well-typed because
   the inner wrapper is well-formed and the context trusts the outer wrapper.

  If this double-wrapped value---call it $\svalue_2$---crosses another boundary,
   it loses the outer wrapper.
  The result is still well-formed.

  \begin{displayrrarray}
    \estab{\obnd{\sowner_3}{(\tpair{\tnat}{\tnat})}{\sowner_0}}{\svalue_2}
    & \rredF
    \\[0.5ex]\sidecond{\(\emon{\obnd{\sowner_1}{\stype_0}{\sowner_2}}{\svalue_0}\)}
  \end{displayrrarray}

  If this wrapped pair re-enters typed code, then it gains a wrapper
   to document the new context's assumptions:

  \begin{displayrrarray}
    \edynb{\obnd{\sowner_4}{(\tpair{\stype_1}{\stype_2})}{\sowner_3}}{(\emon{\obnd{\sowner_1}{\stype_0}{\sowner_2}}{\svalue_0})}
    & \rredF
    \\[0.5ex]\sidecond{\(\emon{\obnd{\sowner_4}{(\tpair{\stype_1}{\stype_2})}{\sowner_3}}{(\emon{\obnd{\sowner_1}{\stype_0}{\sowner_2}}{\svalue_0})}\)}
  \end{displayrrarray}

  \noindent{}Future round-trips preserve type soundness in a similar manner.
\end{proofsketch}
}|

@exact|{
\begin{theorem}\label{thm:F-CM}
  \fname{} does not satisfy $\propcm{}$
\end{theorem}
\begin{proof}
  Consider the lifted variant of the $\ssta$ rule that removes an outer guard wrapper:

  \begin{displayrrarray}
    \obars{\estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\obbars{\emon{\sbnd_1}{\svalue_0}}{\sownerlist_2}}}{\sowner_3}
    & \nredFD &
    \obbars{\svalue_0}{\fconcat{\sownerlist_2}{\sowner_3}}
    \\\sidecond{if $\fshallow{\tagof{\stype_0}}{(\emon{\sbnd_1}{\svalue_0})}$}
  \end{displayrrarray}

  \noindent{}Suppose $\sowner_0 \neq \sowner_1$.
  If the redex satisfies single-owner consistency, then $\sownerlist_2$ contains
   $\sowner_1$ and $\sowner_3 = \sowner_0$.
  Thus the rule creates a value with two distinct labels.
\end{proof}
}|

@exact|{
\begin{theorem}
  \fname{} satisfies\/ $\propbspath$
\end{theorem}
\begin{proof}
  The \fname{} rule that raises a boundary error blames only the current
   boundary:

  \begin{displayrrarray}
    \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
    & \nredFS
    & \boundaryerror{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
    \\\sidecond{if $\neg\fshallow{\tagof{\stype_0}}{\svalue_0}$}
  \end{displayrrarray}

  \noindent{}Component $\sowner_1$ clearly sent the mis-matched value.
  Technically, propagation laws~ @exact{\ref{law:pos}} and @exact{\ref{law:cross}}
   ensure that $\sowner_1 \in \fvalueowners{\svalue_0}$.
\end{proof}
}|

@exact|{
\begin{theorem}
  \fname{} does not satisfy\/ $\propbcpath$
\end{theorem}
\begin{proof}
  The proof of @exact{\theoremref{thm:F-CM}} shows how a function or pair value can acquire two labels.
  Suppose $\obbars{\efun{\svar_0}{\svar_0}}{\fconcat{\sowner_0}{\sowner_1}}$ is one such value,
   and that it reaches an incompatible boundary:

  \begin{displayrrarray}
    \edynb{\obnd{\sowner_2}{\tint}{\sowner_1}}{\obbars{\efun{\svar_0}{\svar_0}}{\fconcat{\sowner_0}{\sowner_1}}}
    & \nredFS
    & \boundaryerror{\obnd{\sowner_2}{\tint}{\sowner_1}}{\svalue_0}
  \end{displayrrarray}

  \noindent{}The error does not include sender $\sowner_0$.
\end{proof}
}|

@exact|{
\begin{theorem}
  $\csym{} \sbehaviorle \fsym{}$
\end{theorem}
\begin{proofsketch}
  If an untyped value travels to typed code and back again, \fname{} unwraps it.
  By contrast, \cname{} keeps the wrapper to enforce the boundary type.

  Let:

  \begin{displayrrarray}
    \sexpr_0 = \estab{\sbnd_0}{(\edynb{\obnd{\sowner_0}{(\tfun{\tnat}{\tnat})}{\sowner_1}}{(\efun{\svar_0}{\svar_0})})}
    \\[0.5ex]
    \sexpr_1 = \eapp{\tdyn}{\sexpr_0}{\epair{2}{8}}
  \end{displayrrarray}

  \noindent{}Then $\sexpr_1 \rredF {\epair{2}{8}}$ and
    $\sexpr_1 \rredC \sboundaryerror\,(\ldots)$.
\end{proofsketch}
}|


@section[#:tag "sec:design:tech:transient"]{@exact{\fproperties{\tname{}}}}

@figure*[
  "fig:transient-reduction"
  @elem{@|tname| notions of reduction}
  @exact|{
  \lbl{\fbox{\tname{} Syntax}~extends \syntaxfo}{
    \begin{langarray}
      \svalue & \BNFeq &
        \sint \mid \snat \mid \eloc
    \end{langarray}
  }

  \lbl{\fbox{$\conf{\sexpr}{\vstore}{\bstore} \nredTX \conf{\sexpr}{\vstore}{\bstore}$} \missingrules{}}{
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
      & \conf{\boundaryerror{\bappend{\fmapref{\bstore_0}{\svalue_0}}{\fmapref{\bstore_0}{\eloc_0}}}{\svalue_0}}{\vstore_0}{\bstore_0}
      \\\sidecond{if \(\neg \fshallow{\ftagof{\stype_0}}{\fmapref{\vstore_0}{\svalue_0}}\)}
      \\[1.0ex]

      \conf{(\eunopt{\stoptional}{\eloc_0})}{\vstore_0}{\bstore_0}
      &  \nredTX
      &  \conf{(\echecktwo{\stoptional}{\sdelta(\sunop, \vstore_0(\eloc_0))}{\eloc_0})}{\vstore_0}{\bstore_0}
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
      & \conf{\boundaryerror{\frev{\fmapref{\bstore_0}{\eloc_0}}}{\svalue_0}}{\vstore_0}{\bstore_0}
      \\\sidecond{if $\fmapref{\vstore_{0}}{\eloc_0}=\efun{\tann{\svar_0}{{\stype_0}}}{\sexpr_0}$
                  and $\neg \fshallow{\ftagof{\stype_0}}{\fmapref{\vstore_0}{\svalue_0}}$}
      \\[1.0ex]

      \conf{\sprevalue_0}{\vstore_0}{\bstore_0}
      &  \nredTX
      &  \conf{\eloc_0}{(\eset{\vrecord{\eloc_0}{\sprevalue_0}} \cup \vstore_0)}{(\eset{\brecord{\eloc_0}{\semptymap}} \cup {\bstore_0})}
      \\\sidecond{where $\ffresh{\eloc_0}{\vstore_0\mbox{ and }\bstore_0}$}

%      \conf{(\eunopt{\stype_0}{\svalue_0})}{\vstore_0}{\bstore_0}
%      &  \nredTX
%      &  \conf{\tagerrorS}{\vstore_0}{\bstore_0}
%      \\\sidecond{if $\sdelta(\sunop, {\vstore_0(\svalue_0)})$ is undefined}
%      \\[0.5ex]
%      \conf{(\ebinopt{\stype_0}{\svalue_0}{\svalue_1})}{\vstore_0}{\bstore_0}
%      &  \nredTX
%      &  \conf{\tagerrorS}{\vstore_0}{\bstore_0}
%      \\\sidecond{if \(\sdelta(\sbinop, \svalue_0, \svalue_1)\) is undefined}
%      \\[0.5ex]
%      \conf{(\eapp{\stype_0}{\svalue_0}{\svalue_1})}{\vstore_0}{\bstore_0}
%      &  \nredTX
%      & \conf{\tagerrorS}{\vstore_0}{\bstore_0}
%      \\\sidecond{if \(\fmapref{\vstore_{0}}{\svalue_0} \not\in (\efun{\svar}{\sexpr}) \cup (\efun{\tann{\svar}{\stype}}{\sexpr})\)}
%      \\[1.0ex]
%      \conf{(\eunopt{\tdyn}{\svalue_0})}{\vstore_0}{\bstore_0}
%      &  \nredTX
%      &  \conf{\tagerrorD}{\vstore_0}{\bstore_0}
%      \\\sidecond{if $\sdelta(\sunop, {\vstore_0(\svalue_0)})$ is undefined}
%      \\[0.5ex]
%      \conf{(\ebinopt{\tdyn}{\svalue_0}{\svalue_1})}{\vstore_0}{\bstore_0}
%      &  \nredTX
%      &  \conf{\tagerrorD}{\vstore_0}{\bstore_0}
%      \\\sidecond{if \(\sdelta(\sbinop, \svalue_0, \svalue_1)\) is undefined}
%      \\[0.5ex]
%      \conf{(\eapp{\tdyn}{\svalue_0}{\svalue_1})}{\vstore_0}{\bstore_0}
%      &  \nredTX
%      & \conf{\tagerrorD}{\vstore_0}{\bstore_0}
%      \\\sidecond{if \(\fmapref{\vstore_{0}}{\svalue_0} \not\in (\efun{\svar}{\sexpr}) \cup (\efun{\tann{\svar}{\stype}}{\sexpr})\)}
    \end{rrarray}
  }

  \smallskip
  \lbl{\fbox{$\sexpr; \vstore; \bstore \rredT \sexpr; \vstore; \bstore$} ${}={} \rastar_{\tsym}$}{
    where $\conf{\ctx[\sexpr_0]}{\vstore_0}{\bstore_0} \mathrel{\tsym} \conf{\ctx[\sexpr_1]}{\vstore_1}{\bstore_1}$
    iff $\conf{\sexpr_0}{\vstore_0}{\bstore_0} \nredTX \conf{\sexpr_1}{\vstore_1}{\bstore_1}$
  }

}|]


The @|tname| semantics in @figure-ref{fig:transient-reduction} builds on the first-order evaluation language (@figure-ref{fig:evaluation-fo});
 it stores pairs and functions on a heap
 as indicated by the syntax of @figure-ref{fig:evaluation-fo},
 and aims to enforce type constructors (@${\stag}, or @${\tagof{\stype}})
 through shape checks.
For every pre-value @${\sprevalue} stored on a heap @${\vstore}, there is a
 corresponding entry in a blame map @${\bstore} that points to a set of boundaries.
The blame map provides information if
 a mismatch occurs, following Reticulated Python@~citep{vss-popl-2017,v-thesis-2019}.

Unlike for the higher-order-checking semantics, there is significant overlap between the
 @|tname| rules for typed and untyped redexes.
Thus @figure-ref{fig:transient-reduction} presents one notion of reduction.

The first group of rules in @figure-ref{fig:transient-reduction} handle boundary
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

The second group of reduction rules handle primitives and application.
Pair projections and function applications must be followed by a check
 in typed contexts to enforce the type annotation at the elimination form.
@; %% TODO illustrate ... move app-annotation onto the check-expression, so we don't forget
@; %%    % The motivation for @${\scheck} expressions is to validate the result computed by a function.
@; %%    % If every function returned an immediate value, then a typed \tname{}
@; %%    %  context could check the result against the expected type and raise
@; %%    %  a blame error that incorporates the function's blame map entry.
@; %%    % But function application generally yields an expression, so both
@; %%    %  the expected type and the function's address need to stick around until
@; %%    %  that expression reduces to a value.
@; %%    % %% TODO further explain with typed->typed application? Don't really need to because blame settles it... we could say its needed even without blame
@; %%    % %% TODO (check U ) is different, only for type soundness as a boundary, and really only needed for function application
In untyped contexts, a check for the dynamic type embeds a possibly-typed subexpression.
@; %% TODO awkward
The binary operations are not elimination forms, so they are not followed by
 a check.
Applications of typed functions additionally check the input value against
 the function's domain type.
If successful, the blame map records the check.
Otherwise, @|tname| reports the boundaries associated with the function@~citep{vss-popl-2017}.

Observe that applications of untyped functions in untyped code do not update the
 blame map.
This enables an implementation that inserts all checks by rewriting typed
 code at compile-time, leaving untyped code as-is.
Protected typed code can then interact with any untyped libraries.
@; %% higher-order code gets the same benefit for untyped code that's aware of guards

@; % The last rule creates a new address for a pre-value and initializes heap
@; %  and blame map entries.

Not shown in @figure-ref{fig:transient-reduction} are rules for elimination
 forms that halt the program.
When @${\sdelta} is undefined or when a non-function is applied, the result
 is either an invariant error or a tag error depending on the context.
@; % For example, $\conf{(\eapp{\tint}{2}{3})}{\semptymap}{\semptymap} \nredTX \conf{\tagerrorS}{\semptymap}{\semptymap}$
@; %  because the annotation shows that this redex is in a typed context.

@|tname| shape checks do not guarantee full type soundness,
 complete monitoring, or the standard blame soundness and completeness.
They do, however, preserve the top-level shape of all values in typed code.
Furthermore, @|tname| satisfies a heap-based notion of blame soundness.
Blame completeness fails because @|tname| does not update the blame map when an
 untyped function is applied in an untyped context.

@; %% then the $\sWLheap$ invariant (@figure-ref{fig:evaluation-fo}) suffices to
@; %% prove that \tname{} is blame sound for @exact{\definitionref{def:blame-heap}}.

@exact|{
\begin{theorem}
  \tname{} does not satisfy $\propts{\sidproj}$
\end{theorem}
\begin{proofsketch}
  Let $\sexpr_0 = \edynb{\obnd{\sowner_0}{(\tfun{\tnat}{\tnat})}{\sowner_1}}{(\efun{\svar_0}{{-4}})}$.
  \begin{itemize}
    \item $\sWT \sexpr_0 : \tfun{\tnat}{\tnat}~$ in the surface language, but
    \item $\conf{\sexpr_0}{\semptymap}{\semptymap} \rredT \conf{\eloc_0}{\vstore_0}{\bstore_0}$, where $\fmapref{\vstore_0}{\eloc_0} = (\efun{\svar_0}{{-4}})$
  \end{itemize}
  and $\not\sWTfull (\efun{\svar_0}{{-4}}) : \tfun{\tnat}{\tnat}$.
\end{proofsketch}
}|

@exact|{
\begin{theorem}
  \tname{} satisfies $\propts{\stagproj}$
\end{theorem}
\begin{proofsketch}
  Suppose that typed code applies an untyped function, and that this function
   eventually computes an ill-typed result:

  \begin{displayrrarray}
    \conf{(\eapp{\tint}{\eloc_0}{4})}{\vstore_0}{\bstore_0}
    & \nredTX
    & \conf{(\echecktwo{\tint}{{\epair{4}{\esum{\tdyn}{4}{1}}}}{\eloc_0})}{\vstore_0}{\bstore_1}
    \\\sidecond{if \(\fmapref{\vstore_{0}}{\eloc_0}=\efun{\svar_0}{\epair{\svar_0}{\esum{\tdyn}{\svar_0}{1}}}\)}
    \\\sidecond{and $\bstore_1 = \fmapupdate{\bstore_0}{\svalue_0}{\frev{\fmapref{\bstore_0}{\eloc_0}}}$}
  \end{displayrrarray}

  \noindent{}The new check expression guards the context from the function body
   and ensures shape-correctness.
\end{proofsketch}
}|

@exact|{
\begin{theorem}
  \tname{} does not satisfy $\propcm{}$
\end{theorem}
\begin{proof}
  A structured value can cross any boundary with a matching shape, regardless
   of the deeper type structure.
  For pairs this results in a value with more than one label:

  \begin{displayrrarray}
    \conf{\obars{\edynb{\obnd{\sowner_0}{\tpair{\stype_0}{\stype_1}}{\sowner_1}}{\obbars{\epair{\svalue_0}{\svalue_1}}{\sownerlist_2}}}{\sowner_3}}{\vstore_0}{\bstore_0}
    & \nredTX &
    \conf{\obbars{\eloc_0}{\fconcat{\sownerlist_2}{\sowner_3}}}{\vstore_1}{\bstore_1}
  \end{displayrrarray}

\end{proof}
}|

@exact|{
\begin{theorem}\label{thm:T-BS}
  \tname{} does not satisfy $\propbspath$
\end{theorem}
\begin{proof}
  %If one component defines a function and exports it to two different clients,
  % then the blame information for each client is merged on the global blame map.
  Let component $\sowner_0$ define a function $f_0$ and export it to
   components $\sowner_1$ and $\sowner_2$.
  If component $\sowner_2$ triggers a type mismatch, then component $\sowner_1$
   gets blamed even though there is no direct channel from $\sowner_1$ to $\sowner_2$.

  The following term expresses the scenario above, using a let-expression
   to abbreviate untyped function application:

   {\newcommand{\theexampleint}{5}
    \newcommand{\theexampletype}{(\tfun{\tint}{\tint})}
    \begin{displayrrarray}
      (\eletdecl{f_0}{(\efun{\svar_0}{\epair{\svar_0}{\svar_0}})}
      \\
        ~\eletdecl{f_1}{(\estab{\obnd{\sowner_0}{\theexampletype}{\sowner_1}}{\obars{\edynb{\obnd{\sowner_1}{\theexampletype}{\sowner_0}}{\obars{f_0}{\sowner_0}}}{\sowner_1}})}
      \\
        ~\conf{\estab{\obnd{\sowner_0}{\tint}{\sowner_2}}{\obars{\eapp{\tint}{(\edynb{\obnd{\sowner_2}{\theexampletype}{\sowner_0}}{\obars{f_0}{\sowner_0}})}{\theexampleint}}{\sowner_2}})^{\raisedsowner}}{\emptyset}{\emptyset}
    \end{displayrrarray}}

  \noindent{}Reduction ends in a boundary error that blames all three components.
\end{proof}
}|

@exact|{
\begin{theorem}\label{thm:T-BC}
  \tname{} does not satisfy $\propbcpath$.
\end{theorem}
\begin{proof}
  The rule for untyped function application does not update the blame map:

  \begin{displayrrarray}
    \conf{(\eapp{\tdyn}{\eloc_0}{\svalue_0})}{\vstore_0}{\bstore_0}
    &  \nredTX
    & \conf{(\esubst{\sexpr_0}{\svar_0}{\svalue_0})}{\vstore_0}{\bstore_0}
    \\\sidecond{if \(\fmapref{\vstore_{0}}{\eloc_0}=\efun{\svar_0}{\sexpr_0}\)}
  \end{displayrrarray}

  \noindent{}On one hand, this missing update supports an implementation
   of \tname{} that rewrites typed code and leaves untyped code as-is.
  On the other hand, this rule leaves gaps in the blame map when the untyped
   function has crossed a few boundaries.
  The following term illustrates the problem by using an untyped identity
   function $f_1$ to coerce the type of another function ($f_0$).
  After the coercion, an application leads to type mismatch.

  {\newcommand{\theexamplefun}{\efun{\svar_0}{\svar_0}}
  \newcommand{\typea}{(\tfun{\tint}{\tint})}
  \newcommand{\typeb}{(\tfun{\tint}{\tpair{\tint}{\tint}})}
  \newcommand{\typeatxt}{\stype_0}
  \newcommand{\typebtxt}{\stype_1}
  \newcommand{\deepfowners}{\fconcat{\sowner_2}{\fconcat{\sowner_1}{\fconcat{\sowner_0}{\fconcat{\sowner_3}{\fconcat{\sowner_4}{\fconcat{\sowner_3}{\fconcat{\sowner_0}{\sowner_5}}}}}}}}
  \begin{displayrrarray}
    (\eletdecl{f_0}{\estab{\obnd{\sowner_0}{\typeatxt}{\sowner_1}}{(\edynb{\obnd{\sowner_1}{\typeatxt}{\sowner_2}}{(\theexamplefun)})}}
    \\[0.7ex]
    ~\eletdecl{f_1}{\estab{\obnd{\sowner_0}{(\tfun{\typeatxt}{\typebtxt})}{\sowner_3}}{(\edynb{\obnd{\sowner_3}{(\tfun{\typeatxt}{\typebtxt})}{\sowner_4}}{(\efun{\svar_1}{\svar_1})})}}
    \\[0.7ex]
    ~\estab{\obnd{\sowner_0}{(\tpair{\tint}{\tint})}{\sowner_5}}{}
    \\[0.5ex]\qquad\conf{\zeroheight{\obars{\eapp{\tpair{\tint}{\tint}}{(\edynb{\obnd{\sowner_5}{\typebtxt}{\sowner_0}}{\obars{\eapp{\tdyn}{f_1}{f_0}}{\sowner_0}})}{42}}{\sowner_5})^{\raisedsowner}}}{\semptymap}{\semptymap}
  \end{displayrrarray}}

  \noindent{}Reduction ends in a boundary error that does not report the crucial labels $\sowner_3$ and $\sowner_4$.
\end{proof}
}|

Weakening the specification for ownership labels enables a positive statement
 about the @|tname| blame strategy; namely, if @|tname| blames an address then
 it reports boundaries that are relevant to the address.
To arrive at weakened @emph{heap-based propagation laws}, the path-based
 laws from @sectionref{sec:design:laws} gain the following rule:

@exact|{
\begin{enumerate}
  \item[8.] \label{law:heap}
    If an address gains a label, then so does the associated pre-value on the heap.
   \subitem
     $\conf{\sfst~{\obars{\eloc_0}{\sowner_0}}}{\vstore_0}{\bstore_0}
      \rrarrow
      \conf{\obars{\eloc_1}{\sowner_0}}{\fmapreplace{\vstore_0}{\eloc_1}{\obars{\fmapref{\vstore_0}{\eloc_1}}{\sowner_0}}}{\bstore_0}$
   \subitem
     {\qquad\emph{where} $\fmapref{\vstore_0}{\eloc_0} = \obars{\epair{\eloc_1}{\eloc_2}}{\sowner_0}$}
   \subitem
     \Lawref{law:pos} adds the pair label to the element, which propogates to the heap.
\end{enumerate}
}|

The heap-based laws motivate weakened definitions of blame soundness and blame completeness
 that incorporate the labels on a heap value.

@exact|{
\begin{definition}[heap-based blame soundness and blame completeness]\label{def:blame-heap}
  For all well-formed\/ $\sexpr_0$
  such that\/ $\sexpr_0; \semptymap \rredX \boundaryerror{\sbset_0}{\svalue_0}; \vstore_0$:
  \begin{itemize}
    \itemsep0.1ex
    \item
      $\xsym$ satisfies $\propbsheap$ 
      iff
      $\fbsetsenders{\sbset_0} \subseteq \fvalueowners{\svalue_0} \cup \fvalueowners{\fmapref{\vstore_0}{\svalue_0}}$
    \item
      $\xsym$ satisfies $\propbcheap$
      iff
      $\fbsetsenders{\sbset_0} \supseteq \fvalueowners{\svalue_0} \cup \fvalueowners{\fmapref{\vstore_0}{\svalue_0}}$
  \end{itemize}
\end{definition}
}|

@exact|{
\begin{theorem}\leavevmode
  \begin{itemize}
    \item
      \tname{} satisfies $\propbsheap$
    \item
      \tname{} does not satisfy $\propbcheap$
  \end{itemize}
\end{theorem}
\begin{proofsketch}
  Under the weakened specification, the example term from @exact{\theoremref{thm:T-BS}}
   raises a sound error that blames only components that handled the relevant address.
  The full proof depends on progress and preservation lemmas for a
   $\sWLheap$ judgment, which ensures that the blame map records a subset of
   the true owners for each heap-allocated value.

   %% 2020-02-01 : these rules CAN fit side-by-side, but that leads to a fortuitous error:
   %% > ! LaTeX Error: Float(s) lost.
   %% > See the LaTeX manual or LaTeX Companion for explanation.
   %% > Type  H <return>  for immediate help.
   %% > l.2566 \end{figure}
   %% > You've lost some text.  Try typing  <return>  to proceed.
   %% > ! LaTeX Error: This may be a LaTeX bug.
   %% > See the LaTeX manual or LaTeX Companion for explanation.
   %% > Type  H <return>  for immediate help.
   %% > l.2566 \end{figure}
   %% > Call for help
   \smallskip
   \hfill\begin{minipage}{0.80\columnwidth}
     \lbl{\fbox{$\sownerenv; \sowner \sWLheap \conf{\sexpr}{\vstore}{\bstore}$}\ \missingrules{}}{\begin{mathpar}
     \inferrule*{
       \sowner_0 \in \sownerlist_0
       \\\\
       \fbsetsenders{\fmapref{\bstore_0}{\eloc_0}}
        \subseteq
        \fvalueowners{\fmapref{\vstore_0}{\eloc_0}}
     }{
       \sownerenv_0; \sowner_0 \sWLheap \conf{\obbars{\eloc_0}{\sownerlist_0}}{\vstore_0}{\bstore_0}
     }

     \inferrule*{
       \sowner_0 \in \sownerlist_0
       \\
       \sownerenv_0; \sowner_0 \sWLheap \conf{\sexpr_0}{\vstore_0}{\bstore_0}
       \\\\
       \fbsetsenders{\fmapref{\bstore_0}{\eloc_0}} \subseteq \fvalueowners{\fmapref{\vstore_0}{\eloc_0}}
     }{
       \sownerenv_0; \sowner_0 \sWLheap \conf{\obbars{\echecktwo{\stoptional}{\sexpr_0}{\eloc_0}}{\sownerlist_0}}{\vstore_0}{\bstore_0}
     }
   \end{mathpar}}\end{minipage}\hfill
  \smallskip

  One subtle case of the proof concerns function application, because the unlabeled
   rule appears to blame a typed function (at address $\eloc_0$) for an unrelated incompatible value:

  \begin{displayrrarray}
    \conf{(\eapp{\stoptional}{\eloc_0}{\svalue_0})}{\vstore_0}{\bstore_0}
    &  \nredTX
    & \conf{\boundaryerror{\frev{\fmapref{\bstore_0}{\eloc_0}}}{\svalue_0}}{\vstore_0}{\bstore_0}
    \\\sidecond{if $\fmapref{\vstore_{0}}{\eloc_0}=\efun{\tann{\svar_0}{{\stype_0}}}{\sexpr_0}$
                and $\neg \fshallow{\ftagof{\stype_0}}{\fmapref{\vstore_0}{\svalue_0}}$}
  \end{displayrrarray}

  \noindent{}But the value is not unrelated.
  The shape check happens when the incoming value meets the function's type annotation;
   that is, after the function receives the input value.
  By @exact{\lawref{law:neg}}, the correct labeling is the following:

  \begin{displayrrarray}
    \conf{\obars{\eapp{\stoptional}{\obbars{\eloc_0}{\sownerlist_0}}{\svalue_0}}{\sowner_1}}{\vstore_0}{\bstore_0}
    &  \nredTX
    & \conf{\boundaryerror{\frev{\fmapref{\bstore_0}{\eloc_0}}}{\obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}}}{\vstore_1}{\bstore_0}\!\!\!\!
    \\\sidecond{if $\fmapref{\vstore_{0}}{\eloc_0}=\efun{\tann{\svar_0}{{\stype_0}}}{\sexpr_0}$
                and $\neg \fshallow{\ftagof{\stype_0}}{\fmapref{\vstore_0}{\svalue_0}}$}
    \\\sidecond{where $\vstore_1 \eeq \fmapreplace{\vstore_0}{\svalue_0}{\obbars{\fmapref{\vstore_0}{\svalue_0}}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}}$}
  \end{displayrrarray}

  \noindent{}Additionally blaming $\fmapref{\bstore_0}{\svalue_0}$
   seems like a useful change to the original \tname{} semantics@~citep{vss-popl-2017}.
  Thanks to the heap-based blame analysis, one can see that the change gives
   more information and preserves the $\propbsheap$\/ property.

  Blame completeness fails because \tname{} does not update the blame map
   during an untyped function application.
  Refer to the proof of @exact{\theoremref{thm:T-BC}} for an example term.
\end{proofsketch}
}|

@exact|{
\begin{theorem}\label{thm:T-preorder}
  $\fsym{} \sbehaviorle \tsym{}$
\end{theorem}
\begin{proofsketch}
  \fname{} and \tname{} are very similar; the only behavioral difference
   comes from subtyping.
  \fname{} uses wrappers to enforce the type on a boundary.
  \tname{} uses boundary types only for an initial shape check, and instead uses the static types
   in typed code to guide checks at elimination forms.

  In the following expression, a boundary declares one type and an elimination
   form requires a weaker type:

  \begin{displayrrarray}
    \sexpr_0 \eeq \efst{\tint}{(\edynb{\obnd{\sowner_0}{(\tpair{\tnat}{\tnat})}{\sowner_1}}{\epair{{-4}}{4}})}
  \end{displayrrarray}

  \noindent{}Since ${{-4}}$ is an integer, \tname{} reduces to a value.
  \fname{} detects an error.
\end{proofsketch}
}|


@section[#:tag "sec:design:tech:amnesic"]{@exact{\fproperties{\aname{}}}}

@figure*[
  "fig:amnesic-reduction"
  @elem{@|aname| notions of reduction}
  @exact|{
  \lbl{\fbox{\aname{} Syntax}~extends \syntaxho}{
    \begin{langarray}
      \svalue & \BNFeq &
        \sint \mid
        \snat \mid
        \epair{\svalue}{\svalue} \mid
        \efun{\svar}{\sexpr} \mid
        \efun{\tann{\svar}{\stype}}{\sexpr} \mid
        \emon{\obnd{\sowner}{\tfun{\stype}{\stype}}{\sowner}}{\svalue} \mid
        \emon{\obnd{\sowner}{\tpair{\stype}{\stype}}{\sowner}}{\svalue} \mid
        \ehist{\sblist}{\svalue}
    \end{langarray}
  }

  \smallskip
  \lbl{\fbox{$\sexpr \nredAS \sexpr$} \missingrules{}}{
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
      & \boundaryerror{\fconcat{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sblist_0}}{\svalue_0}
      \\\sidecond{if $\neg\fshallow{\tagof{\stype_0}}{\svalue_0}$
                  and $\sblist_0 \eeq \fgettrace{\svalue_0}$}
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
      \eapp{\stype_0}{(\emon{\obnd{\sowner_0}{\stype_1}{\sowner_1}}{\svalue_0})}{\svalue_1}
      & \nredAS
      & \edynb{\sbnd_0}{(\eapp{\tdyn}{\svalue_0}{(\estab{\sbnd_1}{\svalue_1})})}
      \\\sidecond{where $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_0}{\sowner_1}$
                  and $\sbnd_1 \sassign \obnd{\sowner_1}{\ftypedom{\stype_1}}{\sowner_0}$}
    \end{rrarray}
  }

  \smallskip
  \lbl{\fbox{$\sexpr \nredAD \sexpr$} \missingrules{}}{
    \begin{rrarray}
      \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      & \nredAD
      & \emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0}
      \\\sidecond{if $\fshallow{\tagof{\stype_0}}{\svalue_0}$
                  and $\svalue_0 \in {\epair{\svalue}{\svalue}} \cup {(\efun{\tann{\svar}{{\stype}}}{\sexpr})}$}
      \\[0.5ex]
      \estab{\sbnd_0}{(\emon{\sbnd_1}{(\ehopt{\sblist_0}{\svalue_0})})}
      & \nredAD
      & \eprehist{(\fconcat{\sbnd_0}{\fconcat{\sbnd_1}{\sblist_0}})}{\svalue_0}
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
      \efst{\tdyn}{(\ehopt{\sblist_0}{(\emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0})})}
      & \nredAD
      & \eprehist{\sblist_0}{(\estab{\sbnd_0}{(\efst{\stype_1}{\svalue_0})})}
      \\\sidecond{where $\stype_1 \sassign \ftypefst{\stype_0}$
                  and $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_1}{\sowner_1}$}
      \\[0.5ex]
      \esnd{\tdyn}{(\ehopt{\sblist_0}{(\emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0})})}
      & \nredAD
      & \eprehist{\sblist_0}{(\estab{\sbnd_0}{(\esnd{\stype_1}{\svalue_0})})}
      \\\sidecond{where $\stype_1 \sassign \ftypesnd{\stype_0}$
                  and $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_1}{\sowner_1}$}
      \\[0.5ex]
      \eapp{\tdyn}{(\ehopt{\sblist_0}{(\emon{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\svalue_0})})}{\svalue_1}
      & \nredAD
      & \eprehist{\sblist_0}{(\estab{\sbnd_0}{(\eapp{\stype_2}{\svalue_0}{\sexpr_0})})}
      \\\sidecond{where $\stype_2 \sassign \ftypecod{\stype_0}$
                  and $\sbnd_0 \sassign \obnd{\sowner_0}{\stype_2}{\sowner_1}$
                  and $\sbnd_1 \sassign \obnd{\sowner_1}{\ftypedom{\stype_0}}{\sowner_0}$}
      \\\sidecond{and $\sexpr_0 \sassign (\edynb{\sbnd_1}{(\faddtrace{\frev{\sblist_0}}{\svalue_1})})$}
      \\[1.0ex]
      \eprehist{\sblist_0}{\svalue_0}
      & \nredAD
      & \svalue_1
      \\\sidecond{where $\svalue_1 \sassign \faddtrace{\sblist_0}{\svalue_0}$}
    \end{rrarray}
  }

  \smallskip
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
     later used at a supertype
     (@${\eapp{\tint}{(\emon{\obnd{\sowner_0}{(\tfun{\tnat}{\tnat})}{\sowner_1}}{\efun{\svar_0}{{-7}}})}{2}}).
    @|aname| runs this application without error but @|fname| raises a boundary error.
}]

Thus, the following wrapped values can occur at run-time:

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

    \smallskip
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

@Figure-ref{fig:amnesic-reduction}
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
\begin{theorem}
  \aname{} satisfies $\propts{\sidproj}$
\end{theorem}
\begin{proofsketch}
  When \aname{} extracts an element from a guarded pair:

  \begin{displayrrarray}
    \efst{\stype_0}{(\emon{\obnd{\sowner_0}{\tpair{\stype_1}{\stype_2}}{\sowner_1}}{\svalue_0})}
    & \nredAS
    & \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{(\efst{\tdyn}{\svalue_0})}
  \end{displayrrarray}

  \noindent{}a newly-created boundary enforces the context's assumption ($\stype_0$)
   about the first element of the untyped pair.

   The examples in the proof of @exact{\theoremref{thm:F-TS}} justify the \aname{} strategy
    for removing guard wrappers.
\end{proofsketch}

\begin{theorem}
  \aname{} does not satisfy $\propcm{}$
\end{theorem}
\begin{proofsketch}
  Removing a wrapper creates a value with more than one label:

  \begin{displayrrarray}
    \obars{\estab{\obnd{\sowner_0}{(\tfun{\stype_0}{\stype_1})}{\sowner_1}}{\obbars{\emon{\sbnd_1}{\obbars{\ehist{\sblist_0}{\obbars{\efun{\svar_0}{\svar_0}}{\sownerlist_2}}}{\sownerlist_3}}}{\sownerlist_4}}}{\sowner_5}
      & \nredAD
    \\[1ex]\sidecond{\(\obbars{\eprehist{(\fconcat{\obnd{\sowner_0}{(\tfun{\stype_0}{\stype_1})}{\sowner_1}}{\fconcat{\sbnd_1}{\sblist_0}})}{\obbars{\efun{\svar_0}{\svar_0}}{\sownerlist_2}}}{\fconcat{\sownerlist_3}{\fconcat{\sownerlist_4}{\sowner_5}}}\)}
  \end{displayrrarray}

\end{proofsketch}
}|

@exact|{
\begin{theorem}
  \aname{} satisfies $\propbspath$\/ and $\propbcpath$
\end{theorem}
\begin{proofsketch}
  By progress and preservation lemmas for a path-based consistency judgment, $\sWLpath$, that weakens
   single-owner consistency to allow multiple labels around a trace-wrapped value.

  \smallskip
  \hfill\begin{minipage}{0.8\columnwidth}
  \lbl{\fbox{$\sownerenv; \sowner \sWLpath \sexpr$} \missingrules{}}{\begin{mathpar}
      \inferrule*{
        \sblist_0 = \obnd{\sowner_0}{\stype_0}{\sowner_1} \cdots \obnd{\sowner_{n-1}}{\stype_{n-1}}{\sowner_n}
        \\
        \sownerenv_0; \sowner_n \sWLpath \svalue_0
      }{
        \sownerenv_0; \sowner_0 \sWLpath \obars{\ehist{\sblist_0}{\obbars{\svalue_0}{\sowner_n \cdots \sowner_1}}}{\sowner_0}
      }
  \end{mathpar}}\end{minipage}\hfill
  \smallskip

  Now, consider again the guard-dropping rule:

  \begin{displayrrarray}
    \obars{\estab{\obnd{\sowner_0}{(\tfun{\stype_0}{\stype_1})}{\sowner_1}}{\obbars{\emon{\sbnd_1}{\obbars{\ehist{\sblist_0}{\obbars{\efun{\svar_0}{\svar_0}}{\sownerlist_2}}}{\sownerlist_3}}}{\sownerlist_4}}}{\sowner_5}
      & \nredAD
    \\[1ex]\sidecond{\(\obbars{\eprehist{(\fconcat{\obnd{\sowner_0}{(\tfun{\stype_0}{\stype_1})}{\sowner_1}}{\fconcat{\sbnd_1}{\sblist_0}})}{\obbars{\efun{\svar_0}{\svar_0}}{\sownerlist_2}}}{\fconcat{\sownerlist_3}{\fconcat{\sownerlist_4}{\sowner_5}}}\)}
  \end{displayrrarray}

  \noindent{}Let $\sbnd_1 = \obnd{\sowner_1}{(\tfun{\stype_0}{\stype_1})}{\sowner_6}$.
  Path-consistency for the redex implies $\sownerlist_3 = \sowner_6$ and $\sownerlist_4 = \sowner_1$.
  Thus the new labels on the result, $\sowner_2$ and $\sowner_1$, that match the sender names on the two boundaries added to the trace wrapper.
\end{proofsketch}
}|

@exact|{
\begin{theorem}
  $\tsym{} \sbehavioreq \asym{}$
\end{theorem}
\begin{proofsketch}
  Consider the expression from @exact{\theoremref{thm:T-preorder}}:

  \begin{displayrrarray}
    \sexpr_0 \eeq \efst{\tint}{(\edynb{\obnd{\sowner_0}{(\tpair{\tnat}{\tnat})}{\sowner_1}}{\epair{{-4}}{4}})}
  \end{displayrrarray}

  \noindent{}Both \aname{} and \tname{} check for an integer.
  %%Thus, \aname{} gives an indirect argument that \tname{} satisfies $\propts{\sidproj}$
\end{proofsketch}
}|


@section[#:tag "sec:design:tech:erasure"]{@exact{\fproperties{\ename{}}}}

@figure*[
  "fig:erasure-reduction"
  @elem{@|ename| notions of reduction}
  @exact|{
  \lbl{\fbox{\ename{} Syntax}~extends \syntaxeo{}}{
    \begin{langarray}
      \svalue & \BNFeq &
        \sint \mid \snat \mid \epair{\svalue}{\svalue} \mid \efun{\svar}{\sexpr} \mid \efun{\tann{\svar}{\stype}}{\sexpr}
    \end{langarray}
  }

  \smallskip
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

@Figure-ref{fig:erasure-reduction} presents the values and notions of reduction
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
  \ename{} satisfies neither $\propts{\sidproj}$\/ nor $\propts{\stagproj}$
\end{theorem}
\begin{proof}
  Dynamic-to-static boundaries are unsound.
  A function, for example, can enter a typed context that expects an integer:

  \begin{displayrrarray}
    \edynb{\obnd{\sowner_0}{\tint}{\sowner_1}}{(\efun{\svar_0}{42})}
    & \nredEX
    & (\efun{\svar_0}{42})
  \end{displayrrarray}

\end{proof}
}|

@exact|{
\begin{theorem}
  \ename{} satisfies $\propts{\sdynproj}$
\end{theorem}
\begin{proofsketch}
  Every rule yields a well-formed expression.
\end{proofsketch}
}|

@exact|{
\begin{theorem}
  \ename{} does not satisfy $\propcm{}$
\end{theorem}
\begin{proofsketch}
  A static-to-dynamic boundary can create a value with multiple labels:

  \begin{displayrrarray}
    \obars{\estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\obars{\svalue_0}{\sowner_2}}}{\sowner_3}
    & \nredEX &
    \obbars{\svalue_0}{\fconcat{\sowner_2}{\sowner_3}}
  \end{displayrrarray}

\end{proofsketch}
}|

@exact|{
\begin{theorem}\leavevmode
  \begin{itemize}
    \item \ename{} satisfies\/ $\propbspath$
    \item \ename{} does not satisfy\/ $\propbcpath$
  \end{itemize}
\end{theorem}
\begin{proofsketch}
  An \ename{} boundary error blames an empty set, for example:

  \begin{displayrrarray}
    \efst{\tint}{(\efun{\svar_0}{\svar_0})}
    & \nredEX &
    \boundaryerror{\emptyset}{(\efun{\svar_0}{\svar_0})}
  \end{displayrrarray}

  \noindent{}The empty set is trivially blame sound.
  It is blame-incomplete because every value has at least one label, representing the context.
\end{proofsketch}
}|

@exact|{
\begin{theorem}
  $\asym{} \sbehaviorle \esym{}$
\end{theorem}
\begin{proofsketch}
  Let:

  \begin{displayrrarray}
    \sexpr_0 \eeq \eapp{\tnat}{(\edynb{\obnd{\sowner_0}{(\tfun{\tnat}{\tnat})}{\sowner_1}}{(\efun{\svar_0}{{-9}})})}{4}
  \end{displayrrarray}

  \noindent{}\aname{} checks for a natural-number result and errors, but \ename{} checks nothing.
\end{proofsketch}
}|


