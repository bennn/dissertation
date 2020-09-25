#lang greenman-thesis/include
@(require greenman-thesis/oopsla-2019/main)

@title[#:tag "sec:design:basic"]{Evaluation Framework}

This section introduces the basic ideas of the evaluation framework;
 detailed formal definitions are deferred to @sectionref{sec:design:technical}.
To formulate different type-enforcement stategies on an equal footing,
 the framework begins with one mixed-typed surface language (@sectionref{sec:design:basic:surface})
 and models stategies as distinct semantics (@sectionref{sec:design:semantic-framework}).
The properties listed above support an analysis.
Type soundness (@sectionref{sec:design:basic:ts}) and complete monitoring (@sectionref{sec:design:cm})
 characterize the type mismatches that a semantics detects.
Blame soundness and blame completeness (@sectionref{sec:design:ownership})
 measure the quality of error messages.
The error preorder (@sectionref{sec:design:basic:preorder}) enables direct behavioral comparisons.


@section[#:tag "sec:design:basic:surface"]{Surface Language}

The surface multi-language combines two independent pieces in the
 style of @citet{mf-toplas-2009}.
Statically-typed expressions constitute one piece;
 dynamically-typed expressions are the other half.
Technically, these expression languages are identified by two
 judgments: typed expressions @${\sexpr_0} satisfy @${\sWT \sexpr_0 : \stype_0}
 for some type @${\stype_0}, and untyped expressions @${\sexpr_1} satisfy
 @${\sWT \sexpr_1 : \tdyn} for the dynamic type.
Boundary expressions connect the two languages syntactically and
 enable run-time interactions.

Note that @${\tdyn} is not the flexible dynamic type that is compatible with
 any static type@~citep{t-popl-1990,st-sfp-2006},
 rather, it is the uni-type that describes all well-formed untyped
 expressions@~citep{mf-toplas-2009}.
Consequently, there is no need for a type precision judgment in the surface language
 because all mixed-typed interactions occur through boundary expressions.
How to add a dynamic type is a separate dimension that is orthogonal to the
 question of how to enforce types;
 whether or not a language has a dynamic type, our results
 apply to its type-enforcement strategy.
Whether the dynamic type is useful is a question for another time@~citep{g-snapl-2019}.

The core statically-typed (@${\svaluestat}) and dynamically-typed (@${\svaluedyn})
 values are mirror images, and consist of integers, natural numbers, pairs, and functions.
This common set of values is the basis for typed-untyped communication.
Types @${\stype} summarize values:

@exact|{
\smallskip
  \qquad%
  \begin{langarray}
    \svaluestat & \BNFeq &
      \sint \mid \snat \mid \wideas{\epair{\svaluestat}{\svaluestat}}{\epair{\svaluedyn}{\svaluedyn}} \mid \efun{\tann{\svar}{\stype}}{\sexprstat}
    \\
    \svaluedyn & \BNFeq &
      \sint \mid \snat \mid \epair{\svaluedyn}{\svaluedyn} \mid \efun{\svar}{\sexprdyn}
    \\[1ex]
    \stype & \BNFeq &
      \tint \mid \tnat \mid \tfun{\stype}{\stype} \mid \tpair{\stype}{\stype}
  \end{langarray}
\smallskip
}|

These value sets are relatively small, but suffice to illustrate the behavior of
 gradual types for the basic ingredients of a full language.
First, the values include
 atomic data, finite structures, and higher-order values.
Second, the natural numbers @${\snat} are a subset of the integers @${\sint} to
 motivate a subtyping judgment for the typed half of the language.
Subtyping helps the model distinguish between two type-sound methods of
 enforcing types (declaration-site vs. use-site) and demonstrates how the
 model can scale to include true union types, which must be part of any type
 system for originally-untyped code@~citep{tf-icfp-2010,cl-icfp-2017,tfffgksst-snapl-2017}.

Surface expressions include function application, primitive operations, and
 boundaries.
The details of the first two are fairly standard (@sectionref{sec:design:surface-language}),
 but note that function application comes with an explicit @${\sapp} operator (@${\sapp~\sexpr_0~\sexpr_1}).
Boundary expressions are the glue that enables mixed-typed programming.
A program starts with named chunks of code, called components.
Boundary expressions link these chunks together with a static type
 to describe the types of values that may cross the boundary.
Suppose that a typed component named @${\sowner_0} imports and applies an
 untyped function from component @${\sowner_1}:


@exact|{
%% 2020-09-24 : still using tikz to get the fonts right
\begin{equation}
  \label{eq:bnd:e0}
  \begin{minipage}{27mm}
    {\hfill\(\sowner_1\)~~}\\[-3mm]
    \begin{mdframed}[style=dynframestyle,userdefinedwidth=27mm]\(
      \efun{\svar_0}{\ssum~\svar_0~2}
    \)\end{mdframed}
  \end{minipage}\begin{minipage}{24mm}\begin{tikzpicture}
    \node (A) {};
    \node (B) [right of=A,xshift=3em] {};
    \draw[->] (A)
      edge [bend left]
      node [above] {\raisebox{0pt}[0pt]{$\tfun{\tnat}{\tnat}$}}
      node [below,yshift=-3mm] {\raisebox{0pt}[0pt]{$f$}}
      (B);
  \end{tikzpicture}\end{minipage}\begin{minipage}{11mm}
    {\hfill\(\sowner_0\)~~~}\\[-3mm]
    \begin{mdframed}[style=staframestyle,userdefinedwidth=11mm]\(f~9\)\end{mdframed}
  \end{minipage}
\end{equation}
}|

@|noindent|The surface language can model the composition of these components with a boundary
 expression that embeds an untyped function in a typed context.
The boundary expression is annotated with a @emph{boundary specification}
 @${\obnd{\sowner_0}{\tfun{\tnat}{\tnat}}{\sowner_1}} to explain that
 component @${\sowner_0} expects a function from sender @${\sowner_1}:

@exact|{
\begin{displayrrarray}
  % note: purposefully avoiding the application type annotation
  \eqref{eq:bnd:e0} ~=~ \sapp~(\edynb{\obnd{\sowner_0}{\tfun{\tnat}{\tnat}}{\sowner_1}}{(\efun{\svar_0}{\ssum~\svar_0~2})})~9
\end{displayrrarray}
}|

@|noindent|In turn, this two-component expression may be imported into a larger
 untyped component.
The sketch below shows an untyped component in the center that imports
 two typed components:
 a new typed function on the left and the expression @exact{\eqref{eq:bnd:e0}}
 on the right.

@exact|{
\begin{equation}
  %% 2020-09-24 : still using tikz to get the fonts right
  \label{eq:bnd:e1}
  \begin{minipage}{39mm}
    {\hfill\(\sowner_3\)~~~}\\[-3mm]
    \begin{mdframed}[style=staframestyle,userdefinedwidth=39mm]\(
      \efun{\tann{\svar_1}{\tpair{\tint}{\tint}}}{\sfst~\svar_1}
    \)\end{mdframed}
  \end{minipage}\begin{minipage}{32mm}\begin{tikzpicture}
    \node (A) {};
    \node (B) [right of=A,xshift=5em] {};
    \node (vspace) [below of=A,yshift=7mm] {};
    \draw[->] (A)
      edge [bend left]
      node [above] {\raisebox{0pt}[0pt]{$\tfun{(\tpair{\tint}{\tint})}{\tint}$}}
      node [below,yshift=-2mm] {\raisebox{0pt}[0pt]{$g$}}
      (B);
  \end{tikzpicture}\end{minipage}\begin{minipage}{10mm}
    {\hfill\(\sowner_2\)~~}\\[-3mm]
    \begin{mdframed}[style=dynframestyle,userdefinedwidth=10mm]\(g~x\)\end{mdframed}
  \end{minipage}\begin{minipage}{17mm}\begin{tikzpicture}
    \node (A) {};
    \node (B) [right of=A,xshift=1em] {};
    \draw[<-] (A)
      edge [bend left]
      node [above] {\raisebox{0pt}[0pt]{$\tnat$}}
      node [below,yshift=-2mm] {\raisebox{0pt}[0pt]{$x$}}
      (B);
  \end{tikzpicture}\end{minipage}\begin{minipage}{7mm}
    \raisebox{-6mm}{\eqref{eq:bnd:e0}}
  \end{minipage}
\end{equation}
}|

@|noindent|When linearized to the surface language, this term becomes:

@exact|{
\begin{displayrrarray}
  \eqref{eq:bnd:e1} ~=~ \sapp~(\estab{\obnd{\sowner_2}{\tfun{\tpair{\tint}{\tint}}{\tint}}{\sowner_3}}{(\efun{\tann{\svar_1}{\tpair{\tint}{\tint}}}{\sfst~\svar_1})})
  \\[0.5ex]
  \hphantom{\eqref{eq:bnd:e1} ~=~ \sapp~}(\estab{\obnd{\sowner_2}{\tnat}{\sowner_0}}{\eqref{eq:bnd:e0}})
\end{displayrrarray}
}|

Technically, a boundary expression combines a boundary specification @${\sbnd}
 and a sender expression.
The specification includes the names of the client and sender components,
 in that order, along with a type to describe values that are intended to cross
 the boundary.
Names, such as @${\sowner_0}, come from some countable set @${\sowner}.
The boundary types guide the static type checker, but are mere suggestions
 unless a semantics decides to enforce them.

@exact|{
\smallskip
  \qquad%
  \begin{langarray}
    \sexprstat & \BNFeq &
      \ldots \mid \edynb{\sbnd}{\sexprdyn}
    \\
    \sexprdyn & \BNFeq &
      \ldots \mid \estab{\sbnd}{\sexprstat}
  \end{langarray}
  \qquad
  \begin{langarray}
    \sbnd & \BNFeq &
      \obnd{\sowner}{\stype}{\sowner}
    \\
    \sowner & \BNFeq &
      \textrm{\scountable{} set of names}
  \end{langarray}
\smallskip
}|

The typing judgments for typed and untyped expressions require a mutual
 dependence to handle boundary expressions.
A well-typed expression may include any well-formed dynamically-typed code.
Conversely, a well-formed untyped expression may include any typed expression
 that matches the specified annotation:

@exact|{
\smallskip
{\hfill
  \begin{minipage}{0.45\columnwidth}
    {\noindent\parbox[t]{\columnwidth}{{\fbox{$\stypeenv \sWT \sexpr : \stype$}}\\[-4mm]
    \begin{mathpar}
      \inferrule*{
        \stypeenv_0 \sWT \sexpr_0 : \tdyn
      }{
        \stypeenv_0 \sWT \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sexpr_0} : \stype_0
      }
    \end{mathpar}}}
  \end{minipage}\begin{minipage}{0.45\columnwidth}
    {\noindent\parbox[t]{\columnwidth}{{\fbox{$\stypeenv \sWT \sexpr : \tdyn$}}\\[-4mm]
    \begin{mathpar}
      \inferrule*{
        \stypeenv_0 \sWT \sexpr_0 : \stype_0
      }{
        \stypeenv_0 \sWT \estab{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sexpr_0} : \tdyn
      }
    \end{mathpar}}}
  \end{minipage}
\hfill}\smallskip
}|

Each surface-language component must have a name,
 drawn from a set @${\sowner} of labels.
These names must be @emph{coherent} according to a judgment that validates an
 expression relative to a current name and a mapping from variables to names
 (@exact{\hyperref[fig:surface-ownership]{$\sownerenv; \sowner \sWL \sexpr$}}, @sectionref{sec:design:surface-language}).
All boundary specifications must have a client name that matches the current
 name, and variables bound in one component cannot appear free in a different
 one.

The purpose of the names is to enable a notion of @emph{ownership}, or responsibility.
As an expression reduces to a value, ownership determines which components are
 responsible for the current expression and all subexpressions.
Since component names appear in the surface syntax, they can help explain
 a run-time mismatch in terms of source-code boundaries.
Suppose a program halts due to a mismatch between a type and a value.
If one component is responsible for the value and the language can find both the
 client with the type expectation and source of the incompatible value, then a
 programmer knows exactly where to start debugging---either the expectation
 or the value-source must change.




@section[#:tag "sec:design:semantic-framework"]{Semantic Framework}

The surface language enables the construction of mixed-typed expressions.
The next step is to assign behaviors to these programs via formal semantics.
Semantics that correspond to different type-enforcement strategies
must have equivalent behavior on boundary-free expressions.
Fully-typed terms, for instance, must compute equivalent values.
Starting from this constraint, the central design problem is how to enforce boundary types.

The first ingredient of a semantics is the set of result values @${\svalue} that
 expressions may reduce to.
A result set typically extends the core typed and untyped values mentioned above
 (@${\svalue \supseteq \svaluestat \cup \svaluedyn}).
Potential reasons for the extended value set include the following:
@itemlist[#:style 'ordered
  @item{
    to permit untyped values in typed code, and vice versa;
  }
  @item{
    to track the identity of values on a heap;
  }
  @item{
    @latex-label{val:monitor}
    to associate a value with a delayed type-check; and
  }
  @item{
    @latex-label{val:trace}
    to record the boundaries that a value has previously crossed.
  }
]
Reasons @latex-ref{val:monitor} and @latex-ref{val:trace} introduce two kinds of
 wrapper value.
A guard wrapper, written @${\emon{\sbnd}{\svalue}}, associates a boundary
 specification with a value to achieve delayed type checks.
A trace wrapper, written @${\ehist{\sblist}{\svalue}}, attaches a list of
 boundaries to a value as metadata.
Guards are similar to boundary expressions; they separate a context
 component from a value component.
Trace wrappers simply annotate values.

Note: a language with the dynamic type will need a third
  wrapper for basic values that have been assigned type dynamic.
We conjecture that this wrapper is the only change needed to transfer our
 positive results.
Our negative results do not require changes for the dynamic type because
 such a language can express all our ``precisely-typed'' counterexample terms.

Second, a semantics must give reduction rules for boundary expressions.
These rules initiate a type-enforcement strategy.
For example, the @|nname| semantics (@sectionref{sec:design:tech:natural})
 enforces full types via classic techniques@~citep{ff-icfp-2002,mf-toplas-2009}.
It admits the following two reductions.
Note a filled triangle (@${\snreddyn}) describes a step in untyped
 code and an open triangle (@${\snredsta}) is for statically-typed code:

@exact|{
\begin{displayrrarray}
  \rrnum{a} \estab{\obnd{\sowner_0}{\tnat}{\sowner_1}}{42}
  & \nredND
  & 42
\end{displayrrarray}
}|

@exact|{
\begin{displayrrarray}
  \rrnum{b} \edynb{\obnd{\sowner_0}{(\tfun{\tint}{\tnat})}{\sowner_1}}{(\efun{\svar_0}{{-8}})}
  & \nredNS
  \\\qquad\qquad\zerowidth{\emon{\obnd{\sowner_0}{(\tfun{\tint}{\tnat})}{\sowner_1}}{(\efun{\svar_0}{{-8}})}}
\end{displayrrarray}
}|

@|noindent|The first rule lets a typed number enter an untyped context.
The second rule gives typed code access to an untyped function through a newly-created
 guard wrapper.
Guard wrappers are a @emph{higher-order} tool for enforcing higher-order types.
As such, wrappers require elimination rules.
The @|nname| semantics includes the following rule to unfold
 the application of a typed, guarded function into two boundaries:

@exact|{
\begin{displayrrarray}
   \rrnum{c} \sapp~{(\emon{\obnd{\sowner_0}{(\tfun{\tint}{\tnat})}{\sowner_1}}{(\efun{\svar_0}{{-8}})})}~{1}
   & \nredNS
   \\\qquad\qquad\zerowidth{\edynb{\obnd{\sowner_0}{\tnat}{\sowner_1}}{(\sapp~{(\efun{\svar_0}{{-8}})}~{(\estab{\obnd{\sowner_1}{\tint}{\sowner_0}}{1})})}}
\end{displayrrarray}
}|

@|noindent|Other semantics have different behavior at boundaries and
 different supporting rules.
The @|tname| semantics (@sectionref{sec:design:tech:transient}) takes a @emph{first-order}
 approach to boundaries.
Instead of using wrappers, it checks shapes at a boundary and guards
 elimination forms with shape-check expressions.
For example, the following simplified reduction demonstrates a successful check.
The triangle is filled gray (@${\nredXsym}) because @|tname| is defined
 via one notion of reduction that handles both typed and untyped code:

@exact|{
\begin{displayrrarray}
  \rrnum{d} \echecktwo{(\tpair{\tnat}{\tnat})}{\epair{{-1}}{{-2}}}{}
  & \nredTX
  & {\epair{{-1}}{{-2}}}
\end{displayrrarray}
}|

These two points, values and checking rules, are the distinctive aspects of
 a semantics.
Other ingredients can be shared, such as the:
 errors, evaluation contexts, and interpretation of primitive operations.
Indeed, @sectionref{sec:design:tech:eval} defines three baseline evaluation
 languages---higher-order, first-order, and erasure---that abstract over the
 common ingredients.


@section[#:tag "sec:design:basic:ts"]{Type Soundness}

Type soundness asks whether evaluation is well-defined, and whether
  a surface-language type predicts aspects of the result.
Since there are two kinds of surface expression, soundness has two parts:
 one for statically-typed code and one for dynamically-typed code.

For typed code, the question is whether code can trust the
 types of its subexpressions.
If an expression with static type @${\stype_0} reduces to a value, what
 (if anything) does type @${\stype_0} predict about that value?
There are a range of possible answers.
At one end, the result value may match the full type @${\stype_0} according
 to an evaluation-language typing judgment.
The other extreme is that the result is a well-formed value of indeterminate
 shape.
In both cases, the programmer knows that typed code cannot reach an undefined
 state during evaluation.

For untyped code, there is one surface type.
If an expression reduces to a value, then uni-type soundness can only guarantee
 that the result is a well-formed value of indeterminate shape.
The practical benefit of such a theorem is that untyped code cannot reach
 an undefined state through mixed-typed interactions.

Both parts combine into the following rough definition, where the
 function @${\sXproj} and judgment @${\sWTX} are parameters.
The function maps surface types to observations that one can make about a result;
 varying the choice of @${\sXproj} offers a spectrum of soundness for typed code.
The judgment @${\sWTX} matches a value with a description.

@exact|{
\definitionsketch{$\sXproj{}$-\textrm{type soundness}}{
  \begin{minipage}[t]{0.50\columnwidth}
    If\/ $\sexpr_0$ has static type\/ $\stype_0$ (\/$\sWT \sexpr_0 : \stype_0$),\\
     then one of the following holds:
     \vspace{-1ex}
    \begin{itemize}
      \item
        $\sexpr_0$ reduces to a value\/ $\svalue_0$\\ and\/ $\sWTX \svalue_0 : \fXproj{\stype_0}$
      \item
        $\sexpr_0$ reduces to an allowed error
      \item
        $\sexpr_0$ reduces endlessly.
    \end{itemize}
  \end{minipage}\begin{minipage}[t]{0.48\columnwidth}
    If\/ $\sexpr_0$ is untyped ($\sWT \sexpr_0 : \tdyn$),\\
     then one of the following holds:
     \vspace{-1ex}
    \begin{itemize}
      \item
        $\sexpr_0$ reduces to a value\/ $\svalue_0$\\ and\/ $\sWTX \svalue_0 : \tdyn$
      \item
        $\sexpr_0$ reduces to an allowed error
      \item
        $\sexpr_0$ reduces endlessly.
    \end{itemize}
  \end{minipage}
}
}|


@section[#:tag "sec:design:cm"]{Complete Monitoring}

Complete monitoring tests whether a mixed-typed semantics has control over
 every interaction between typed and untyped code.
If the property holds, then a programmer can rely on the language to insert
 check at the proper points, for example, between the library and client
 demonstrated in @figureref{fig:tr-example}.
Concretely, if a value passes through the type @${(\tfun{\tint}{\tint})}
 then complete monitoring guarantees that the language has control over
 every input to the function and every result that the function computes,
 regardless of whether these interactions occur in a typed or untyped context.

Because all such interactions originate at the boundaries
 between typed and untyped code,
 a first-draft way to formalize complete monitoring is to ask whether each
 boundary comes with a full run-time check when possible and an error otherwise.
A language that meets this strict requirement certainly has full control.
However, other good designs fail.
Suppose typed code expects a pair of integers and a semantics initially
 admits any pair at the boundary but eventually checks that the pair contains integers.
Despite the incomplete check at the boundary, this delayed-checking semantics eventually
 performs all necessary checks and should satisfy a complete monitoring theorem.
Higher-order values raise a similar question because a single run-time check
 cannot prove that a function value always behaves a certain way.
Nevertheless, a language that checks every call and return is in full control
 of the interactions between a function and its context.

Our definition of complete monitoring translates these intuitions about
 interactions and control into statements about @emph{ownership labels}@~citep{dfff-popl-2011}.
At the start of an evaluation, no interactions have occurred yet and every
 expression has one owner: the enclosing component.
The reduction of a boundary term is the semantics of an interaction in which
 a value flows from one sender component to a client.
At this point, the sender loses full control over the value.
If the value fully matches the type expectations of the client, then the loss
 of control is no problem and the client gains full ownership.
Otherwise, the sender and client may have to assume joint ownership of the value,
 depending on the nature of the reduction relation.
If a semantics can create a value with multiple owners, then it admits that
 a component may lose full control over its interactions with other components.

Technically, an ownership label @${{}^{\sowner_0}} names one source-code component.
Expressions and values come with at least one ownership label;
 for example, @${\obars{42}{\sowner_0}} is an integer with one owner
 and @${\obars{\obars{\obars{42}{\sowner_0}}{\sowner_1}}{\sowner_2}} is an
 integer with three owners, written @${\obbars{42}{\fconcat{\sowner_0}{\fconcat{\sowner_1}{\sowner_2}}}} for short.
A complete monitoring theorem requires two ingredients that manage these labels.
First, a reduction relation @${\samplerred}
 must propagate ownership labels to reflect interactions and checks.
Second, a single-ownership judgment @${\sWL} must test whether every value in an
 expression has a unique owner.
To satisfy complete monitoring, reduction must preserve single-ownership.

The key single-ownership rules deal with labeled expressions and boundary terms:

@exact|{
\smallskip
\lbl{\fbox{$\sownerenv; \sowner \sWL \sexpr$}}{\begin{mathpar}
    \inferrule*{
      \sownerenv_0; \sowner_0 \sWL \sexpr_0
    }{
      \sownerenv_0; \sowner_0 \sWL \obars{\sexpr_0}{\sowner_0}
    }

    \inferrule*{
      \sownerenv_0; \sowner_1 \sWL \sexpr_0
    }{
      \sownerenv_0; \sowner_0 \sWL \edynb{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sexpr_0}
    }

\end{mathpar}}
}|

@|noindent|Values such as @${\obbars{42}{\fconcat{\sowner_0}{\sowner_1}}}
 represent a communication that slipped through the run-time checking protocol,
 and therefore fail to satisfy single ownership.
@bold{Sneak preview} one way that a semantics can transfer a higher-order value
without creating a joint-ownership is by providing controlled access through
a wrapper.
The client owns the wrapper, and the sender retains ownership of the enclosed value.


@exact|{
\definitionsketch{\textrm{complete monitoring}}{
  For all\/ ${}\sWL \sexpr_0$,
  any reduction\/ $\sexpr_0 \samplerred \sexpr_1$
  implies\/ ${}\sWL \sexpr_1$.
}\smallskip
}|

The definition of complete monitoring is deceptively simple because it assumes
 a reduction relation that correctly propagates labels.
In practice, a language comes with an unlabeled reduction relation,
 and it is up to a researcher to design a lifted relation that handles labeled terms.
Lifting requires insight to correctly transfer labels
 and to ensure that labels do not change the behavior of programs.
If labels do not transfer correctly, then a complete monitoring theorem becomes
 meaningless.
And if the lifted relation depends on labels to compute a result, then
 a complete monitoring theorem says nothing about the original reduction relation.



@subsection[#:tag "sec:design:laws"]{How to lift a reduction relation}

The models in @sectionref{sec:design:technical} present six reduction relations
 for a mixed-typed language.
Each relation needs a lifted version to support an attempt at a complete
 monitoring proof.
These lifted reduction relations are deferred to supplementary material,
 but come about semi-automatically through the
 following informal guidelines, or ``natural laws,'' for labeling.

Each law describes one way that labels may be transferred or dropped
 during evaluation.
To convey the general idea, each law also comes with a brief illustration, namely,
 an example reduction and a short comment.
The example reductions use a hypothetical @${\samplerrarrow} relation
 over the surface language.
Recall that @${\sstat} and @${\sdyn} are boundary terms; they link two
 components, a context and an enclosed expression, via a type.
When reading an example, accept the transitions
 @${\sexpr\!\samplerrarrow\!\sexpr} as axioms and focus on how the labels change
 in response.

@exact|{
{\begin{enumerate}
    %% NOTE when editing laws, remember there is an 8th in technical.tex for transient
    \itemsep1ex
    \item \label{law:base}
      If a base value reaches a boundary with a matching base type,
      then the value must drop its current labels as it crosses the boundary.
      %% NOTE before we said 'may drop' to avoid being too-restrictive,
      %%  but if 'may' is possible there's an argument that Natural is not
      %%  a complete monitor ... nor any semantics that lets base values cross.
      %% 'must' is less confusing and avoids this interpretation
    \subitem\hfill $\newcommand{\thevalue}{0}
              \obars{\estab{\obnd{\sowner_0}{\tnat}{\sowner_1}}{\obars{\thevalue}{\fconcat{\sowner_2}{\sowner_1}}}}{\sowner_0}
              \samplerrarrow \obars{\thevalue}{\sowner_0}$
    \subitem\hfill
      \emph{The value\/ $0$ fully matches the type\/ $\tnat$.}

    %[law of no-check transfer]
    \item \label{law:cross}
      Any other value that crosses a boundary must acquire the label of
      the new context.
    \subitem\hfill
      $\newcommand{\thevalue}{\epair{{-2}}{1}}
                \obars{\estab{\obnd{\sowner_0}{\tnat}{\sowner_1}}{\obars{\thevalue}{\sowner_1}}}{\sowner_0}
                \samplerrarrow \obbars{\thevalue}{\fconcat{\sowner_1}{\sowner_0}}$
    \subitem\hfill
      \emph{The pair\/ $\epair{{-2}}{1}$ does not match the type\/ $\tnat$.}

    \item \label{law:pos}
      Every value that flows out of a value $\svalue_0$
      acquires the labels of $\svalue_0$ and the context.
    \subitem\hfill
      $\obars{\ssnd~{\obbars{\epair{\obars{1}{\sowner_0}}{\obars{2}{\sowner_1}}}{\fconcat{\sowner_2}{\sowner_3}}}}{\sowner_4}
       \samplerrarrow \obbars{2}{\fconcat{\sowner_1}{\fconcat{\sowner_2}{\fconcat{\sowner_3}{\sowner_4}}}}$
    \subitem\hfill
      \emph{The value\/ $2$ flows out of the pair\/ $\epair{1}{2}$.}

    \item \label{law:neg}
      Every value that flows into a value $\svalue_0$ acquires the label
      of the context and the reversed labels of $\svalue_0$.
    \subitem\hfill
      $\newcommand{\thevalue}{\epair{8}{6}}
       \obars{\sapp~{\obbars{\efun{\svar_0}{\sfst~{\svar_0}}}{\fconcat{\sowner_0}{\sowner_1}}}~{\obars{\thevalue}{\sowner_2}}}{\sowner_3}
       \samplerrarrow$
    \subitem\hfill
       $\newcommand{\thevalue}{\epair{8}{6}}
        \obars{\obbars{\sfst~{\obbars{\thevalue}{\fconcat{\sowner_2}{\fconcat{\sowner_3}{\fconcat{\sowner_1}{\sowner_0}}}}}}{\fconcat{\sowner_0}{\sowner_1}}}{\sowner_3}$
    \subitem\hfill
      \emph{The argument value\/ $\epair{8}{6}$ is input to the function.} 
    \subitem\hfill
      \emph{The substituted body flows out of the function, and}
    \subitem\hfill
      \emph{by \lawref{law:pos} acquires the function's labels.}

    \item \label{law:new}
      A primitive operation ($\sdelta$) may remove labels on incoming base values.
    \subitem\hfill
      $\obars{\ssum~{\obars{2}{\sowner_0}}~{\obars{3}{\sowner_1}}}{\sowner_2}
       \samplerrarrow \obars{5}{\sowner_2}$
    \subitem\hfill
      \emph{Assuming\/ $\sdelta(\ssum, 2, 3) = 5$.}

    \item \label{law:dup}
      Consecutive equal labels may be dropped.
    \subitem\hfill
      $\obbars{0}{\fconcat{\sowner_0}{\fconcat{\sowner_0}{\fconcat{\sowner_1}{\sowner_0}}}} \eeq \obbars{0}{\fconcat{\sowner_0}{\fconcat{\sowner_1}{\sowner_0}}}$

    \item \label{law:error}
      Labels on an error term may be dropped.
    \subitem\hfill
      $\obars{\edynb{\obnd{\sowner_0}{\tint}{\sowner_1}}{(\ssum~{9}~{\obars{\divisionbyzeroerror}{\sowner_1}})}}{\sowner_0}
       \samplerrarrow \divisionbyzeroerror$

  \end{enumerate}}
}|

To show how these laws generate a lifted reduction relation,
 the following rules lift the examples from @sectionref{sec:design:semantic-framework}.
Each rule accepts input with any sequence of labels (@${\sownerlist}),
 pattern-matches the important ones, and shuffles via the guidelines.
The first rule (a') demonstrates a base-type boundary (@exact{\lawref{law:base}}).
The second (b') demonstrates a higher-order boundary (@exact{\lawref{law:cross}}); the
 new guard on the right-hand side implicitly inherits the context label.
The third rule (c') sends an input (@exact{\lawref{law:neg}}) and creates new application
 and boundary expressions.
The fourth rule (d') applies @exact{\lawref{law:pos}} for an output.

@exact|{
\begin{displayrrarray}
  \rrnum{a'} \obars{\estab{\obnd{\sowner_0}{\tnat}{\sowner_1}}{\obbars{42}{\sownerlist_2}}}{\sowner_3}
  & \nredND &
  \obars{42}{\sowner_3}
\end{displayrrarray}
}|

@exact|{
\begin{displayrrarray}
  \rrnum{b'} \obars{\edynb{\obnd{\sowner_0}{(\tfun{\tint}{\tnat})}{\sowner_1}}{\obbars{\efun{\svar_0}{\obbars{{-8}}{\sownerlist_2}}}{\sownerlist_3}}}{\sowner_4}
  & \nredNS &
  \\[0.5ex]\qquad\zerowidth{\obars{\emon{\obnd{\sowner_0}{(\tfun{\tint}{\tnat})}{\sowner_1}}{\obbars{\efun{\svar_0}{\obbars{{-8}}{\sownerlist_2}}}{\sownerlist_3}}}{\sowner_4}}
\end{displayrrarray}
}|

@exact|{
\begin{displayrrarray}
  \rrnum{c'} \obars{\sapp~{\obbars{\emon{\obnd{\sowner_0}{(\tfun{\tint}{\tnat})}{\sowner_1}}{\obars{\svalue_0}{\sowner_2}}}{\sownerlist_3}}~{\obbars{1}{\sownerlist_4}}}{\sowner_5}
  & \nredNS &
  \\[0.5ex]\qquad\zerowidth{\obars{\edynb{\obnd{\sowner_0}{\tnat}{\sowner_1}}{\obars{\sapp\,{\svalue_0}\,{(\estab{\obnd{\sowner_1}{\tint}{\sowner_0}}{\obbars{1}{\fconcat{\sownerlist_4}{\fconcat{\sowner_5}{\frev{\sownerlist_3}}}}})}}{\sowner_2}}}{\sowner_5}}
\end{displayrrarray}
}|

@exact|{
\begin{displayrrarray}
  \rrnum{d'} \obars{\echecktwo{(\tpair{\tnat}{\tnat})}{\obbars{\epair{\obbars{{-1}}{\sownerlist_0}}{\obbars{{-2}}{\sownerlist_1}}}{\sownerlist_2}}{}}{\sowner_3}
  & \nredTX
  \\[0.5ex]\qquad\qquad\zerowidth{\obbars{\epair{\obbars{{-1}}{\sownerlist_0}}{\obbars{{-2}}{\sownerlist_1}}}{\fconcat{\sownerlist_2}{\sowner_3}}}
\end{displayrrarray}
}|

Although the design of a lifted reduction relation is a challenge
 for every language at hand,
 the laws in this section bring across the intuition behind prior
 formalizations of complete monitoring@~citep{dfff-popl-2011,dtf-esop-2012,tsdtf-oopsla-2012,mdffc-oopsla-2016}
 and may help guide future work.


@section[#:tag "sec:design:ownership"]{Blame Soundness, Blame Completeness}

Blame soundness and blame completeness
 ask whether a semantics can identify the responsible parties
 in the event of a run-time mismatch.
A type mismatch occurs when a typed context receives an unexpected value.
The value may be the result of a boundary expression or an elimination form,
 and the underlying issue may lie with either the value,
 the current type expectation, or some prior communication.
In any event, a programmer needs to know which components previously handled
 the value to begin debugging.
A semantics offers information by blaming a set of boundaries (@${\sbset});
 the meta-question is whether those boundaries have any connection to the
 value at hand.

Suppose that a reduction halts on the value @${\svalue_0} and blames
 the set @${\sbset_0} of boundaries.
Ideally, the names in these boundaries should list exactly the components that
 have handled this value.
Ownership labels let us state the question precisely.
The lifted variant of the same reduction provides an independent specification
 of the responsible components; namely, the owners that get attached to
 @${\svalue_0} as it crosses boundaries.
Relative to this source-of-truth, blame soundness asks whether the
 names in @${\sbset_0} are a subset of the true owners.
Blame completeness asks for a superset of the true owners.

A semantics can trivially satisfy blame soundness alone by reporting an empty
 set of boundaries.
Conversely, the trivial way to achieve blame completeness is to blame
 every boundary for every possible mismatch.
The real challenge is to satisfy both or implement a pragmatic tradeoff.

@exact|{
\definitionsketch{\textrm{blame soundness}}{
  For all reductions that end in a mismatch for value\/ $\svalue_0$
  blaming boundaries\/ $\sbset_0$,
  the names in\/ $\sbset_0$
  are a \textbf{\emph{subset}} of the labels on\/ $\svalue_0$.
}
}|

@exact|{
\definitionsketch{\textrm{blame completeness}}{
  For all reductions that end in a mismatch for value\/ $\svalue_0$
  blaming boundaries\/ $\sbset_0$,
  the names in\/ $\sbset_0$
  are a \textbf{\emph{superset}} of the labels on\/ $\svalue_0$.
}\smallskip
}|

The propagation laws above (@sectionref{sec:design:laws}) specify one way
 to manage ownership labels.
But other ground-truth strategies are possible,
 and may provide insights about semantics that fail to be blame-sound and
 blame-complete with the standard labeling.
As a case in point, the @|tname| semantics (@sectionref{sec:design:tech:transient}) uses heap addresses
 to allow mixed-typed interaction without wrapper expressions.
The evaluation of a function, for example, draws a fresh heap address @${\eloc_0}
 and stores the function on a value heap (@${\vstore}).

@exact|{
\begin{displayrrarray}
  \conf{(\efun{\svar_0}{\svar_0})}{\vstore_0}{\bstore_0}
  &  \nredTX
  &  \conf{\eloc_0}{(\eset{\vrecord{\eloc_0}{(\efun{\svar_0}{\svar_0})}} \cup \vstore_0)}{(\eset{\brecord{\eloc_0}{\semptymap}} \cup {\bstore_0})}\!\!\!\!
  \\\sidecond{where $\ffresh{\eloc_0}{\vstore_0\mbox{ and }\bstore_0}$}
\end{displayrrarray}
}|

@|noindent|When this function pointer @${\eloc_0} crosses a boundary,
 the semantics records the crossing on a blame heap (@${\bstore}).
The blame heap provides a set of boundaries if a type mismatch occurs,
 but this set is typically unsound because it conflates different
 pointers to the same value.
Propagating labels onto the heap, however, enables a conjecture
 that @|tname| blames only boundaries that are relevant to the address of the
 incompatible value.


@section[#:tag "sec:design:basic:preorder"]{Error Preorder}

Whereas the preceding properties characterize the semantics independently of each other,
 an @emph{error preorder relation} allows direct comparisons.
Strategies that perform many eager run-time checks have a lower position in
 the order.

One semantics lies below another in this preorder,
 written @${\xsym \sbehaviorle \ysym},
 if it raises errors on at least as many well-formed input expressions.
Put another way,
 @${\xsym{} \sbehaviorle \ysym{}}
 if and only if the latter reduces at least as many expressions to a result value.
When two semantics agree about which expressions raise run-time errors,
 the notation @${\xsym{} \sbehavioreq \ysym{}} shows that they lie below one
 another.

@exact|{
\definitionsketch{\textrm{error preorder }$\sbehaviorle$}{
  $\xsym \sbehaviorle \ysym$
  iff\/ $\eset{\sexpr_0 \mid \fexists{\svalue_0}{\sexpr_0 \rredX \svalue_0}} \subseteq \eset{\sexpr_1 \mid \fexists{\svalue_1}{\sexpr_1 \rredY \svalue_1}}$.
}
}|

@exact|{
\definitionsketch{\textrm{error equivalence }$\sbehavioreq$}{
  $\xsym \sbehavioreq \ysym$
  iff\/ $\xsym \sbehaviorle \ysym$
  and\/ $\ysym \sbehaviorle \xsym$.
}
\vspace{-4ex}
}|
