#lang greenman-thesis/include
@(require greenman-thesis/oopsla-2019/main)

@title[#:tag "sec:design:basic"]{Evaluation Framework}

This section introduces the basic ideas of the evaluation framework.
To formulate different type-enforcement stategies on an equal footing,
 the framework begins with one mixed-typed surface language (@sectionref{sec:design:basic:surface})
 and models stategies as distinct semantics (@sectionref{sec:design:semantic-framework}).
The properties listed above support an analysis.
Type soundness (@sectionref{sec:design:basic:ts}) and complete monitoring (@sectionref{sec:design:laws})
 characterize the type mismatches that a semantics detects.
Blame soundness and blame completeness (@sectionref{sec:design:ownership})
 measure the quality of error messages.
The error preorder (@sectionref{sec:design:basic:preorder}) enables direct comparisons.


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
Consequently, there is no type consistency judgment in the surface language.
All typed/untyped interaction must occur though boundary expressions.
To add a dynamic type, uses of that type must be translated to boundaries@~citep{afsw-popl-2011,svcb-snapl-2015}.
@; %Whether to include a dynamic type, what its behavior should be,
@; % and what flexibility it offers is a separate issue@~citep{svcb-snapl-2015,gct-popl-2016,nla-popl-2019}.

The core statically-typed (@${\svaluestat}) and dynamically-typed (@${\svaluedyn})
 values consist of integers, natural numbers, pairs, and functions.
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
  \end{langarray}
  \qquad\qquad
  \begin{langarray}
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
The details of the first two are standard, and are deferred to @sectionref{sec:design:technical}.
@; TODO awkward
Boundary expressions are the glue that enables mixed-typed programs.
A program starts with named chunks of code, called components.
Boundary expressions link these chunks together with a static type
 to describe the types of values that may cross the boundary.
Suppose that a typed component named @${\sowner_0} imports and applies an
 untyped function from component @${\sowner_1}:

@exact|{
\begin{equation}
  \label{eq:bnd:e0}
  \begin{minipage}{25mm}
    {\hfill\(\sowner_1\)~~}\\[-3mm]
    \begin{mdframed}[style=dynframestyle,userdefinedwidth=25mm]\(
      \efun{\svar_0}{\ssum~\svar_0~2}
    \)\end{mdframed}
  \end{minipage}\begin{minipage}{23mm}\begin{tikzpicture}
    \node (A) {};
    \node (B) [right of=A,xshift=3em] {};
    \draw[->] (A)
      edge [bend left]
      node [above] {\raisebox{0pt}[0pt]{$\tfun{\tnat}{\tnat}$}}
      node [below,yshift=-3mm] {\raisebox{0pt}[0pt]{$f$}}
      (B);
  \end{tikzpicture}\end{minipage}\begin{minipage}{9mm}
    {\hfill\(\sowner_0\)~~~}\\[-3mm]
    \begin{mdframed}[style=staframestyle,userdefinedwidth=9mm]\(f~9\)\end{mdframed}
  \end{minipage}
\end{equation}
}|

@exact{\noindent{}}The surface language can model the composition of these components with a boundary
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

@exact{\noindent{}}In turn, this two-component expression may be part of a larger
 untyped component.
The sketch below shows an untyped component in the center that imports
 two typed components:
 the expression @exact{\eqref{eq:bnd:e0}} from above (on the right),
 and a new typed function (on the left).

@exact|{
\begin{equation}
  \label{eq:bnd:e1}
  \begin{minipage}{36mm}
    {\hfill\(\sowner_3\)~~~}\\[-3mm]
    \begin{mdframed}[style=staframestyle,userdefinedwidth=36mm]\(
      \efun{\tann{\svar_1}{\tpair{\tint}{\tint}}}{\sfst~\svar_1}
    \)\end{mdframed}
  \end{minipage}\begin{minipage}{30mm}\begin{tikzpicture}
    \node (A) {};
    \node (B) [right of=A,xshift=5em] {};
    \node (vspace) [below of=A,yshift=7mm] {};
    \draw[->] (A)
      edge [bend left]
      node [above] {\raisebox{0pt}[0pt]{$\tfun{(\tpair{\tint}{\tint})}{\tint}$}}
      node [below,yshift=-2mm] {\raisebox{0pt}[0pt]{$g$}}
      (B);
  \end{tikzpicture}\end{minipage}\begin{minipage}{9mm}
    {\hfill\(\sowner_2\)~~}\\[-3mm]
    \begin{mdframed}[style=dynframestyle,userdefinedwidth=9mm]\(g~x\)\end{mdframed}
  \end{minipage}\begin{minipage}{16mm}\begin{tikzpicture}
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

@exact{\noindent{}}When linearized to the surface language, this term becomes:

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
It is up to a semantics to enforce the types in these boundaries.

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
      \textrm{\scountable{} set}
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
 (@${\sownerenv; \sowner \sWL \sexpr}).
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
Each semantics must have equivalent behavior on boundary-free expressions.
Fully-typed terms, for instance, must compute equivalent values.
Starting from this constraint, the choice of type-enforcement strategy
 guides the design for the rest.

The first ingredient of a semantics is the set of result values @${\svalue} that
 expressions may reduce to.
A result set typically extends the core typed and untyped values mentioned above
 (@${\svalue \supseteq \svaluestat \cup \svaluedyn}).
Potential reasons for the extended value set include the following:
@itemlist[#:style 'ordered
@item{
    to permit untyped values in typed code, and vice versa;
}@item{
    to track the identity of values on a heap;
}@item{ @exact{\label{val:monitor}}
    to associate a value with a delayed type-check for later uses; and
}@item{ @exact{\label{val:trace}}
    to record the boundaries that a value has previously crossed.
}]
Reasons @exact{\ref{val:monitor}} and @exact{\ref{val:trace}} introduce two kinds of
 wrapper value.
A guard wrapper, written @${\emon{\sbnd}{\svalue}}, associates a boundary
 specification with a value to achieve delayed type checks.
A trace wrapper, written @${\ehist{\sblist}{\svalue}}, attaches a list of
 boundaries to a value as metadata.
Guards are similar to boundary expressions; they separate a context
 component from a value component.
Trace wrappers simply annotate values.

@; %% TODO draw monitor ... but how? Need to illustrate evaluation ... maybe
@; %%  show "yes" and "no" steps
@; %% TODO discussion here

Second, a semantics must give reduction rules for boundary expressions.
These rules initiate a type-enforcement strategy.
For example, the @|nname| semantics (@sectionref{sec:design:tech:natural}) admits the
 following two reductions:

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
  & \emon{\obnd{\sowner_0}{(\tfun{\tint}{\tnat})}{\sowner_1}}{(\efun{\svar_0}{{-8}})}
\end{displayrrarray}
}|

@exact{\noindent{}}The first rule lets a typed number enter an untyped context.
The second rule gives typed code access to an untyped function through a newly-created
 guard wrapper.
Guard wrappers are a @emph{higher-order} mechanism for enforcing higher-order types.
As such, wrappers require elimination rules.
The @|nname| semantics includes the following rule to unfold
 the application of a typed, guarded function into two boundaries:

@exact|{
\begin{displayrrarray}
   \rrnum{c} \sapp~{(\emon{\obnd{\sowner_0}{(\tfun{\tint}{\tnat})}{\sowner_1}}{(\efun{\svar_0}{{-8}})})}~{1}
   & \nredNS
   \\\qquad\quad\zerowidth{\edynb{\obnd{\sowner_0}{\tnat}{\sowner_1}}{(\sapp~{(\efun{\svar_0}{{-8}})}~{(\estab{\obnd{\sowner_1}{\tint}{\sowner_0}}{1})})}}
\end{displayrrarray}
}|

@exact{\noindent{}}Other semantics have different behavior at boundaries and
 different supporting rules.
The @|tname| semantics (@sectionref{sec:design:tech:transient})
 takes a @emph{first-order} approach to boundaries.
Instead of using wrappers, it checks shapes at a boundary and guards later
 elimination forms with shape-check expressions.
For example, the following simplified reduction demonstrates a successful check:

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
\definitionsketch{$\sXproj{}$-type soundness}{
  \begin{minipage}[t]{0.50\columnwidth}
    If\/ $\sexpr_0$ has static type\/ $\stype_0$ (\/$\sWT \sexpr_0 : \stype_0$),\\
     then one of the following holds:
     \vspace{-1ex}
    \begin{itemize}
      \item
        $\sexpr_0$ reduces to a value\/ $\svalue_0$\\ and\/ $\sWTX \svalue_0 : \fXproj{\svalue_0}$
      \item
        $\sexpr_0$ reduces to an allowed error
      \item
        $\sexpr_0$ reduces endlessly
    \end{itemize}
  \end{minipage}\begin{minipage}[t]{0.46\columnwidth}
    If\/ $\sexpr_0$ is well-formed ($\sWT \sexpr_0 : \tdyn$),\\
     then one of the following holds:
     \vspace{-1ex}
    \begin{itemize}
      \item
        $\sexpr_0$ reduces to a value\/ $\svalue_0$\\ and\/ $\sWTX \svalue_0 : \tdyn$
      \item
        $\sexpr_0$ reduces to an allowed error
      \item
        $\sexpr_0$ reduces endlessly
    \end{itemize}
  \end{minipage}
}}|


@section[#:tag "sec:design:laws"]{Complete Monitoring}

@; %% ??? show examples of bad labels?
@; %% ??? give illustration for our model syntax, and that stat/dyn/G are the only boundaries?

Complete monitoring asks whether a mixed-typed semantics performs all the
 run-time checks necessary to ensure that interactions between typed and untyped
 code respect the type annotations in a program.
If the property holds, then a programmer can use types to predict behavior.
For example, a value that passes through the type @${(\tfun{\tint}{\tint})} is
 guaranteed to act like an integer function whether or not it is statically
 typed; if an untyped value misbehaves, then a run-time check halts
 the program.

Because all typed/untyped interactions take place at boundaries,
 a simplistic way to formalize complete monitoring is to ask whether each
 boundary comes with a full type check.
A language that meets this strict requirement certainly respects type
 annotations; however, other good designs fail.
Suppose typed code expects a pair of integers and a semantics
 admits any pair at the boundary and later checks that such pairs contain integers.
This lazy semantics eventually checks all type obligations, despite using
 a partial check at the boundary.
Higher-order values pose a similar challenge.
A single run-time check cannot prove that an untyped function always behaves
 a certain way; nevertheless, a language that carefully guards such values
 satisfies the high-level idea of complete monitoring.

Our statement of complete monitoring therefore introduces
 @emph{ownership labels} @~citep{dfff-popl-2011} to indirectly reach the goal.
An ownership label @$|{{}^\sowner_0}| names one component in the source code of a program.
Expressions and values can carry ownership labels---for example,
 @${\obars{42}{\sowner_0}}---to show the responsible components.
With labels as a syntactic tool, a complete monitoring theorem is
 two steps away.
First, a reduction relation @${\samplerred} must propagate labels to
 reflect communications and checks.
Second, the language requires a well-formedness judgment @${\sWL}
 to test whether every value in an expression has one unique owner.

@exact|{
\definitionsketch{complete monitoring}{
  For all\/ ${}\sWL \sexpr_0$,
  any reduction\/ $\sexpr_0 \samplerred \sexpr_1$
  implies\/ ${}\sWL \sexpr_1$
}\smallskip
}|

If @${\samplerred} starts from a base semantics and uses labels to track
 un-checked responsibilities, then the above theorem means that
 a raw value never enters typed code before discharging all its boundary
 obligations.


@subsection{How to lift a reduction relation}

The models in @sectionref{sec:design:technical} present six reduction relations.
Each one is the basis for a @emph{lifted} reduction relation that assigns
 the same behavior to labeled terms, but also propagates labels in a
 way that enables a meaningful complete monitoring theorem.
These lifted relations come about in a semi-automatic way
 according to the following informal laws (in the sense of natural laws).

Each law informally describes one way that labels may be transferred or dropped
 during evaluation.
To convey the general idea, each law comes with a brief illustration; namely,
 an example reduction and a short comment.
The example reductions use a hypothetical @${\samplerrarrow} relation.
Take the transitions (@${\sexpr\!\samplerrarrow\!\sexpr}) as given and focus on the labels.

@exact|{
 {\begin{enumerate}
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
       \samplerrarrow
       \obars{\obbars{\sfst~{\obbars{\thevalue}{\fconcat{\sowner_2}{\fconcat{\sowner_3}{\fconcat{\sowner_1}{\sowner_0}}}}}}{\fconcat{\sowner_0}{\sowner_1}}}{\sowner_3}$
    \subitem\hfill
      \emph{The argument value\/ $\epair{8}{6}$ is input to the function. The substituted body flows out}
    \subitem\hfill
      \emph{of the function, and by \lawref{law:pos} acquires the function's labels.}

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

To show how these laws inform the design of a lifted reduction relation,
 the following four rules show lifted variants of the examples
 from @sectionref{sec:design:semantic-framework}.
The first rule demonstrates a base-type boundary (@exact{\lawref{law:base}}).
The second demonstrates a higher-order boundary (\lawref{law:cross}); the
 new guard on the right-hand side implicitly inherits the context label.
The third rule sends an input (\lawref{law:neg}) and creates new application
 and boundary expressions.
The fourth rule applies \lawref{law:pos} for an output.

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
  \\[0.5ex]\qquad\quad\zerowidth{\obars{\edynb{\obnd{\sowner_0}{\tnat}{\sowner_1}}{\obars{\sapp~{\svalue_0}~{(\estab{\obnd{\sowner_1}{\tint}{\sowner_0}}{\obbars{1}{\fconcat{\sownerlist_4}{\fconcat{\sowner_5}{\frev{\sownerlist_3}}}}})}}{\sowner_2}}}{\sowner_5}}
\end{displayrrarray}
}|

@exact|{
\begin{displayrrarray}
  \rrnum{d'} \obars{\echecktwo{(\tpair{\tnat}{\tnat})}{\obbars{\epair{\obbars{{-1}}{\sownerlist_0}}{\obbars{{-2}}{\sownerlist_1}}}{\sownerlist_2}}{}}{\sowner_3}
  & \!\!\!\!\nredTX\!\!\!\! &
  \obbars{\epair{\obbars{{-1}}{\sownerlist_0}}{\obbars{{-2}}{\sownerlist_1}}}{\fconcat{\sownerlist_2}{\sowner_3}}\!\!\!\!\!
\end{displayrrarray}
}|

Ultimately, the design of a useful lifted reduction relation is a challenge
 for the language at hand.
The only certain rule is that lifting can add metadata but cannot change
 behavior.
Nevertheless, the laws in this section have been useful in our formalization
 and may prove useful in future work.


@section[#:tag "sec:design:ownership"]{Blame Soundness, Blame Completeness}

Blame soundness and blame completeness
 ask whether a semantics can identify the responsible parties
 in the event of a run-time type mismatch.
A type mismatch occurs when a typed context receives an unexpected value.
The value may be the result of a boundary expression or an elimination form,
 and the underlying issue may lie with either the value,
 the current type expectation, or some prior communication.
In any event, a programmer needs to know which components are responsible
 for the value to begin debugging.

Ownership labels provide a means to track responsibilities.
Suppose that a reduction blames the labeled value
 @${\obbars{\svalue_0}{\fconcat{\sowner_0}{\fconcat{\ldots}{\sowner_n}}}} for a type mismatch.
The programmer needs to know the component names that correspond to the labels.
The technical question is whether the sender names in these boundaries
 (the @${\sowner_1} in each @${\obnd{\sowner_0}{\stype_0}{\sowner_1}})
 match the ownership labels.
Blame soundness states that the sender names are a subset of the true labels.
Blame completeness guarantees a superset of the true labels.
Together, the two properties imply that a type mismatch reports the whole
 list of responsible components and nothing more.

A semantics can trivially satisfy blame soundness alone by reporting an empty
 set of boundaries.
Conversely, the trivial way to achieve blame completeness is to blame
 every boundary for every possible mismatch.
The real challenge is to satisfy both, or reach a pragmatic tradeoff.

@exact|{
\definitionsketch{blame soundness}{
  For all reductions that end in a mismatch for value\/ $\svalue_0$
  blaming boundaries\/ $\sbset_0$,
  the senders in\/ $\sbset_0$
  are a \textbf{\emph{subset}} of the labels on\/ $\svalue_0$.
}
}|

@exact|{
\definitionsketch{blame completeness}{
  For all reductions that end in a mismatch for value\/ $\svalue_0$
  blaming boundaries\/ $\sbset_0$,
  the senders in\/ $\sbset_0$
  are a \textbf{\emph{superset}} of the labels on\/ $\svalue_0$.
}\smallskip
}|

The propagation laws above (@sectionref{sec:design:laws}) specify one way
 to manage ownership labels.
But other ground-truth strategies are possible,
 and may provide insights about semantics that fail to be blame-sound and
 blame-complete with the standard labeling.

As a case in point, the upcoming @|tname| semantics uses heap addresses
 to allow mixed-typed interaction without wrapper expressions.
The evaluation of a function, for example, draws a fresh heap address @${\eloc_0}
 and stores the function on a value heap (@${\vstore}).

@exact|{
\begin{displayrrarray}
  \conf{(\efun{\svar_0}{\svar_0})}{\vstore_0}{\bstore_0}
  &  \nredTX
  &  \conf{\eloc_0}{(\eset{\vrecord{\eloc_0}{(\efun{\svar_0}{\svar_0})}} \cup \vstore_0)}{(\eset{\brecord{\eloc_0}{\semptymap}} \cup {\bstore_0})}
  \\\sidecond{where $\ffresh{\eloc_0}{\vstore_0\mbox{ and }\bstore_0}$}
\end{displayrrarray}
}|

@exact{\noindent{}}When this function pointer @${\eloc_0} crosses a boundary,
 the semantics records the crossing on a blame heap (@${\bstore}).
The blame heap provides a set of boundaries if a type mismatch occurs,
 but this set is typically unsound because it conflates different
 pointers to the same value.
Propagating labels onto the heap, however, enables a conjecture
 that @|tname| blames only boundaries that are relevant to the address of the
 incompatible value.

@; %% TODO transient heap, check, fail rule?
@; %% ... not sure anymore if we need to show them


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
\definitionsketch{error preorder $\sbehaviorle$}{
  $\xsym \sbehaviorle \ysym$
  iff\/ $\eset{\sexpr_0 \mid \fexists{\svalue_0}{\sexpr_0 \rredX \svalue_0}} \subseteq \eset{\sexpr_1 \mid \fexists{\svalue_1}{\sexpr_1 \rredY \svalue_1}}$.
}
}|

@exact|{
\definitionsketch{error equivalence $\sbehavioreq$)}{
  $\xsym \sbehavioreq \ysym$
  iff\/ $\xsym \sbehaviorle \ysym$
  and\/ $\ysym \sbehaviorle \xsym$.
}
}|

