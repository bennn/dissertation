#lang greenman-thesis/include

@; TODO
@; - Might be able to prove a ``tag error'' lemma, but the elimination forms
@;    currently don't tell between static-typed and untyped
@; - colors for boundary spiderweb picture
@; - ....

@(require
   (prefix-in tr: greenman-thesis/jfp-2019/main)
   (only-in greenman-thesis/shallow/main
     SHALLOW-CURRENT-BENCHMARK*
     get-mixed-path-table
     render-mixed-path-table
     get-mixed-worst-table
     render-mixed-worst-table
     find-lowest-3dpath-D
     get-3d-table
     s:cache-dir)
   (only-in greenman-thesis/oopsla-2019/pict
     both:model-interaction
     both:DS0
     both:DS1
     both:any-wrap
     both:no-wrap
     both:index-of
     untyped-codeblock
     typed-codeblock)
   (only-in gtp-plot/performance-info
     performance-info->num-units)
   (only-in math/statistics
     mean))

@title[#:tag "chap:both"]{@|sDeep| and @|sShallow|, Combined}

This chapter validates the central point of my thesis: that @|sdeep| and @|sshallow| types
 can be combined in a companion language for Racket, and the combination is an improvement
 over either one alone.
First, I prove that @|sdeep| types via @|snatural| and @|sshallow| types
 via @|stransient| can coexist in a formal model.
The two semantics can interoperate without changing the formal properties
 of either one (@sectionref{sec:both:model}).
Second, I report challenges that arose combining @|sDeep| Racket and
 @|sShallow| Racket in a single implementation (@sectionref{sec:both:implementation}).
Overall, the combined implementation has clear benefits (@sectionref{sec:both:evaluation}).
Programmers are better off with a choice of @|sdeep| guarantees
 and @|stransient| performance.
Combining the two semantics in one program can further improve performance.
And, surprisingly, the addition of @|sshallow| types can express programs
 that @|sDeep| Racket currently cannot.

A downside of the combination is that @|snatural| and @|stransient| cannot
 easily share the results of their type checks.
The reason is simple: @|stransient| as-is lacks a way of learning from past checks.
@Sectionref{sec:future:nonopt} explains the synergy challenge in terms of the
 model and outlines implementation techniques that may get around the issue.


@section[#:tag "sec:both:model"]{Model and Properties}

The model combines @|sdeep|-typed code, @|sshallow|-typed code, and
 untyped code in one surface language.
Each of these three disciplines is recognized by a surface-typing
 judgment and comes with a complier.
The three compilers translate well-typed code to a common evaluation
 syntax that has one untyped semantics.

Although the three varieties of surface code give rise to six kinds of
 interactions, the model keeps these interactions under control with
 only three kinds of run-time boundaries (@figureref{fig:both:base-interaction}).
A @emph[swrap] boundary inserts a higher-order check to support @|sdeep|
 types.
A @emph[sscan] boundary validates a top-level shape for @|sshallow| code.
Lastly, a @emph[snoop] boundary does nothing.
@Sectionref{sec:both:model:theorems} proves that these checks are strong enough to realize
 @|sshallow| types that satisfy shape-soundness and @|sdeep| types that
 satisfy complete monitoring.


@figure*[
  "fig:both:base-interaction"
  @elem{@|sDeep|, @|sShallow|, and untyped interactions.}
  both:model-interaction]


@subsection[#:tag "sec:both:model:syntax"]{Syntax}

@figure*[
  "fig:both:surface"
  @elem{Surface syntax}

  @exact|{
\begin{langarray}
  \ssurface & \slangeq &
    \svar \mid \sint \mid \epair{\ssurface}{\ssurface} \mid
  \\ & &
    \efun{\svar}{\ssurface}
    \mid \efun{\tann{\svar}{\stype}}{\ssurface}
    \mid \efun{\tann{\svar}{\tfloor{\stype}}}{\ssurface} \mid
  \\ & &
    \eunop{\ssurface} \mid \ebinop{\ssurface}{\ssurface} \mid
    \eappu{\ssurface}{\ssurface} \mid
  \\ & &
    \emod{\slang}{\ssurface}
  \\
  \stype & \slangeq &
    \tnat \mid \tint \mid \tpair{\stype}{\stype} \mid \tfun{\stype}{\stype}
  \\
  \slang & \slangeq &
    \sD \mid \sS \mid \sU
  \\
  \stspec & \slangeq &
    \stype \mid \tfloor{\stype} \mid \tdyn
\end{langarray}
}|]

The surface syntax begins with simple expressions and adds module boundaries
 to enable a three-way interpretation.
The simple expressions are function application (@${\eappu{\ssurface}{\ssurface}}),
 primitive operation application (@${\eunop{\ssurface}}, @${\ebinop{\ssurface}{\ssurface}}),
 variables (@${\svar}),
 integers (@${\sint}),
 pairs (@${\epair{\ssurface}{\ssurface}}),
 and functions.
Functions come in three flavors:
 an untyped function has no type annotation (@${\efun{\svar}{\ssurface}}),
 a @|sdeep|-typed function has a type annotation (@${\efun{\tann{\svar}{\stype}}{\ssurface}}),
 and a @|sshallow|-typed function has an underlined type annotation (@${\efun{\tann{\svar}{\tfloor{\stype}}}{\ssurface}}).
The underline mark simplifies proofs, and serves as a hint to readers that
 only the top-level shape of this type is guaranteed at run-time.
It is @emph{not} a meta-function.
Types (@${\stype}) express natural numbers (@${\tnat}),
 integers (@${\tint}),
 pairs (@${\tpair{\stype}{\stype}}),
 and functions (@${\tfun{\stype}{\stype}}).

Module-boundary expressions declare the intent of the code within them.
For example, the term @${\emod{\sD}{\ssurface_0}} asks for @|sdeep| types
 in expression @${\ssurface_0} by default.
If another boundary appears within @${\ssurface_0}, then its language flag
 (@${\sD}, @${\sS}, or @${\sU}) sets a new default.

Note that module boundaries are similar to the boundary expressions
 from @chapter-ref{chap:design}.
Instead of adding a third boundary term to split the old @${\sstat} boundaries
 into @|sdeep| and @|sshallow| versions, the present model uses one
 parameterized term.
Both the old and new boundary terms correspond to module boundaries in a
 realistic language.


@subsection[#:tag "sec:both:model:types"]{Surface Typing}

In principle, the surface language comes with three typing judgments
 to recognize @|sdeep|, @|sshallow|, and untyped code.
These judgments are mutually recursive at module-boundary terms.
To keep things simple, however, @figure-ref{fig:both:surface-type}
 presents one judgment (@${\stypeenv \sST \sexpr : \stspec})
 that supports three possible conclusions.
A conclusion (@${\stspec}) is one of:
 the uni-type @${\tdyn} of untyped code,
 a type @${\stype} for @|sdeep|-typed code,
 or a decorated type @${\tfloor{\stype}} for @|sshallow| code.
The notation is again a hint.
A decorated type is equal to a normal type during static type checking,
 but makes a weaker statement about program behavior.

The typing rules are relatively simple, but declarative.
The rules for modules, for example, give no hint about how to find a type
 conclusion that fits the rest of the program.
A second notably aspect is that one module may contain another with
 the same language flag.

@Figureref{fig:both:extra-type} defines a
 subtyping judgment (@${\ssubt}) and a type-assignment for primitive
 operations (@${\sDelta}).
These are both standard.


@figure*[
  "fig:both:surface-type"
  @elem{Surface typing judgment (selected rules)}

@exact|{
\begin{mathpar}
  \inferrule*{
    \tann{\svar_0}{\tdyn} \in \stypeenv_0
  }{
    \stypeenv_0 \sST \svar_0 : \tdyn
  }

  \inferrule*{
    \tann{\svar_0}{\stype_0} \in \stypeenv_0
  }{
    \stypeenv_0 \sST \svar_0 : \stype_0
  }

  \inferrule*{
    \tann{\svar_0}{\tfloor{\stype_0}} \in \stypeenv_0
  }{
    \stypeenv_0 \sST \svar_0 : \tfloor{\stype_0}
  }

  \inferrule*{
  }{
    \stypeenv_0 \sST \sint_0 : \tdyn
  }

  \inferrule*{
  }{
    \stypeenv_0 \sST \snat_0 : \tnat
  }

  \inferrule*{
  }{
    \stypeenv_0 \sST \snat_0 : \tfloor{\tnat}
  }

  \inferrule*{
  }{
    \stypeenv_0 \sST \sint_0 : \tint
  }

  \inferrule*{
  }{
    \stypeenv_0 \sST \sint_0 : \tfloor{\tint}
  }

  \inferrule*{
    \stypeenv_0 \sST \ssurface_0 : \tdyn
    \\
    \stypeenv_0 \sST \ssurface_1 : \tdyn
  }{
    \stypeenv_0 \sST \epair{\ssurface_0}{\ssurface_1} : \tdyn
  }

%  \inferrule*{
%    \stypeenv_0 \sST \ssurface_0 : \stype_0
%    \\
%    \stypeenv_0 \sST \ssurface_1 : \stype_1
%  }{
%    \stypeenv_0 \sST \epair{\ssurface_0}{\ssurface_1} : \tpair{\stype_0}{\stype_1}
%  }
%
%  \inferrule*{
%    \stypeenv_0 \sST \ssurface_0 : \tfloor{\stype_0}
%    \\
%    \stypeenv_0 \sST \ssurface_1 : \tfloor{\stype_1}
%  }{
%    \stypeenv_0 \sST \epair{\ssurface_0}{\ssurface_1} : \tfloor{\tpair{\stype_0}{\stype_1}}
%  }
%
  \inferrule*{
    \fcons{\tann{\svar_0}{\tdyn}}{\stypeenv_0} \sST \sexpr_0 : \tdyn
  }{
    \stypeenv_0 \sST \efun{\svar_0}{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \fcons{\tann{\svar_0}{\stype_0}}{\stypeenv_0} \sST \sexpr_0 : \stype_1
  }{
    \stypeenv_0 \sST \efun{\tann{\svar_0}{\stype_0}}{\sexpr_0} : \tfun{\stype_0}{\stype_1}
  }

  \inferrule*{
    \fcons{\tann{\svar_0}{\tfloor{\stype_0}}}{\stypeenv_0} \sST \sexpr_0 : \tfloor{\stype_1}
  }{
    \stypeenv_0 \sST \efun{\tann{\svar_0}{\stype_0}}{\sexpr_0} : \tfloor{\tfun{\stype_0}{\stype_1}}
  }

%  \inferrule*{
%    \stypeenv_0 \sST \ssurface_0 : \tdyn
%  }{
%    \stypeenv_0 \sST \eunop{\ssurface_0} : \tdyn
%  }
%
  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \stype_0
    \\\\
    \sDelta(\sunop, \stype_0) = \stype_1
  }{
    \stypeenv_0 \sST \eunop{\sexpr_0} : \stype_1
  }

%  \inferrule*{
%    \stypeenv_0 \sST \sexpr_0 : \tfloor{\stype_0}
%    \\
%    \sDelta(\sunop, \stype_0) = \stype_1
%  }{
%    \stypeenv_0 \sST \eunop{\sexpr_0} : \tfloor{\stype_1}
%  }
%
%  \inferrule*{
%    \stypeenv_0 \sST \ssurface_0 : \tdyn
%    \\
%    \stypeenv_0 \sST \ssurface_1 : \tdyn
%  }{
%    \stypeenv_0 \sST \ebinop{\ssurface_0}{\ssurface_1} : \tdyn
%  }
%
%  \inferrule*{
%    \stypeenv_0 \sST \sexpr_0 : \stype_0
%    \\
%    \stypeenv_0 \sST \sexpr_1 : \stype_1
%    \\\\
%    \sDelta(\sbinop, \stype_0, \stype_1) = \stype_2
%  }{
%    \stypeenv_0 \sST \ebinop{\sexpr_0}{\sexpr_1} : \stype_2
%  }
%
  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \tfloor{\stype_0}
    \\
    \stypeenv_0 \sST \sexpr_1 : \tfloor{\stype_1}
    \\\\
    \sDelta(\sbinop, \stype_0, \stype_1) = \stype_2
  }{
    \stypeenv_0 \sST \ebinop{\sexpr_0}{\sexpr_1} : \tfloor{\stype_2}
  }

%  \inferrule*{
%    \stypeenv_0 \sST \ssurface_0 : \tdyn
%    \\
%    \stypeenv_0 \sST \ssurface_1 : \tdyn
%  }{
%    \stypeenv_0 \sST \eapp{\ssurface_0}{\ssurface_1} : \tdyn
%  }
%
%  \inferrule*{
%    \stypeenv_0 \sST \sexpr_0 : \tfun{\stype_0}{\stype_1}
%    \\
%    \stypeenv_0 \sST \sexpr_1 : \stype_0
%  }{
%    \stypeenv_0 \sST \eapp{\sexpr_0}{\sexpr_1} : \stype_1
%  }
%
  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \tfloor{\tfun{\stype_0}{\stype_1}}
    \\
    \stypeenv_0 \sST \sexpr_1 : \tfloor{\stype_0}
  }{
    \stypeenv_0 \sST \eapp{\sexpr_0}{\sexpr_1} : \tfloor{\stype_1}
  }

%  \inferrule*{
%    \stypeenv_0 \sST \sexpr_0 : \stype_0
%    \\
%    \fsubt{\stype_0}{\stype_1}
%  }{
%    \stypeenv_0 \sST \sexpr_0 : \stype_1
%  }
%
  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \tfloor{\stype_0}
    \\
    \fsubt{\stype_0}{\stype_1}
  }{
    \stypeenv_0 \sST \sexpr_0 : \tfloor{\stype_1}
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \tdyn
  }{
    \stypeenv_0 \sST \emodule{\sulang}{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \stype_0
  }{
    \stypeenv_0 \sST \emodule{\stlang}{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \tfloor{\stype_0}
  }{
    \stypeenv_0 \sST \emodule{\sslang}{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \tdyn
  }{
    \stypeenv_0 \sST \emodule{\sulang}{\sexpr_0} : \stype_0
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \stype_0
  }{
    \stypeenv_0 \sST \emodule{\stlang}{\sexpr_0} : \stype_0
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \tfloor{\stype_0}
  }{
    \stypeenv_0 \sST \emodule{\sslang}{\sexpr_0} : \stype_0
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \tdyn
  }{
    \stypeenv_0 \sST \emodule{\sulang}{\sexpr_0} : \tfloor{\stype_0}
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \stype_0
  }{
    \stypeenv_0 \sST \emodule{\stlang}{\sexpr_0} : \tfloor{\stype_0}
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \tfloor{\stype_0}
  }{
    \stypeenv_0 \sST \emodule{\sslang}{\sexpr_0} : \tfloor{\stype_0}
  }
\end{mathpar}
}|]

@figure*[
  "fig:both:extra-type"
  @elem{Subtyping and primitive op. typing}

  @exact|{
\lbl{\fbox{$\fsubt{\stype}{\stype}$}}{\begin{mathpar}
  \inferrule*{
  }{
    \fsubt{\tnat}{\tint}
  }

  \inferrule*{
    \fsubt{\stype_0}{\stype_2}
    \\
    \fsubt{\stype_1}{\stype_3}
  }{
    \fsubt{\tpair{\stype_0}{\stype_1}}{\tpair{\stype_2}{\stype_3}}
  }

  \inferrule*{
    \fsubt{\stype_2}{\stype_0}
    \\
    \fsubt{\stype_1}{\stype_3}
  }{
    \fsubt{\tfun{\stype_0}{\stype_1}}{\tfun{\stype_2}{\stype_3}}
  }
\end{mathpar}}

\begin{minipage}[t]{0.5\columnwidth}
\lbl{\fbox{$\sDelta : \ffun{\tpair{\sunop}{\stype}}{\stype}$}}{
  \begin{langarray}
    \sDelta(\sfst, \tpair{\stype_0}{\stype_1}) & \feq & \stype_0
  \\
    \sDelta(\ssnd, \tpair{\stype_0}{\stype_1}) & \feq & \stype_1
  \end{langarray}
}

\end{minipage}\begin{minipage}[t]{0.5\columnwidth}
\lbl{\fbox{$\sDelta : \ffun{\tpair{\sbinop}{\tpair{\stype}{\stype}}}{\stype}$}}{
  \begin{langarray}
    \sDelta(\ssum, \tnat, \tnat) & \feq & \tnat
  \\
    \sDelta(\ssum, \tint, \tint) & \feq & \tint
  \\
    \sDelta(\squotient, \tnat, \tnat) & \feq & \tnat
  \\
    \sDelta(\squotient, \tint, \tint) & \feq & \tint
  \end{langarray}
}
\end{minipage}
}|]


@subsection[#:tag "sec:both:model:eval-syntax"]{Evaluation Syntax}

The evaluation syntax removes the declarative parts of the surface syntax
 and adds tools for enforcing types.
First to go are the module boundary expressions, which express a desire
 for a style of type enforcement.
Instead, the evaluation syntax has three kinds of run-time check expression:
 a @|swrap| boundary fully enforces a type, perhaps with a guard wrapper (@${\emon{\stype}{\svalue}});
 a @|sscan| boundary checks a type-shape (@${\sshape}),
 and a @|snoop| boundary checks nothing.
Second, the @|sshallow|-typed functions from the surface syntax (@${\efun{\tann{\svar}{\tfloor{\stype}}}{\ssurface}})
 are replaced with shape-annotated functions (@${\efun{\tann{\svar}{\sshape}}{\sexpr}}).
Type-shapes (@${\sshape}) express the outermost constructor of a type;
 the weakened function annotation reflects a weakened run-time guarantee.

The evaluation syntax also includes values (@${\svalue}),
 errors (@${\serror}),
 and evaluation contexts (@${\sctx}).
Running a program may produce a value or an error.
For the most part, the different errors come from boundaries.
A @${\swraperror} arises when a @|swrap| boundary receives invalid input,
 a @${\sscanerror} comes from a failed @|sscan|,
 and a @${\sdivzeroerror} occurs when a primitive operation rejects its input.
The final error, @${\stagerror}, is the result of a malformed term that cannot
 reduce further.
Such errors can easily occur in untyped code without any boundaries;
 for instance, the application of a number (@${\eappu{2~}{4}}) signals a tag error.
Reduction in typed code, whether @|sdeep| or @|sshallow|, should never raise a
 tag error.

@figure*[
  "fig:both:eval-syntax"
  @elem{Evaluation Syntax}

  @exact|{
\begin{langarray}
  \sexpr & \slangeq &
    \svar \mid \svalue \mid \epair{\sexpr}{\sexpr}
    \mid \eunop{\sexpr} \mid
    \ebinop{\sexpr}{\sexpr} \mid \eappu{\sexpr}{\sexpr} \mid \serror \mid
  \\ & &
    \ewrap{\stype}{\sexpr}
    \mid \escan{\sshape}{\sexpr}
    \mid \enoop{\sexpr}
  \\
  \svalue & \slangeq &
    \sint \mid \epair{\svalue}{\svalue}
    \mid \efun{\svar}{\sexpr}
    \mid \efun{\tann{\svar}{\stype}}{\sexpr}
    \mid \efun{\tann{\svar}{\sshape}}{\sexpr}
    \mid \emon{\stype}{\svalue}
  \\
  \sshape & \slangeq &
    \knat \mid \kint \mid \kpair \mid \kfun \mid \kany
  \\
  \serror & \slangeq &
    \swraperror \mid \sscanerror \mid \sdivzeroerror \mid \stagerror
  \\
  \sctx & \slangeq &
    \sctxhole \mid \eunop{\sctx} \mid \ebinop{\sctx}{\sexpr} \mid \ebinop{\svalue}{\sctx}
    \mid \eappu{\sctx}{\sexpr} \mid \eappu{\svalue}{\sctx} \mid
  \\ & &
    \enoop{\sctx} \mid \escan{\sshape}{\sctx} \mid \ewrap{\stype}{\sctx}
\end{langarray}
}|]


@subsection[#:tag "sec:both:model:eval-types"]{Evaluation Typing}

The evaluation syntax comes with three typing judgments that describe the
 run-time invariants of @|sdeep|, @|sshallow|, and @|suntyped| code.
The @|sdeep| typing judgment (@${\sWTT}) validates full types.
The @|sshallow| judgment (@${\sWTS}) checks top-level shapes.
In this judgment, elimination forms have a catch-all shape (@${\kany}) because
 they can produce any value at run-time;
 these must appear within a @|sscan| expression to guarantee a non-trivial
 shape.
Lastly, the @|suntyped| judgment (@${\sWTU}) guarantees no free variables.

@figure*[
  "fig:both:deep-type"
  @elem{@|sDeep| typing judgment}

@exact|{
\begin{mathpar}
  \inferrule*{
    \tann{\svar_0}{\stype_0} \in \stypeenv_0
  }{
    \stypeenv_0 \sWTT \svar_0 : \stype_0
  }

  \inferrule*{
  }{
    \stypeenv_0 \sWTT \snat_0 : \tnat
  }

  \inferrule*{
  }{
    \stypeenv_0 \sWTT \sint_0 : \tint
  }

  \inferrule*{
    \stypeenv_0 \sWTT \sexpr_0 : \stype_0
    \\
    \stypeenv_0 \sWTT \sexpr_1 : \stype_1
  }{
    \stypeenv_0 \sWTT \epair{\sexpr_0}{\sexpr_1} : \tpair{\stype_0}{\stype_1}
  }

  \inferrule*{
    \fcons{\tann{\svar_0}{\stype_0}}{\stypeenv_0} \sWTT \sexpr_0 : \stype_1
  }{
    \stypeenv_0 \sWTT \efun{\tann{\svar_0}{\stype_0}}{\sexpr_0} : \tfun{\stype_0}{\stype_1}
  }

  \inferrule*{
    \stypeenv_0 \sWTU \svalue_0 : \tdyn
  }{
    \stypeenv_0 \sWTT \emon{\stype_0}{\svalue_0} : \stype_0
  }

  \inferrule*{
    \stypeenv_0 \sWTS \svalue_0 : \sshape_0
  }{
    \stypeenv_0 \sWTT \emon{\stype_0}{\svalue_0} : \stype_0
  }

  \inferrule*{
    \stypeenv_0 \sWTT \sexpr_0 : \stype_0
    \\\\
    \sDelta(\sunop, \stype_0) = \stype_1
  }{
    \stypeenv_0 \sWTT \eunop{\sexpr_0} : \stype_1
  }

  \inferrule*{
    \stypeenv_0 \sWTT \sexpr_0 : \stype_0
    \\
    \stypeenv_0 \sWTT \sexpr_1 : \stype_1
    \\\\
    \sDelta(\sbinop, \stype_0, \stype_1) = \stype_2
  }{
    \stypeenv_0 \sWTT \ebinop{\sexpr_0}{\sexpr_1} : \stype_2
  }

  \inferrule*{
    \stypeenv_0 \sWTT \sexpr_0 : \tfun{\stype_0}{\stype_1}
    \\
    \stypeenv_0 \sWTT \sexpr_1 : \stype_0
  }{
    \stypeenv_0 \sWTT \eappu{\sexpr_0}{\sexpr_1} : \stype_1
  }

  \inferrule*{
    \stypeenv_0 \sWTT \sexpr_0 : \stype_0
  }{
    \stypeenv_0 \sWTT \enoop{\sexpr_0} : \stype_0
  }

  \inferrule*{
    \stypeenv_0 \sWTU \sexpr_0 : \tdyn
  }{
    \stypeenv_0 \sWTT \ewrap{\stype_0}{\sexpr_0} : \stype_0
  }

  \inferrule*{
    \stypeenv_0 \sWTS \sexpr_0 : \sshape_0
  }{
    \stypeenv_0 \sWTT \ewrap{\stype_0}{\sexpr_0} : \stype_0
  }

  \inferrule*{
    \stypeenv_0 \sWTT \sexpr_0 : \stype_0
    \\
    \fsubt{\stype_0}{\stype_1}
  }{
    \stypeenv_0 \sWTT \sexpr_0 : \stype_1
  }

  \inferrule*{
  }{
    \stypeenv_0 \sWTT \serror : \stype_0
  }
\end{mathpar}
}|]

@figure*[
  "fig:both:shallow-type"
  @elem{@|sShallow| typing judgment, subtyping, and shape map}

@exact|{
\begin{mathpar}
  \inferrule*{
    \tann{\svar_0}{\sshape_0} \in \stypeenv_0
  }{
    \stypeenv_0 \sWTS \svar_0 : \sshape_0
  }

  \inferrule*{
  }{
    \stypeenv_0 \sWTS \snat_0 : \tnat
  }

  \inferrule*{
  }{
    \stypeenv_0 \sWTS \sint_0 : \tint
  }

  \inferrule*{
    \stypeenv_0 \sWTS \sexpr_0 : \sshape_0
    \\
    \stypeenv_0 \sWTS \sexpr_1 : \sshape_1
  }{
    \stypeenv_0 \sWTS \epair{\sexpr_0}{\sexpr_1} : \kpair
  }

  \inferrule*{
    \fcons{\tann{\svar_0}{\tdyn}}{\stypeenv_0} \sWTU \sexpr_0 : \tdyn
  }{
    \stypeenv_0 \sWTS \efun{\svar_0}{\sexpr_0} : \kfun
  }

  \inferrule*{
    \fcons{\tann{\svar_0}{\sshape_0}}{\stypeenv_0} \sWTS \sexpr_0 : \sshape_1
  }{
    \stypeenv_0 \sWTS \efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0} : \kfun
  }

  \inferrule*{
    \stypeenv_0 \sWTT \svalue_0 : \stype_0
  }{
    \stypeenv_0 \sWTS \emon{\stype_0}{\svalue_0} : \kfun
  }

  \inferrule*{
    \stypeenv_0 \sWTS \sexpr_0 : \sshape_0
  }{
    \stypeenv_0 \sWTS \eunop{\sexpr_0} : \kany
  }

  \inferrule*{
    \stypeenv_0 \sWTS \sexpr_0 : \sshape_0
    \\
    \stypeenv_0 \sWTS \sexpr_1 : \sshape_1
  }{
    \stypeenv_0 \sWTS \ebinop{\sexpr_0}{\sexpr_1} : \kany
  }

  \inferrule*{
    \stypeenv_0 \sWTS \sexpr_0 : \kfun
    \\
    \stypeenv_0 \sWTS \sexpr_1 : \sshape_0
  }{
    \stypeenv_0 \sWTS \eappu{\sexpr_0}{\sexpr_1} : \kany
  }

  \inferrule*{
    \stypeenv_0 \sWTS \sexpr_0 : \sshape_0
  }{
    \stypeenv_0 \sWTS \enoop{\sexpr_0} : \sshape_0
  }

  \inferrule*{
    %% TODO why need this???
    \stypeenv_0 \sWTU \sexpr_0 : \tdyn
  }{
    \stypeenv_0 \sWTS \enoop{\sexpr_0} : \kany
  }

  \inferrule*{
    \stypeenv_0 \sWTU \sexpr_0 : \tdyn
  }{
    \stypeenv_0 \sWTS \escan{\sshape_0}{\sexpr_0} : \sshape_0
  }

  \inferrule*{
    \stypeenv_0 \sWTS \sexpr_0 : \sshape_1
  }{
    \stypeenv_0 \sWTS \escan{\sshape_0}{\sexpr_0} : \sshape_0
  }

  \inferrule*{
    \stypeenv_0 \sWTT \sexpr_0 : \stype_0
    \\
    \fshape{\stype_0} = \sshape_0
  }{
    \stypeenv_0 \sWTS \ewrap{\stype_0}{\sexpr_0} : \sshape_0
  }

  \inferrule*{
    \stypeenv_0 \sWTS \sexpr_0 : \sshape_0
    \\
    \fsubt{\sshape_0}{\sshape_1}
  }{
    \stypeenv_0 \sWTS \sexpr_0 : \sshape_1
  }

  \inferrule*{
  }{
    \stypeenv_0 \sWTS \serror : \sshape_0
  }
\end{mathpar}

\lbl{\fbox{$\fsubt{\sshape}{\sshape}$}}{\begin{mathpar}
  \inferrule*{
  }{
    \fsubt{\knat}{\kint}
  }

  \inferrule*{
  }{
    \fsubt{\sshape_0}{\kany}
  }
\end{mathpar}}

\lbl{\fbox{$\sshapecheck : \ffun{\stype}{\sshape}$}}{
  \begin{langarray}
    \fshape{\tnat} & \feq & \knat
  \\
    \fshape{\tint} & \feq & \kint
  \\
    \fshape{\tpair{\stype_0}{\stype_1}} & \feq & \kpair
  \\
    \fshape{\tfun{\stype_0}{\stype_1}} & \feq & \kfun
  \end{langarray}
}
}|]


@figure*[
  "fig:both:untyped-type"
  @elem{Untyped typing judgment (dynamic typing)}

@exact|{
\begin{mathpar}
  \inferrule*{
    \tann{\svar_0}{\tdyn} \in \stypeenv_0
  }{
    \stypeenv_0 \sWTU \svar_0 : \tdyn
  }

  \inferrule*{
  }{
    \stypeenv_0 \sWTU \sint_0 : \tdyn
  }

  \inferrule*{
    \stypeenv_0 \sWTU \sexpr_0 : \tdyn
    \\
    \stypeenv_0 \sWTU \sexpr_1 : \tdyn
  }{
    \stypeenv_0 \sWTU \epair{\sexpr_0}{\sexpr_1} : \tdyn
  }

  \inferrule*{
    \fcons{\tann{\svar_0}{\tdyn}}{\stypeenv_0} \sWTU \sexpr_0 : \tdyn
  }{
    \stypeenv_0 \sWTU \efun{\svar_0}{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \fcons{\tann{\svar_0}{\sshape_0}}{\stypeenv_0} \sWTS \sexpr_0 : \sshape_1
  }{
    \stypeenv_0 \sWTU \efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \stypeenv_0 \sWTT \svalue_0 : \stype_0
  }{
    \stypeenv_0 \sWTU \emon{\stype_0}{\svalue_0} : \tdyn
  }

  \inferrule*{
    \stypeenv_0 \sWTU \sexpr_0 : \tdyn
  }{
    \stypeenv_0 \sWTU \eunop{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \stypeenv_0 \sWTU \sexpr_0 : \tdyn
    \\
    \stypeenv_0 \sWTU \sexpr_1 : \tdyn
  }{
    \stypeenv_0 \sWTU \ebinop{\sexpr_0}{\sexpr_1} : \tdyn
  }

  \inferrule*{
    \stypeenv_0 \sWTU \sexpr_0 : \tdyn
    \\
    \stypeenv_0 \sWTU \sexpr_1 : \tdyn
  }{
    \stypeenv_0 \sWTU \eappu{\sexpr_0}{\sexpr_1} : \tdyn
  }

  \inferrule*{
    \stypeenv_0 \sWTU \sexpr_0 : \tdyn
  }{
    \stypeenv_0 \sWTU \enoop{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \stypeenv_0 \sWTS \sexpr_0 : \sshape_0
  }{
    \stypeenv_0 \sWTU \enoop{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \stypeenv_0 \sWTU \sexpr_0 : \tdyn
  }{
    \stypeenv_0 \sWTU \escan{\sshape_0}{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \stypeenv_0 \sWTS \sexpr_0 : \sshape_1
  }{
    \stypeenv_0 \sWTU \escan{\sshape_0}{\sexpr_0} : \tdyn
  }

  \inferrule*{
    \stypeenv_0 \sWTT \sexpr_0 : \stype_0
  }{
    \stypeenv_0 \sWTU \ewrap{\stype_0}{\sexpr_0} : \tdyn
  }

  \inferrule*{
  }{
    \stypeenv_0 \sWTU \serror : \tdyn
  }
\end{mathpar}
}|]


@subsection[#:tag "sec:both:model:completion"]{Completion}

A completion pass links the surface and evaluation syntaxes.
The basic goal is to translate module boundaries to appropriate run-time
 checks, but other terms may require checks as well.
Formally, the goal is to map all well-typed surface expressions to well-typed
 evaluation expressions (@exact{\lemmaref{lemma:both:completion}}).
The completion rules shown in @figureref["fig:both:completion1" "fig:both:completion2"]
 meet this goal via different strategies for each kind of code:
@itemlist[
@item{
 In @|sdeep|-typed code, completion inserts @|swrap| expressions at the
  module boundaries to less-typed code.
 Other @|sdeep| expressions have no checks.
}
@item{
 In @|sshallow| code, completion scans incoming untyped code and the result
  of every elimination form.
}
@item{
 In untyped code, completion adds no run-time checks.
 At the boundaries to @|sdeep| and @|sshallow| code, however, the above
  strategies call for a @|swrap| or @|sscan| check.
}
]
@|noindent|@Figureref{fig:both:completion1} in particular shows how surface
 functions translate to evaluation syntax functions and how applications translate.
For @|sdeep| and @|suntyped| code, the completion of an application is simply
 the completion of its subexpressions.
For @|sshallow| code, this elimination form requires a @|sscan| check to
 validate the result.
Other elimination forms have similar completions.

The completion of a @|sshallow| function is deceptively simple.
In a realistic language, such functions would translate to an un-annotated
 function that first @|sscan|s the shape of its input and then proceeds with the
 body expression.
This model, however, gets an implicit domain check thanks to cooperation
 from the upcoming semantics.
The application of a @|sshallow|-typed function always @|sscan|s the
 argument before substituting into the function body (@sectionref{sec:both:model:reduction}).
This design simplifies the model and proof details regarding substitution,
 but the lack of an explicit domain check means that the model cannot
 support a pass that eliminates redundant checks.
Fixing this limitation is a top priority for future extensions of the model.

@Figure-ref{fig:both:completion2} presents the completion rules for module
 boundaries.
Aside from the self-boundaries, the picture in @figure-ref{fig:both:base-interaction}
 is an accurate summary of these rules.
Each module represents a channel of communication between a context and the inside of the module.
The module declares its type discipline and the context's style is clear
 from the conclusion of the surface typing judgment.
To protect against mis-communications, the side with the stronger type requirements
 determines the check that a module boundary completes to.
@|sDeep| always directs, @|sshallow| wins over @|suntyped|, and the others---with
 one exception---are clear @|snoop|s.
The exception is for @|sshallow| values that exit to @|suntyped| code;
 for integers there is nothing to protect, but functions would seem to need
 some kind of wrapper to protect their body against untyped input.
In fact, these boundaries are safe @|snoop|s because @|sshallow| pre-emptively
 protects functions as noted above.


@figure*[
  "fig:both:completion1"
  @elem{Surface-to-evaluation completion (selected rules)}

@exact|{
\begin{mathpar}
  \inferrule*{
  }{
    \stypeenv_0 \sST \svar_0 : \tdyn \scompile \svar_0
  }

  \inferrule*{
  }{
    \stypeenv_0 \sST \svar_0 : \stype_0 \scompile \svar_0
  }

  \inferrule*{
  }{
    \stypeenv_0 \sST \svar_0 : \tfloor{\stype_0} \scompile \svar_0
  }

%  \inferrule*{
%  }{
%    \stypeenv_0 \sST \sint_0 : \tdyn \scompile \sint_0
%  }
%
%  \inferrule*{
%  }{
%    \stypeenv_0 \sST \sint_0 : \stype_0 \scompile \sint_0
%  }
%
%  \inferrule*{
%  }{
%    \stypeenv_0 \sST \sint_0 : \tfloor{\stype_0} \scompile \sint_0
%  }
%
%  \inferrule*{
%    \stypeenv_0 \sST \sexpr_0 : \tdyn \scompile \sexpr_2
%    \\\\
%    \stypeenv_0 \sST \sexpr_1 : \tdyn \scompile \sexpr_3
%  }{
%    \stypeenv_0 \sST \epair{\sexpr_0}{\sexpr_1} : \tdyn \scompile \epair{\sexpr_2}{\sexpr_3}
%  }
%
%  \inferrule*{
%    \stypeenv_0 \sST \sexpr_0 : \stype_0 \scompile \sexpr_2
%    \\\\
%    \stypeenv_0 \sST \sexpr_1 : \stype_1 \scompile \sexpr_3
%  }{
%    \stypeenv_0 \sST \epair{\sexpr_0}{\sexpr_1} : \tpair{\stype_0}{\stype_1} \scompile \epair{\sexpr_2}{\sexpr_3}
%  }
%
%  \inferrule*{
%    \stypeenv_0 \sST \sexpr_0 : \tfloor{\stype_0} \scompile \sexpr_2
%    \\\\
%    \stypeenv_0 \sST \sexpr_1 : \tfloor{\stype_1} \scompile \sexpr_3
%  }{
%    \stypeenv_0 \sST \epair{\sexpr_0}{\sexpr_1} : \tfloor{\tpair{\stype_0}{\stype_1}} \scompile \epair{\sexpr_2}{\sexpr_3}
%  }
%
  \inferrule*{
    \fcons{\tann{\svar_0}{\tdyn}}{\stypeenv_0} \sST \sexpr_0 : \tdyn \scompile \sexpr_1
  }{
    \stypeenv_0 \sST \efun{\svar_0}{\sexpr_0} : \tdyn \scompile \efun{\svar_0}{\sexpr_1}
  }

  \inferrule*{
    \fcons{\tann{\svar_0}{\stype_0}}{\stypeenv_0} \sST \sexpr_0 : \stype_1 \scompile \sexpr_1
  }{
    \stypeenv_0 \sST \efun{\tann{\svar_0}{\stype_0}}{\sexpr_0} : \tfun{\stype_0}{\stype_1} \scompile \efun{\tann{\svar_0}{\stype_0}}{\sexpr_1}
  }

  \inferrule*{
    \fcons{\tann{\svar_0}{\tfloor{\stype_0}}}{\stypeenv_0} \sST \sexpr_0 : \tfloor{\stype_1} \scompile \sexpr_1
    \\
    \fshape{\stype_0} = \sshape_0
  }{
    \stypeenv_0 \sST \efun{\tann{\svar_0}{\tfloor{\stype_0}}}{\sexpr_0} : \tfloor{\tfun{\stype_0}{\stype_1}} \scompile \efun{\tann{\svar_0}{\sshape_0}}{\sexpr_1}
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \tdyn \scompile \sexpr_2
    \\\\
    \stypeenv_0 \sST \sexpr_1 : \tdyn \scompile \sexpr_3
  }{
    \stypeenv_0 \sST \eappu{\sexpr_0}{\sexpr_1} : \tdyn \scompile \eappu{\sexpr_2}{\sexpr_3}
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \tfun{\stype_1}{\stype_0} \scompile \sexpr_2
    \\\\
    \stypeenv_0 \sST \sexpr_1 : \stype_1 \scompile \sexpr_3
  }{
    \stypeenv_0 \sST \eappu{\sexpr_0}{\sexpr_1} : \stype_0 \scompile \eappu{\sexpr_2}{\sexpr_3}
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \tfloor{\tfun{\stype_1}{\stype_0}} \scompile \sexpr_2
    \\\\
    \stypeenv_0 \sST \sexpr_1 : \tfloor{\stype_1} \scompile \sexpr_3
    \\
    \fshape{\stype_0} = \sshape_0
  }{
    \stypeenv_0 \sST \eappu{\sexpr_0}{\sexpr_1} : \tfloor{\stype_0} \scompile \escan{\sshape_0}{(\eappu{\sexpr_2}{\sexpr_3})}
  }

\end{mathpar}
}|]

@figure*[
  "fig:both:completion2"
  @elem{Completion for module boundaries}

  @exact|{
\begin{mathpar}
  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \tdyn \scompile \sexpr_1
  }{
    \stypeenv_0 \sST \emodule{\sulang}{\sexpr_0} : \tdyn \scompile \enoop{\sexpr_1}
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \stype_0 \scompile \sexpr_1
  }{
    \stypeenv_0 \sST \emodule{\stlang}{\sexpr_0} : \tdyn \scompile \ewrap{\stype_0}{\sexpr_1}
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \tfloor{\stype_0} \scompile \sexpr_1
  }{
    \stypeenv_0 \sST \emod{\sslang}{\sexpr_0} : \tdyn \scompile \enoop{\sexpr_1}
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \tdyn \scompile \sexpr_1
  }{
    \stypeenv_0 \sST \emodule{\sulang}{\sexpr_0} : \stype_0 \scompile \ewrap{\stype_0}{\sexpr_1}
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \stype_0 \scompile \sexpr_1
  }{
    \stypeenv_0 \sST \emodule{\stlang}{\sexpr_0} : \stype_0 \scompile \enoop{\sexpr_1}
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \tfloor{\stype_0} \scompile \sexpr_1
  }{
    \stypeenv_0 \sST \emodule{\sslang}{\sexpr_0} : \stype_0 \scompile \ewrap{\stype_0}{\sexpr_1}
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \tdyn \scompile \sexpr_1
  }{
    \stypeenv_0 \sST \emodule{\sulang}{\sexpr_0} : \tfloor{\stype_0} \scompile \escan{\sshape_0}{\sexpr_1}
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \stype_0 \scompile \sexpr_1
  }{
    \stypeenv_0 \sST \emodule{\stlang}{\sexpr_0} : \tfloor{\stype_0} \scompile \ewrap{\stype_0}{\sexpr_1}
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \tfloor{\stype_0} \scompile \sexpr_1
  }{
    \stypeenv_0 \sST \emodule{\sslang}{\sexpr_0} : \tfloor{\stype_0} \scompile \enoop{\sexpr_1}
  }
\end{mathpar}
}|]


@subsection[#:tag "sec:both:model:reduction"]{Reduction Relation}

The semantics of the evaluation syntax is based on one notion of reduction (@figure-ref{fig:both:rr}).
@; ... like how MT reuses host reduction, but don't get too excited we still play tricks with transient
Aside from the domain checks for @|sshallow|-typed functions, reduction proceeds
 in a standard, untyped fashion.
Unary and binary operations proceed according to the @${\sdelta} metafunction (@figureref{fig:both:extra-rr}).
Basic function application substitutes an argument value into a function body.
Wrapped function application decomposes into two wrap boundaries: one
 for the input and another for the result.
Lastly, boundary terms optionally perform a run-time check.
A @|snoop| boundary performs no check and lets any value cross.
A @|sscan| boundary checks the top-level shape of a value against the expected
 type.
And a @|swrap| boundary checks top-level shapes and either installs a wrapper
 around a higher-order value or recursively checks a data structure.

@Figure-ref{fig:both:extra-rr} defines evaluation metafunctions.
The @${\sdelta} function gives semantics to primitives.
The @${\sshallow} function matches a type shape against the outer structure of a value.

@figure*[
  "fig:both:rr"
  @elem{Semantics for the evaluation syntax}

@exact|{
\begin{rrarray}
  \eunop{\svalue_0} & \snr
  & \stagerror
  \\\sidecond{if $\sdelta(\sunop, \svalue_0)$ is undefined}
  \\[1.0ex]
  \eunop{\svalue_0} & \snr
  & \sdelta(\sunop, \svalue_0)
  \\\sidecond{if $\sdelta(\sunop, \svalue_0)$ is defined}
  \\[1.0ex]
  \ebinop{\svalue_0}{\svalue_1} & \snr
  & \stagerror
  \\\sidecond{if $\sdelta(\sbinop, \svalue_0, \svalue_1)$ is undefined}
  \\[1.0ex]
  \ebinop{\svalue_0}{\svalue_1} & \snr
  & \sdelta(\sbinop, \svalue_0, \svalue_1)
  \\\sidecond{if $\sdelta(\sbinop, \svalue_0, \svalue_1)$ is defined}
  \\[1.0ex]
  \eappu{\svalue_0}{\svalue_1} & \snr &
  \stagerror
  \\\sidecond{if $\svalue_0 \not\in \efun{\svar}{\sexpr} \cup \efun{\tann{\svar}{\stype}}{\sexpr} \cup \efun{\tann{\svar}{\sshape}}{\sexpr} \cup \emon{\stype}{\svalue}$}
  \\[1.0ex]
  \eappu{(\efun{\svar_0}{\sexpr_0})}{\svalue_0} & \snr
  & \esubst{\sexpr_0}{\svar_0}{\svalue_0}
  \\[1.0ex]
  \eappu{(\efun{\tann{\svar_0}{\stype_0}}{\sexpr_0})}{\svalue_0} & \snr
  & \esubst{\sexpr_0}{\svar_0}{\svalue_0}
  \\[1.0ex]
  \eappu{(\efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0})}{\svalue_0} & \snr
  & \sscanerror
  \\\sidecond{if $\neg\fshallow{\sshape_0}{\svalue_0}$}
  \\[1.0ex]
  \eappu{(\efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0})}{\svalue_0} & \snr
  & \esubst{\sexpr_0}{\svar_0}{\svalue_0}
  \\\sidecond{if $\fshallow{\sshape_0}{\svalue_0}$}
  \\[1.0ex]
  \eappu{(\emon{\tfun{\stype_0}{\stype_1}}{\svalue_0})}{\svalue_1} & \snr
  & \ewrap{\stype_1}{(\eappu{\svalue_0}{(\ewrap{\stype_0}{\svalue_1})})}
  \\[1.0ex]
  \enoop{\svalue_0} & \snr
  & \svalue_0
  \\[1.0ex]
  \escan{\sshape_0}{\svalue_0} & \snr
  & \sscanerror
  \\\sidecond{if $\neg\fshallow{\sshape_0}{\svalue_0}$}
  \\[1.0ex]
  \escan{\sshape_0}{\svalue_0} & \snr
  & \svalue_0
  \\\sidecond{if $\fshallow{\sshape_0}{\svalue_0}$}
  \\[1.0ex]
  \ewrap{\stype_0}{\svalue_0} & \snr
  & \swraperror
  \\\sidecond{if $\fshallow{\fshape{\sshape_0}}{\svalue_0}$}
  \\[1.0ex]
  \ewrap{\tfun{\stype_0}{\stype_1}}{\svalue_0} & \snr
  & \emon{\tfun{\stype_0}{\stype_1}}{\svalue_0}
  \\\sidecond{if $\fshallow{\kfun}{\svalue_0}$}
  \\[1.0ex]
  \ewrap{\tpair{\stype_0}{\stype_1}}{\epair{\svalue_0}{\svalue_1}} & \snr
  & \epair{\ewrap{\stype_0}{\svalue_0}}{\ewrap{\stype_1}{\svalue_1}}
  \\[1.0ex]
  \ewrap{\stype_0}{\svalue_0} & \snr
  & \svalue_0
  \\\sidecond{if $\stype_0 \in \tint \cup \tnat$ and $\fshallow{\stype_0}{\svalue_0}$}
\end{rrarray}

\medskip
\lbl{\fbox{\(\sexpr \srr \sexpr\)}\(~~\sdefeq \mbox{reflexive, transitive, compatible closure of $\snr$}\)}{
}

}|]

@figure*[
  "fig:both:rrlbl"
  @elem{Labeled semantics for the evaluation language, derived from @figure-ref{fig:both:rr} and the guidelines in @section-ref{sec:design:laws}.}

@exact|{
\begin{rrarray}
  \obars{\eunop{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
  & \snr
  & \obars{\stagerror}{\sowner_1}
  \\\sidecond{if $\svalue_0 \not\in \obars{\svalue}{\sowner}$ and $\sdelta(\sunop, \svalue_0)$ is undefined}
  \\[1.0ex]
  \obars{\eunop{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
  & \snr
  & \obbars{\sdelta(\sunop, \svalue_0)}{\fconcat{\sownerlist_0}{\sowner_1}}
  \\\sidecond{if $\sdelta(\sunop, \svalue_0)$ is defined}
  \\[1.0ex]
  \obars{\ebinop{\obbars{\svalue_0}{\sownerlist_0}}{\obbars{\svalue_1}{\sownerlist_1}}}{\sowner_2}
  & \snr
  & \obars{\stagerror}{\sowner_2}
  \\\sidecond{if $\svalue_i \not\in \obars{\svalue}{\sowner}$ and $\sdelta(\sbinop, \svalue_0, \svalue_1)$ is undefined}
  \\[1.0ex]
  \obars{\ebinop{\obbars{\svalue_0}{\sownerlist_0}}{\obbars{\svalue_1}{\sownerlist_1}}}{\sowner_2}
  & \snr
  & \obars{\sdelta(\sbinop, \svalue_0, \svalue_1)}{\sowner_2}
  \\\sidecond{if $\sdelta(\sbinop, \svalue_0, \svalue_1)$ is defined}
  \\[1.0ex]
  \obars{\eappu{\obbars{\svalue_0}{\sownerlist_0}}{\svalue_1}}{\sowner_1}
  & \snr
  & \obars{\stagerror}{\sowner_1}
  \\\sidecond{if $\svalue_0 \not\in \obars{\svalue}{\sowner} \cup \efun{\svar}{\sexpr} \cup \efun{\tann{\svar}{\stype}}{\sexpr} \cup \efun{\tann{\svar}{\sshape}}{\sexpr} \cup \emon{\stype}{\svalue}$}
  \\[1.0ex]
  \obars{\eappu{\obbars{\efun{\svar_0}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1}
  & \snr
  & \obbars{\esubst{\sexpr_0}{\svar_0}{\obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}}}{\fconcat{\sownerlist_0}{\sowner_1}}
  \\[1.0ex]
  \obars{\eappu{\obbars{\efun{\tann{\svar_0}{\stype_0}}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1}
  & \snr
  & \obbars{\esubst{\sexpr_0}{\svar_0}{\obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}}}{\fconcat{\sownerlist_0}{\sowner_1}}
  \\[1.0ex]
  \obars{\eappu{\obbars{\efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1}
  & \snr
  & \obars{\sscanerror}{\sowner_1}
  \\\sidecond{if $\neg\fshallow{\sshape_0}{\svalue_0}$}
  \\[1.0ex]
  \obars{\eappu{\obbars{\efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1}
  & \snr
  & \obbars{\esubst{\sexpr_0}{\svar_0}{\obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}}}{\fconcat{\sownerlist_0}{\sowner_1}}
  \\\sidecond{if $\fshallow{\sshape_0}{\svalue_0}$}
  \\[1.0ex]
  \obars{\eappu{\obbars{\emon{\tfun{\stype_0}{\stype_1}}{\obars{\svalue_0}{\sowner_0}}}{\sownerlist_1}}{\svalue_1}}{\sowner_2}
  & \snr
  \\\sidecond{\qquad\(\obbars{\ewrap{\stype_1}{\obars{\eappu{\svalue_0}{(\ewrap{\stype_0}{\obbars{\svalue_1}{\fconcat{\sowner_2}{\frev{\sownerlist_1}}}})}}{\sowner_0}}}{\fconcat{\sownerlist_1}{\sowner_2}}\)}
  \\[1.0ex]
  \obars{\enoop{\obbars{\svalue_0}}{\sownerlist_0}}{\sowner_1}
  & \snr
  & \obbars{\svalue_0}{\fconcat{\sownerlist_0}{\sowner_1}}
  \\[1.0ex]
  \obars{\escan{\sshape_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
  & \snr
  & \obars{\sscanerror}{\sowner_1}
  \\\sidecond{if $\neg\fshallow{\sshape_0}{\svalue_0}$}
  \\[1.0ex]
  \obars{\escan{\sshape_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
  & \snr
  & \obbars{\svalue_0}{\fconcat{\sownerlist_0}{\sowner_1}}
  \\\sidecond{if $\fshallow{\sshape_0}{\svalue_0}$}
  \\[1.0ex]
  \obars{\ewrap{\stype_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
  & \snr
  & \obars{\swraperror}{\sowner_1}
  \\\sidecond{if $\fshallow{\fshape{\sshape_0}}{\svalue_0}$}
  \\[1.0ex]
  \obars{\ewrap{\tfun{\stype_0}{\stype_1}}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
  & \snr
  & \obars{\emon{\tfun{\stype_0}{\stype_1}}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
  \\\sidecond{if $\fshallow{\kfun}{\svalue_0}$}
  \\[1.0ex]
  \obars{\ewrap{\tpair{\stype_0}{\stype_1}}{\obbars{\epair{\svalue_0}{\svalue_1}}{\sownerlist_0}}}{\sowner_1}
  & \snr
  \\\sidecond{\qquad\(\obars{\epair{\ewrap{\stype_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\ewrap{\stype_1}{\obbars{\svalue_1}{\sownerlist_0}}}}{\sowner_1}\)}
  \\[1.0ex]
  \obars{\ewrap{\stype_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}
  & \snr
  & \obars{\svalue_0}{\sowner_1}
  \\\sidecond{if $\stype_0 \in \tint \cup \tnat$ and $\fshallow{\stype_0}{\svalue_0}$}
\end{rrarray}

}|]


@figure*[
  "fig:both:extra-rr"
  @elem{Semantic metafunctions}

@exact|{
\begin{minipage}[t]{0.5\columnwidth}
\lbl{\fbox{$\sdelta : \ffun{\tpair{\sunop}{\svalue}}{\svalue}$}}{
  \begin{langarray}
    \sdelta(\sfst, \epair{\svalue_0}{\svalue_1}) & \feq & \svalue_0
    \\
    \sdelta(\ssnd, \epair{\svalue_0}{\svalue_1}) & \feq & \svalue_1
  \end{langarray}
}

\end{minipage}\begin{minipage}[t]{0.5\columnwidth}
\lbl{\fbox{$\sdelta : \ffun{\tpair{\sbinop}{\tpair{\svalue}{\svalue}}}{\svalue}$}}{
  \begin{langarray}
    \sdelta(\ssum, \sint_0, \sint_1) & \feq & \sint_0 + \sint_1
    \\
    \sdelta(\squotient, \sint_0, 0) & \feq & \divisionbyzeroerror
    \\
    \sdelta(\squotient, \sint_0, \sint_1) & \feq & \floorof{\sint_0 / \sint_1}
  \end{langarray}
}
\end{minipage}

\lbl{\fbox{$\sshallow : \ffun{\tpair{\sshape}{\svalue}}{\fbool}$}}{
  \begin{langarray}
   \fshallow{\kfun}{\svalue_0} & \feq & \ftrue
   \\\sidecond{if $\svalue_0 \in \efun{\svar}{\sexpr} \cup \efun{\tann{\svar}{\stype}}{\sexpr} \cup \efun{\tann{\svar}{\sshape}}{\sexpr} \cup \emon{\stype}{\svalue}$}
   \\[0.8ex]
   \fshallow{\kpair}{\epair{\svalue_0}{\svalue_1}} & \feq & \ftrue
   \\[0.8ex]
   \fshallow{\kint}{\sint_0} & \feq & \ftrue
   \\[0.8ex]
   \fshallow{\knat}{\snat_0} & \feq & \ftrue
   \\[0.8ex]
   \fshallow{\kany}{\svalue_0} & \feq & \ftrue
   \\[0.8ex]
   \fshallow{\sshape_0}{\svalue_0} & \feq & \ffalse
   \\\sidecond{otherwise}
  \end{langarray}
}
}|]


@subsection[#:tag "sec:both:model:ownership"]{Single-Owner Consistency}

@|sDeep| types are characterized by complete monitoring (@chapter-ref{chap:design}).
To state a complete monitoring theorem, the model needs a labeled syntax,
 a single-owner consistency judgment, and a reduction relation that propagates
 labels.

The labeled syntax permits an ownership label around any expression (@figure-ref{fig:both:ownership-syntax}).
For example, the terms @${\obars{4}{\sowner_0}} and @${\obars{\eapp{\svar_0}{\svar_1}}{\sowner_1}}
 illustrate one labeled value and one labeled expression.
Most terms may have zero or more labels.
Boundary terms are an exception;
 a @${\swrap}, @${\sscan}, or @${\snoop} boundary must have at least one label
 around its subexpression.
The notation @${\obbars{\sexpr_0}{\sownerlist_0}} matches an expression with a sequence of labels
 (@${\sownerlist_0}).

An ownership label @${\sowner_0} carries two pieces of information.
First is a typing discipline: @${\sdowner} for @|sdeep|,
 @${\ssowner} for @|sshallow|, and @${\suowner} for @|suntyped|.
Second is a natural number index to distinguish different labels.
Initially, in a well-formed expression, these labels state the original owner
 and typing of a subterm.
As expressions reduce to values and flow across boundaries, labels accumulate
 to show which components are partly responsible for these values.

Ultimately, the goal of our complete monitoring proof effort is to show that
 only @|sdeep|-typed code is responsible for @|sdeep|-typed expressions.
Both @|sshallow| and @|suntyped| may recklessly share values.
The single-owner consistency judgment in @figure-ref{fig:both:ownership-consistency}
 formalizes the target invariant by stating when
 an expression is consistent for label @${\sowner_0} and label environment
 @${\sownerenv_0}.
A variable must be bound to @${\sowner_0} in the label environment.
Non-boundary terms must have consistent subterms.
Boundary terms and guard wrappers are ownership switch points;
 a boundary is consistent if its subterm is consistent with respect to the
 label inside the boundary.
Finally, the rules for explicitly-labeled expressions impose a discipline
 on labels.
A @|sdeep|-labeled expression may have other @|sdeep| labels, but nothing weaker.
@|sShallow| and @|suntyped|-labeled expressions, by contrast, can mix together.

Reduction of a labeled expression begins with the rules for the evaluation language
 (@figure-ref{fig:both:rr}) and propagates labels according to the laws
 stated in @sectionref{sec:design:laws}.
@Figure-ref{fig:both:rrlbl} presents the rules in full.
In short, labels always accumulate unless a simple value meets a boundary with
 a matching type shape.
Even @|snoop| boundaries add a label; this is why ownership consistency allows
 sequences of @|sdeep| labels.

@figure*[
  "fig:both:ownership-syntax"
  @elem{Ownership syntax}

@exact|{
\begin{langarray}
  \sexpr & \slangeq &
    \svar \mid \svalue \mid \epair{\sexpr}{\sexpr}
    \mid \eunop{\sexpr} \mid
    \ebinop{\sexpr}{\sexpr} \mid \eappu{\sexpr}{\sexpr} \mid \serror \mid
  \\ & &
    \ewrap{\stype}{\obars{\sexpr}{\sowner}}
    \mid \escan{\sshape}{\obars{\sexpr}{\sowner}}
    \mid \enoop{\obars{\sexpr}{\sowner}}
    \mid \obars{\sexpr}{\sowner}
  \\
  \svalue & \slangeq &
    \sint \mid \epair{\svalue}{\svalue}
    \mid \efun{\svar}{\sexpr}
    \mid \efun{\tann{\svar}{\stype}}{\sexpr}
    \mid \efun{\tann{\svar}{\sshape}}{\sexpr}
    \mid \emon{\stype}{\obars{\svalue}{\sowner}}
    \mid \obars{\svalue}{\sowner}
  \\
  \sctx & \slangeq &
    \ldots \mid \obars{\sctx}{\sowner}
  \\
  \sowner & \slangeq &
    \stowner_0 \mid \stowner_1 \mid \ldots \mid
  %\\ & &
    \ssowner_0 \mid \ssowner_1 \mid \ldots \mid
  %\\ & &
    \suowner_0 \mid \suowner_1 \mid \ldots
  \\
  \sownerlist & \slangeq &
    \mbox{sequence of ownership labels ($\sowner$)}
  \\
  \sownerenv & \slangeq &
    \cdot \mid \fcons{\tann{\svar}{\sowner}}{\sownerenv}
\end{langarray}
}|]

@figure*[
  "fig:both:ownership-consistency"
  @elem{Single-owner consistency}

@exact|{
\begin{mathpar}
  \inferrule*{
    \tann{\svar_0}{\sowner_0} \in \sownerenv_0
  }{
    \sowner_0; \sownerenv_0 \sWL \svar_0
  }

  \inferrule*{
  }{
    \sowner_0; \sownerenv_0 \sWL \sint_0
  }

  \inferrule*{
    \sowner_0; \sownerenv_0 \sWL \sexpr_0
    \\
    \sowner_0; \sownerenv_0 \sWL \sexpr_1
  }{
    \sowner_0; \sownerenv_0 \sWL \epair{\sexpr_0}{\sexpr_1}
  }

  \inferrule*{
    \sowner_0; \fcons{\tann{\svar_0}{\sowner_0}}{\sownerenv_0} \sWL \sexpr_0
  }{
    \sowner_0; \sownerenv_0 \sWL \efun{\svar_0}{\sexpr_0}
  }

  \inferrule*{
    \sowner_0; \fcons{\tann{\svar_0}{\sowner_0}}{\sownerenv_0} \sWL \sexpr_0
  }{
    \sowner_0; \sownerenv_0 \sWL \efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0}
  }

  \inferrule*{
    \sowner_0; \fcons{\tann{\svar_0}{\sowner_0}}{\sownerenv_0} \sWL \sexpr_0
  }{
    \sowner_0; \sownerenv_0 \sWL \efun{\tann{\svar_0}{\stype_0}}{\sexpr_0}
  }

  \inferrule*{
    \sowner_0; \sownerenv_0 \sWL \sexpr_0
  }{
    \sowner_0; \sownerenv_0 \sWL \eunop{\sexpr_0}
  }

  \inferrule*{
    \sowner_0; \sownerenv_0 \sWL \sexpr_0
    \\
    \sowner_0; \sownerenv_0 \sWL \sexpr_1
  }{
    \sowner_0; \sownerenv_0 \sWL \ebinop{\sexpr_0}{\sexpr_1}
  }

  \inferrule*{
    \sowner_0; \sownerenv_0 \sWL \sexpr_0
    \\
    \sowner_0; \sownerenv_0 \sWL \sexpr_1
  }{
    \sowner_0; \sownerenv_0 \sWL \eappu{\sexpr_0}{\sexpr_1}
  }

  \inferrule*{
  }{
    \sowner_0; \sownerenv_0 \sWL \serror
  }

  \inferrule*{
    \sowner_1; \sownerenv_0 \sWL \sexpr_0
  }{
    \sowner_0; \sownerenv_0 \sWL \enoop{\obars{\sexpr_0}{\sowner_1}}
  }

  \inferrule*{
    \sowner_1; \sownerenv_0 \sWL \sexpr_0
  }{
    \sowner_0; \sownerenv_0 \sWL \escan{\sshape_0}{\obars{\sexpr_0}{\sowner_1}}
  }

  \inferrule*{
    \sowner_1; \sownerenv_0 \sWL \sexpr_0
  }{
    \sowner_0; \sownerenv_0 \sWL \ewrap{\stype_0}{\obars{\sexpr_0}{\sowner_1}}
  }

  \inferrule*{
    \sowner_1; \sownerenv_0 \sWL \svalue_0
  }{
    \sowner_0; \sownerenv_0 \sWL \emon{\stype_0}{\obars{\svalue_0}{\sowner_1}}
  }

  \inferrule*{
    \stowner_1; \sownerenv_0 \sWL \sexpr_0
  }{
    \stowner_0; \sownerenv_0 \sWL \obars{\sexpr_0}{\stowner_1}
  }

  \inferrule*{
    \ssowner_1; \sownerenv_0 \sWL \sexpr_0
  }{
    \ssowner_0; \sownerenv_0 \sWL \obars{\sexpr_0}{\ssowner_1}
  }

  \inferrule*{
    \suowner_0; \sownerenv_0 \sWL \sexpr_0
  }{
    \ssowner_0; \sownerenv_0 \sWL \obars{\sexpr_0}{\suowner_0}
  }

  \inferrule*{
    \suowner_1; \sownerenv_0 \sWL \sexpr_0
  }{
    \suowner_0; \sownerenv_0 \sWL \obars{\sexpr_0}{\suowner_1}
  }

  \inferrule*{
    \ssowner_0; \sownerenv_0 \sWL \sexpr_0
  }{
    \suowner_0; \sownerenv_0 \sWL \obars{\sexpr_0}{\ssowner_0}
  }

\end{mathpar}
}|]


@subsection[#:tag "sec:both:model:theorems"]{Properties}

The primary meta-theoretic results are about type soundness and
 complete monitoring.
Type soundness predicts the possible outcomes of a well-typed expression.
Naturally, these outcomes depend on the ``strength'' of the static types;
 for example, @|suntyped| code has weaker guarantees than @|sshallow| code.
Complete monitoring asks whether single-owner consistency is an invariant;
 if so, then programmers can trust @|sdeep| types as behavioral guarantees.

The statement of type soundness relies on one new notation and a family of
 metafunctions.
The notation @${\ssurface_0 \srr \sexpr_0} defines evaluation for surface
 expressions; the meaning is that @${\ssurface_0} is well-typed somehow
 (@${\fexists{\stspec}{\sST \ssurface_0 : \stspec}}),
 compiles to an evaluation expression (@${\sST \ssurface_0 : \stspec \scompile \sexpr_1}),
 and then the compiled expression steps to the result (@${\sexpr_1 \srr \sexpr_0}).
The metafunctions---@${\stypemapzero}, @${\stypemapshape}, and @${\stypemapone}---map
 surface-language types to evaluation types.
One function, @${\stypemapshape}, extends the similarly-name function from
 @figureref{fig:both:shallow-type} to map the unitype @${\tdyn} to itself.
The others are simple: @${\stypemapzero} maps all types to @${\tdyn}
 and @${\stypemapone} is the identity.
These tools enable a concise, parameterized statement of type soundness.

@exact|{
\begin{definition}[TS$(\stypemap)$]
  Language\ $\slang$
  satisfies\ $\fTS{\stypemap}$
  if for all\ $\ssurface_0$
  such that\ $\sST \ssurface_0 : \stspec$
  holds, one of the following holds:
  \begin{itemize}
    \item $\ssurface_0 \srr \svalue_0$ and\ $\sWTlang \svalue_0 : \ftypemap{\stspec}$
    \item $\ssurface_0 \srr \serror$
    \item $\ssurface_0 \srr$ diverges
  \end{itemize}
\end{definition}
}|

@exact|{
\begin{theorem}[type soundness]\leavevmode
  \begin{itemize}
    \item $\sU$ satisfies\ $\fTS{\stypemapzero}$
    \item $\sS$ satisfies\ $\fTS{\stypemapshape}$
    \item $\sT$ satisfies\ $\fTS{\stypemapone}$
  \end{itemize}
\end{theorem}
\begin{proof}
  \Lemmaref{lemma:both:completion} guarantees that the compiled form
   of the surface expression is well-typed.
  The rest follows from straightforward progress and preservation lemmas for the evaluation typing judgments.
  \Lemmaref{lemma:both:delta} is essential to preservation for primitive operations.
  Lemmas~\ref{lemma:both:su} and~\ref{lemma:both:boundary} are key aspects of preservation for boundary terms.
\end{proof}
}|

Complete monitoring is technically a statement about labeled expressions
 and a label-propagating reduction relation.
But, because the propagating reduction is derived from the basic reduction
 relation in a straightforward manner, our theorem statement uses the
 basic symbol (@${\srr}).
Likewise, both @${\sexpr_0} and @${\sexpr_1} refer to a labeled variant
 of an evaluation-language expression.
If no such labeling exist for a term, then the theorem holds vacuously.

@exact|{
\begin{theorem}[complete monitoring]
  If\ $~\sST \ssurface_0 : \stspec$
  and\ $\sST \ssurface_0 : \stspec \scompile \sexpr_0$
  and\ $\sowner_0; \cdot \Vdash \sexpr_0$
  and\ $\sexpr_0 \srr \sexpr_1$
  then\ $\sowner_0; \cdot \Vdash \sexpr_1$.
\end{theorem}
\begin{proof}
  By a preservation argument.
  The proofs for each basic reduction step are sketched below.
  These depend on two metafunctions: $\srev$ reverses a sequence of labels
   and $\slast$ extracts the last (outermost) element of such a sequence.

  \begin{description}
  \item[Case:]
    \(\obars{\eunop{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snr \obars{\stagerror}{\sowner_1}\)
  \item[]
    by the definition, \(\sowner_1; \cdot \sWL \obars{\stagerror}{\sowner_1}\).

  \item[Case:]
    \(\obars{\eunop{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snr \obbars{\sdelta(\sunop, \svalue_0)}{\fconcat{\sownerlist_0}{\sowner_1}}\)
  \subitem
    \begin{enumerate}
    \item
      $\sownerlist_0$ is either all \sdeep{} labels or a mix of \sshallow{} and \suntyped{}, by single-owner consistency of the redex.
    \item
      similarly, $\sowner_1$ must match $\sownerlist_0$
    \item
      $\svalue_0$ is a pair, because $\sdelta$ is defined on it.
    \item
      both components of $\svalue_0$ are well-labeled, again by single-owner consistency on the redex.
    \item
      by the definition of $\sdelta$.
    \end{enumerate}

  \item[Case:]
    \(\obars{\ebinop{\obbars{\svalue_0}{\sownerlist_0}}{\obbars{\svalue_1}{\sownerlist_1}}}{\sowner_2} \snr \obars{\stagerror}{\sowner_2}\)
  \item[]
    by the definition of $\sWL$.

  \item[Case:]
    \(\obars{\ebinop{\obbars{\svalue_0}{\sownerlist_0}}{\obbars{\svalue_1}{\sownerlist_1}}}{\sowner_2} \snr \obars{\sdelta(\sbinop, \svalue_0, \svalue_1)}{\sowner_2}\)
  \item[]
    by the definition of $\sWL$ and $\sdelta$; note that the binary operators are not elimination forms.

  \item[Case:]
    \(\obars{\eappu{\obbars{\svalue_0}{\sownerlist_0}}{\svalue_1}}{\sowner_1} \snr \obars{\stagerror}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$.

  \item[Case:]
    \(\obars{\eappu{\obbars{\efun{\svar_0}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1} \snr \obbars{\esubst{\sexpr_0}{\svar_0}{\obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}}}{\fconcat{\sownerlist_0}{\sowner_1}}\)
  \subitem
    \begin{enumerate}
    \item\label{step:both:cm:1}
      $\sownerlist_0$ is all \sdeep{} or a mix of \sshallow{} and \suntyped{}, by single-owner consistency of the redex.
    \item\label{step:both:cm:2}
      $\sowner_2; \cdot \sWL \svalue_0$, also by single-owner consistency of the redex.
    \item
      $\flast{\sownerlist_0}; \cdot \sWL \obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}$, by steps~\ref{step:both:cm:1} and~\ref{step:both:cm:2}.
    \item
      $\flast{\sownerlist_0}; \cdot \sWL \svar_0$ for each occurrence of $\svar_0$ in $\sexpr_0$, by single-owner consistency of the redex.
    \item
      by a substitution lemma.
    \end{enumerate}

  \item[Case:]
    \(\obars{\eappu{\obbars{\efun{\tann{\svar_0}{\stype_0}}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1} \snr \obbars{\esubst{\sexpr_0}{\svar_0}{\obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}}}{\fconcat{\sownerlist_0}{\sowner_1}}\)
  \item[]
    similar to the previous case.

  \item[Case:]
    \(\obars{\eappu{\obbars{\efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1} \snr \obars{\sscanerror}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$.

  \item[Case:]
    \(\obars{\eappu{\obbars{\efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0}}{\sownerlist_0}}{\svalue_0}}{\sowner_1} \snr \obbars{\esubst{\sexpr_0}{\svar_0}{\obbars{\svalue_0}{\fconcat{\sowner_1}{\frev{\sownerlist_0}}}}}{\fconcat{\sownerlist_0}{\sowner_1}}\)
  \item[]
    similar to the other substitution cases.

  \item[Case:]
    \(\obars{\eappu{\obbars{\emon{\tfun{\stype_0}{\stype_1}}{\obars{\svalue_0}{\sowner_0}}}{\sownerlist_1}}{\svalue_1}}{\sowner_2} \snr\)
    \\\qquad\(\obbars{\ewrap{\stype_1}{\obars{\eappu{\svalue_0}{(\ewrap{\stype_0}{\obbars{\svalue_1}{\fconcat{\sowner_2}{\frev{\sownerlist_1}}}})}}{\sowner_0}}}{\fconcat{\sownerlist_1}{\sowner_2}}\)
  \subitem
    \begin{enumerate}
    \item
      $\sowner_0; \cdot \sWL \svalue_0$, by single-owner consistency of the redex.
    \item
      $\sowner_2; \cdot \sWL \svalue_1$, again by the redex.
    \item
      $\sownerlist_1$ is either all \sdeep{} or a mix of \sshallow{} and \suntyped{}, again by the redex.
    \item
      by the definition of $\sWL$.
    \end{enumerate}

  \item[Case:]
    \(\obars{\enoop{\obbars{\svalue_0}}{\sownerlist_0}}{\sowner_1} \snr \obbars{\svalue_0}{\fconcat{\sownerlist_0}{\sowner_1}}\)
  \item[]
    by the definition of $\scompile$, because a $\snoop{}$ boundary connects either:
     two \sdeep{} components, two \sshallow{} components, two \suntyped{} components, or one \sshallow{} and one \suntyped{} component.

  \item[Case:]
    \(\obars{\escan{\sshape_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snr \obars{\sscanerror}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$.

  \item[Case:]
    \(\obars{\escan{\sshape_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snr \obbars{\svalue_0}{\fconcat{\sownerlist_0}{\sowner_1}}\)
  \item[]
    by the definition of $\scompile$, because a $\sscan{}$ boundary only links an \suntyped{} component to a \sshallow{} component.

  \item[Case:]
    \(\obars{\ewrap{\stype_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snr \obars{\swraperror}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$.

  \item[Case:]
    \(\obars{\ewrap{\tfun{\stype_0}{\stype_1}}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snr \obars{\emon{\tfun{\stype_0}{\stype_1}}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$.

  \item[Case:]
    \(\obars{\ewrap{\tpair{\stype_0}{\stype_1}}{\obbars{\epair{\svalue_0}{\svalue_1}}{\sownerlist_0}}}{\sowner_1} \snr\)
    \\\qquad\(\obars{\epair{\ewrap{\stype_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\ewrap{\stype_1}{\obbars{\svalue_1}{\sownerlist_0}}}}{\sowner_1}\)
  \item[]
    by the definition of $\sWL$.
    Note that the rule moves the elements of the pair in the redex into a new pair in the contractum.

  \item[Case:]
  \(\obars{\ewrap{\stype_0}{\obbars{\svalue_0}{\sownerlist_0}}}{\sowner_1} \snr \obars{\svalue_0}{\sowner_1}\)
  \\ where $\stype_0 \in \tint \cup \tnat$ and $\fshallow{\stype_0}{\svalue_0}$ 
  \item[]
    by the definition of $\sWL$.

  \end{description}
\end{proof}
}|

@; @subsection[#:tag "sec:both:model:lemmas"]{Lemmas}

@exact|{
\begin{lemma}[completion]\label{lemma:both:completion}
  If\ $\sST \ssurface_0 : \stspec$
  then\ $\sST \ssurface_0 : \stspec \scompile \sexpr_0$
  and either:
  \begin{itemize}
  \item $\stspec \in \stype$
        and\ $\sWTT \sexpr_0 : \stspec$
  \item $\stspec \in \tfloor{\stype}$
        and\ $\sWTS \sexpr_0 : \ftypemapshape{\stspec}$
  \item $\stspec \in \tdyn$
        and\ $\sWTU \sexpr_0 : \tdyn$
  \end{itemize}
\end{lemma}
}|

@;@exact|{
@;\begin{lemma}[decomposition]
@;  For all\ $\sexpr_0$
@;  there exists unique\ $\sexpr_1, \sctx_0$
@;  such that\ $\sexpr_0 \sexpreq \finhole{\sctx_0}[\sexpr_1]$
@;\end{lemma}
@;}|
@;
@;@exact|{
@;\begin{lemma}[type progress]
@;  If\ $~\sST \sexpr_0 : \stspec$
@;  then either\ $\sexpr_0 \in \svalue \cup \serror$
@;  or\ $\sexpr_0 \scc \sexpr_1$
@;\end{lemma}
@;}|
@;
@;@exact|{
@;\begin{lemma}[type preservation]
@;  If\ $~\sST \sexpr_0 : \stspec$
@;  and\ $\sexpr_0 \scc \sexpr_1$
@;  then\ $\sST \sexpr_1 : \stspec$
@;\end{lemma}
@;}|

@exact|{
\begin{lemma}[$\sdelta, \sDelta$ agreement]\label{lemma:both:delta}\leavevmode
  \begin{itemize}
    \item
      If\ $~\sDelta(\sunop, \tdyn) = \tdyn$
      and\ $\sWTU \svalue_0 : \tdyn$
    \item[]
      and\ $\fdefined{\sdelta(\sunop, \svalue_0)}$
      then\ $\sWTU \sdelta(\sunop, \svalue_0) : \tdyn$
    \item
      If\ $~\sDelta(\sunop, \sshape_0) = \sshape_1$
      and\ $\sWTS \svalue_0 : \sshape_0$
    \item[]
      and\ $\fdefined{\sdelta(\sunop, \svalue_0)}$
      then\ $\sWTS \sdelta(\sunop, \svalue_0) : \sshape_1$
    \item
      If\ $~\sDelta(\sunop, \stype_0) = \stype_1$
      and\ $\sWTD \svalue_0 : \stype_0$
    \item[]
      and\ $\fdefined{\sdelta(\sunop, \svalue_0)}$
      then\ $\sWTD \sdelta(\sunop, \svalue_0) : \stype_1$
    \item
      If\ $~\sDelta(\sbinop, \tdyn, \tdyn) = \tdyn$
      and\ $\sWTU \svalue_0 : \tdyn$
      and\ $\sWTU \svalue_1 : \tdyn$
    \item[]
      and\ $\fdefined{\sdelta(\sbinop, \svalue_0, \svalue_1)}$
      then\ $\sWTU \sdelta(\sbinop, \svalue_0, \svalue_1) : \tdyn$
    \item
      If\ $~\sDelta(\sbinop, \sshape_0, \sshape_1) = \sshape_2$
      and\ $\sWTS \svalue_0 : \sshape_0$
      and\ $\sWTS \svalue_1 : \sshape_1$
    \item[]
      and\ $\fdefined{\sdelta(\sbinop, \svalue_0, \svalue_1)}$
      then\ $\sWTS \sdelta(\sbinop, \svalue_0, \svalue_1) : \sshape_2$
    \item
      If\ $~\sDelta(\sbinop, \stype_0, \stype_1) = \stype_2$
      and\ $\sWTD \svalue_0 : \stype_0$
      and\ $\sWTD \svalue_1 : \stype_1$
    \item[]
      and\ $\fdefined{\sdelta(\sbinop, \svalue_0, \svalue_1)}$
      then\ $\sWTD \sdelta(\sbinop, \svalue_0, \svalue_1) : \stype_2$
  \end{itemize}
\end{lemma}
}|

@;@exact|{
@;\begin{lemma}[type substitution]\leavevmode
@;  \begin{itemize}
@;    \item
@;      If\ $~\vdash \efun{\svar_0}{\sexpr_0} : \tdyn$
@;      and\ $~\vdash \svalue_0 : \tdyn$
@;      then\ $~\vdash \esubst{\sexpr_0}{\svar_0}{\svalue_0} : \tdyn$
@;    \item
@;      If\ $~\vdash \efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0} : \tdyn$
@;      and\ $~\vdash \svalue_0 : \tdyn$
@;      and\ $\fshapematch{\sshape_0}{\svalue_0}$
@;      then\ $~\vdash \esubst{\sexpr_0}{\svar_0}{\svalue_0} : \tdyn$
@;    \item
@;      If\ $~\vdash \efun{\svar_0}{\sexpr_0} : \kfun$
@;      and\ $~\vdash \svalue_0 : \sshape_0$
@;      then\ $~\vdash \esubst{\sexpr_0}{\svar_0}{\svalue_0} : \tdyn$
@;    \item
@;      If\ $~\vdash \efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0} : \kfun$
@;      and\ $~\vdash \svalue_0 : \sshape_1$
@;      and\ $\fshapematch{\sshape_0}{\svalue_0}$
@;      then\ $~\vdash \esubst{\sexpr_0}{\svar_0}{\svalue_0} : \kany$
@;    \item
@;      If\ $~\vdash \efun{\tann{\svar_0}{\stype_0}}{\sexpr_0} : \tfun{\stype_0}{\stype_1}$
@;      and\ $~\vdash \svalue_0 : \stype_0$
@;      then\ $~\vdash \esubst{\sexpr_0}{\svar_0}{\svalue_0} : \stype_1$
@;  \end{itemize}
@;\end{lemma}
@;}|

@exact|{
\begin{lemma}\label{lemma:both:su}
  If\ $\sWTS \sexpr_0 : \sshape_0$
  then\ $\sWTU \sexpr_0 : \tdyn$
\end{lemma}
\begin{proof}
  By definition.
  The key rules are for shape-annotated functions.
\end{proof}
}|

@;@exact|{
@;\begin{lemma}[type in-hole]
@;  If\ $~\vdash \finhole{\sctx_0}{\sexpr_0} : \stspec_0$
@;  then\ $\fexistsone{\stspec_1} \vdash \sexpr_0 : \stspec_1$
@;\end{lemma}
@;}|

@;@exact|{
@;\begin{lemma}[type replace]
@;  If\ $~\vdash \finhole{\sctx_0}{\sexpr_0} : \stspec_0$
@;  and\ $~\vdash \sexpr_0 : \stspec_1$
@;  and\ $~\vdash \sexpr_1 : \stspec_1$
@;  then\ $~\vdash \finhole{\sctx_0}{\sexpr_1} : \stspec_0$
@;\end{lemma}
@;}|

@exact|{
\begin{lemma}[boundary-crossing]\label{lemma:both:boundary}\leavevmode
  \begin{itemize}
    \item
      If\ $\sWTlang \svalue_0 : \stspec$
      and\ $\fshapematch{\sshape_0}{\svalue_0}$
      then\ $\sWTS \svalue_0 : \sshape_0$
    \item
      If\ $\sWTS \svalue_0 : \sshape_0$
      then\ $\sWTU \svalue_0 : \tdyn$
    \item
      If\ $\sWTD \svalue_0 : \stype_0$
      and\ $\ewrap{\stype_0}{\svalue_0} \snr \svalue_1$
      then\ $\sWTS \svalue_1 : \ftypemapshape{\stype_0}$
      and\ $\sWTU \svalue_1 : \tdyn$
  \end{itemize}
\end{lemma}
}|

@;@exact|{
@;\begin{lemma}[owner preservation]
@;  If\ $~\vdash \sexpr_0 : \stspec$
@;  and\ $\sowner_0 \Vdash \sexpr_0$
@;  and\ $\sexpr_0 \snr \sexpr_1$
@;  then\ $\sowner_0 \Vdash \sexpr_1$
@;\end{lemma}
@;}|
@;
@;@exact|{
@;\begin{lemma}[label in-hole]
@;  If\ $\sowner_0 \Vdash \finhole{\sctx_0}{\sexpr_0}$
@;  then\ $\fexistsone{\sowner_1} \sowner_1 \Vdash \sexpr_0$
@;\end{lemma}
@;}|
@;
@;@exact|{
@;\begin{lemma}[label replace]
@;  If\ $\sowner_0 \Vdash \finhole{\sctx_0}{\sexpr_0}$
@;  and\ $\sowner_1 \Vdash \sexpr_0$
@;  and\ $\sowner_1 \Vdash \sexpr_1$
@;  then\ $\sowner_0 \Vdash \finhole{\sctx_0}{\sexpr_1}$
@;\end{lemma}
@;}|

@section[#:tag "sec:both:implementation"]{Implementation}
@; X typed-context? hook = easy
@; X reuse ctc + type = hard
@; x #%module-begin, to reuse contract defs save space ... NVM
@; X require/untyped-contract
@; X define-typed/untyped-id
@; X reuse G macros in S code
@; - manual type env needs manual trust

The implementation of @|sShallow| Racket begins with two new @tt{#lang}
 languages to communicate the options available to programmers.
@itemlist[
@item{
  Modules that start with @tt{#lang typed/racket} continue to use @|sdeep| types,
   same as earlier versions of Typed Racket;
}
@item{
  @tt{#lang typed/racket/deep} is a new way to opt-in to @|sdeep| types;
}
@item{
  and @tt{#lang typed/racket/shallow} provides @|sshallow| types.
}]
@|noindent|All three languages invoke the same type checker.
At steps where @|sdeep| and @|sshallow| disagree,
 the compiler queries the current language to proceed.
For example, the type-directed optimizer checks that it has @|sdeep| types
 before rewriting code based on the @|sdeep| soundness guarantee.
@; Such queries are made possible by a module-level variable.

Many parts of the modified compiler use a similar, one-or-the-other strategy
 to handle @|sdeep| and @|sshallow| types.
This section deals with the more challenging aspects.
Sharing variables between @|sdeep| and @|sshallow| required changes to
 type-lookup and wrapper generation (@sectionref{sec:both:impl:code}).
Sharing macros requires further changes; currently, @|sdeep|-typed syntax can only
 be re-used through unsafe mechanisms (@sectionref{sec:both:impl:code}).
Lastly, Typed Racket has a small API that gives programmers control
 over the @|sdeep| type enforcement strategy.
This API needed generalizations to handle @|sshallow| types (@sectionref{sec:both:impl:tu}).


@subsection[#:tag "sec:both:impl:code"]{@|sDeep| and @|sShallow| Interaction}

Racket supports separate compilation.
Each module in a program gets compiled to a core language individually, and
 other modules can re-use the output.
Typed Racket cooperates with the separate compilation protocol by serializing
 the results of type checking.
A well-typed module compiles to untyped code (with appropriate contracts)
 and a local type environment.
When one @|sdeep| module imports from another, it can find the type of the
 imported identifier in the type environment.

At first glance, it appears that @|sshallow| code can use the same protocol
 to find the type of @|sdeep| imports.
The protocol fails, however, because wrappers get in the way.
When @|sdeep| wants to provide an identifier, it really provides a piece of
 syntax called a rename transformer.
These transformers expand to one of two identifiers depending on where they
 appear: @|sdeep|-typed code gets the original identifier and can easily
 look up its type, but @|suntyped| and @|sshallow| code gets a wrapped
 version.
The wrapper causes a direct type lookup to fail.

For @|sdeep|-to-@|sshallow| exports, the solution is to modify
 type lookup to pass through wrappers.
Fortunately, the change was easy to make because the Racket contract library
 provides enough metadata.
At compile time (and only then), a wrapped identifier is associated with
 a structure that links back to the original.
The @|sshallow| type checker looks out for these wrappers and uncovers
 the originals as needed.

@|sShallow|-to-@|sdeep| exports use a dual method.
Like @|sdeep|, a @|sshallow| module provides only rename transformers.
These expand to the original identifier in other @|sshallow| and @|suntyped|
 code; the original is associated with type information.
For @|sdeep| clients, the transformers expand to a wrapped identifier.
Consequently, the @|sdeep| type checker watches for ``untyped'' wrappers and
 tests whether there is an available type.
Such types allow static type checks to succeed, and at run-time the wrapper
 keeps @|sdeep| code safe.

A surprising consequence of the final protocol is that a @|sshallow| module must
 be prepared to create wrappers for its exports.
The wrapper-making code is generated during compilation, at the end of
 type checking, but it does not run until needed by a @|sdeep| client.
In this way, only @|sdeep| code appears to suffer from the expressiveness
 limits of wrappers.


@subsection[#:tag "sec:both:impl:syntax"]{Syntax Re-Use}

@|sShallow| code cannot use @|sdeep| macros.
Re-use is desirable to avoid copying code, but it requires a static analysis
 to enforce soundness.
This section explains the problem and suggests requirements for a solution.

To appreciate the problem, consider the following simple macro.
This macro applies a a typed function @tt{f} to an input, and is consequently
 unsafe:

@typed-codeblock['("(define-syntax-rule (call-f x) (f x))")]

@|noindent|If this macro could appear in @|sshallow| code, then any
 @|sshallow| value @tt{x} could sneak into the @|sdeep| function.
Unless @tt{f} makes no assumptions about its input, such values can break
 the @|sdeep| soundness guarantee and lead to dangerous results in optimized
 code.

One possible fix is to put a contract around every @|sdeep| identifier that
 appears in a macro.
Doing so would require an analysis to find out which contracts are needed,
 and a second analysis to install the contracts (ideally without repeats).
It should also be possible to avoid the contracts if the macro goes only to @|sdeep| clients.
These are major changes.

Another possibility is to statically check whether a macro is safe to
 export.
Safe macros appear, for example, in the typed compatibility layer for the
 RackUnit testing library.
RackUnit is an untyped library that exports some functions and some macros.
The typed layer provides types for the functions and type-annotated copies
 of the macros (about 300 lines in total).
These macros are safe because they do not expose any @|sdeep|-typed identifiers.
For example, the following macro combines a sequence of expressions into
 a named RackUnit test case:

@typed-codeblock['(
  "(define-syntax (test-case stx)"
  "  (syntax-parse stx"
  "    [(_ name expr ...)"
  "     (quasisyntax/loc stx"
  "       (parameterize ([test-name (ensure-str name)])"
  "         (test-begin expr ...)))]))"
)]

@|noindent|This macro is safe for @|sshallow| code, but for complicated reasons.
First, @tt{ensure-str} is a typed function that accepts any input.
Second, @tt{test-begin} is a macro from the same file that is also safe.
Third, @tt{parameterize} comes from untyped Racket.

@; ;; rackunit/rackunit-typed/rackunit/main.rkt
@;(define-syntax (test-begin stx)
@;  (syntax-case stx ()
@;    [(_ expr ...)
@;     (syntax/loc stx
@;       ((current-test-case-around)
@;        (lambda ()
@;          (with-handlers ([(λ (e)
@;                             (and (exn:fail? e)
@;                                  (not (exn:test? e))))
@;                           (λ ([e : exn:fail])
@;                             (test-log! #f)
@;                             (raise e))])
@;          (parameterize ([current-check-handler raise])
@;            (void)
@;            expr ...)))))]
@;    [_
@;     (raise-syntax-error
@;      #f
@;      "Correct form is (test-begin expr ...)"
@;      stx)]))

Currently, the author of a @|sdeep| library can enable syntax re-use by disabling the optimizer
 and unsafely providing macros.
This work-around requires a manual inspection, but it is more appealing than
 forking the RackUnit library and asking programmers to choose the correct version.


@subsection[#:tag "sec:both:impl:tu"]{@|sDeep|--@|sUntyped| Utilities}
@; struggles = edit old code , 2 of these => new type errors

Typed Racket has a small API to let programmers control boundaries
 between @|sdeep| and @|suntyped| code.
The API arose over time, as programmers discovered challenges.
Two forms in this API can lead to surprising results due to the existence of
 @|sshallow| code.

@; require/untyped-contract

The first problem concerns @tt{require/untyped-contract}.
This form lets untyped code import a typed identifier whose precise type
 cannot be expressed with a @|sdeep| contract.
Users supply a supertype of the precise type and @|sDeep| Racket uses this
 weaker type to generate a contract.

For example, the @bm{jpeg} benchmark depends on a library for multi-dimensional
 arrays (@render-lib[(make-lib "math/array" "https://docs.racket-lang.org/math/array.html")]).
This library accepts two kinds of data for array indices:
 either a vector of natural numbers or a vector of integers.
Helper functions assert that values with the integer type do not actually
 contain negative numbers using a run-time checking function:

@exact{\smallskip}
@typed-codeblock['(
  "(: check-array-shape"
  "   (-> (U (Vectorof Natural) (Vectorof Integer))"
  "       (Vectorof Natural)))")]

@|noindent|@|sDeep| contracts cannot express the type for the checking
 function because they lack support for true unions.
The work around is to impose a supertype on untyped clients:

@exact{\smallskip}
@untyped-codeblock['(
  "(require/untyped-contract"
  "  [check-array-shape"
  "   (-> (Vectorof Integer) (Vectorof Natural))])")]

This form comes with a surprising design choice.
If an untyped-contract identifier flows back into typed code,
 the type checker uses the original type rather than the supertype.
For @|sdeep| code, the choice is convenient because more programs can type-check
 using the supertype.
For @|sshallow|, though, the convenience disappears.
A @|sshallow| client must receive the wrapped version of the identifier,
 which means @|sshallow| code must behave in accordance with the supertype;
 hence, the @|sshallow| type checker uses the supertype as well.
Consequently, some well-typed @|sdeep| programs raise type errors upon switching
 to @|sshallow| types.

@; define-typed/untyped-identifier
@; TODO more extreme version of untyped-contract

The second problematic form is @tt{define-typed/untyped-identifier},
 which creates a new identifier from two old ones.
The following example defines @tt{f} from two other names:

@exact{\smallskip}
@typed-codeblock['(
  "(define-typed/untyped-identifier f"
  "  typed-f"
  "  untyped-f)")]

@|noindent|The meaning of the new @tt{f} depends on the context in which it appears.
In typed code, @tt{f} expands to @tt{typed-f}.
In untyped code, an @tt{f} is a synonym for @tt{untyped-f}.

The @tt{typed-f} is intended for @|sdeep|-typed code.
It cannot be safely used in a @|sshallow| module because it may
 assume type invariants.
Consequently, @|sshallow| code gets the untyped id.
This means, unfortunately, that changing a @|sdeep| module to @|sshallow|
 can raise a type checking error because occurrences of @tt{f} that expand to
 @tt{untyped-f} are plain, untyped identifiers.
There is no way to uncover the type that a @tt{typed-f} would have, and
 anyway there is no guarantee that @tt{typed-f} and @tt{untyped-f} have
 the same behavior.

For now, such type errors call for programmer-supplied annotations in
 the @|sshallow| client code.
In the future, this  @;@tt{define-typed/untyped-identifier}
 form would benefit
 from a third argument that specifies behavior in @|sshallow| contexts.


@section[#:tag "sec:both:evaluation"]{Evaluation}
@; @; Evaluation, to be determined, 2-way vs 3-way lattice, programs where combination
@; @;  is better than Guarded-alone or Transient-alone.

The integration of @|sShallow| Racket and @|sDeep| Racket has implications for
 expressiveness (@sectionref{sec:both:expressiveness}) and performance (@sectionref{sec:both:performance}).
Switching between these two type-enforcement strategies can help programmers
 express new designs and avoid huge performance costs.


@subsection[#:tag "sec:both:expressiveness"]{Expressiveness}
@; - TODO can we fix occurrence typing? Any -> box? -> set-box! ???
@;    ditto for objects, avoid the cast, is it worth changing the typechecker?
@; - (Syntaxof (-> Int Int)) ... new mixed programs that TR doesn't allow
@; - higher-order / any errors, gone
@; - indexof ... weird result, gone

Conversations with Typed Racket users have shown that @|sdeep| types can
 lead to unexpected outcomes.
In some programs, type enforcement appears overly strict.
In others, type enforcement is impossible because the implementation of
 @|sDeep| Racket lacks wrappers for certain kinds of values.
Worst of all, the wrappers that @|sDeep| inserts can change hehavior.
@|sShallow| Racket avoids all of these issues because of its weak, wrapper-free
 method of enforcing types.

@;@(let* ((qa-dir "../QA/transient-expressive")
@;        (search-begin "2020-08-19")
@;        (search-end "2019-12-13")
@;        (num-tr-q 6)
@;        (num-s-win 3)
@;       )
@;@elem{
@;To assess whether @|sShallow| Racket could help express designs that
@; programmers want to use, I searched the Racket mailing list for
@; questions about Typed Racket from @|search-begin| back to @|search-end|.
@;In total, @integer->word[num-tr-q] questions asked about Typed Racket errors.
@;Changing to @|sShallow| Racket caused @integer->word[num-s-win] errors
@; to disappear.
@;Other problems were due to type checking, or a run-time issue that @|sShallow|
@; Racket does not change.
@;The conclusion is: yes, @|sshallow| types can make some programmer-created
@; designs expressible.
@;
@;That said, there is a risk that further development could run into a delayed
@; error, and perhaps one that the programmer cannot even articulate as a mailing
@; list question.
@;But for now I declare victory that @|sshallow| is more
@; expressive.
@;})

@subsubsection{Less-strict Any Type}

@user-inspiration['(
 "https://groups.google.com/g/racket-users/c/cCQ6dRNybDg/m/CKXgX1PyBgAJ"
 "https://groups.google.com/g/racket-users/c/jtmVDFCGL28/m/jwl4hsjtBQAJ")]

The @|sdeep| type named @tt{Any} is a normal ``top'' type at compile-time,
 but it is surprisingly strict at run-time.
For compile-time type checking, @tt{Any} is a supertype of every other
 type.
A function that expects an @tt{Any} input must ask occurrence-typing questions
 before it can do anything to it.
At run-time, the @tt{Any} type is enforced with an opaque wrapper.

@figure*[
  "fig:both:any-wrap"
  @elem{@|sDeep| seals mutable values of type @tt{Any} in a wrapper. @|sShallow| lets untyped code modify the box.}

  both:any-wrap]

The wrapper is a surprise for developers who expect programs such
 as @figure-ref{fig:both:any-wrap} to run without error.
This program defines a mutable box in typed code,
 assigns the @tt{Any} type to the box,
 and sends it to untyped code.
The untyped module attempts to set the box.
@|sDeep| Racket raises an exception when untyped code tries to modify the box.
Unfortunately for the programmer, this error is essential for soundness.
If untyped code put an integer in the box, then typed uses of the
 box would give a result that is inconsistent with its type.

@|sShallow| Racket runs the program without error because of its delayed
 checking strategy.
If @|sshallow|-typed code tries to read a symbol from the
 box, then that access will raise an error.
Until then, the program runs.


@subsubsection{No Missing Wrappers}

Every kind of mutable value that can appear in @|sdeep| code needs a kind of
 wrapper to protect it against untyped contexts.
Wrappers do not exist for some values, causing @|sDeep| to reject code
 that sends such a value across a boundary.

@figure*[
  "fig:both:no-wrap"
  @elem{@|sDeep| lacks wrappers for mutable pairs and a few other datatypes. @|sShallow| does not need wrappers, and can express mixed-typed programs that share such values with untyped code.}
  both:no-wrap]

@Figure-ref{fig:both:no-wrap} demonstrates the issue with a mutable pair
 (@tt{MPairof}) type.
@|sDeep| raises a run-time error when untyped code tries to call the @tt{add-mpair}
 function.
@(let* ((missing-wrapper* '(
          "(Async-Channel T)" "(Custodian-Box T)" "(C-Mark-Key T)" "(Evt T)"
          "(Ephemeron T)" "(Future T)" "(MPair T T')" "(MList T)"
          "(Prompt-Tag T T')" "(Syntax T)" "(Thread-Cell T)" "(Weak-Box T)"))
        (num-missing (length missing-wrapper*)))
  @elem{
In total, there are @integer->word[num-missing] types that
 suffer from this issue.
Implementing wrappers for these types is a challenge.
For example, syntax objects can contain mutable data and therefore need wrappers.
But syntax wrappers would require changes to many parts of the Racket compiler,
 including the macro expander.
})

@|sShallow| Racket avoids the question of wrappers thanks to the @|sTransient|
 semantics.
Consequently, programmers gain the ability to send new types across boundaries
 and explore new mixed-typed designs.


@subsubsection{Uniform Behavior}

@user-inspiration['(
 "https://groups.google.com/g/racket-users/c/UD20HadJ9Ec/m/Lmuw0U8mBwAJ"
 "https://groups.google.com/g/racket-users/c/ZbYRQCy93dY/m/kF_Ek0VvAQAJ")]

Although the purpose of @|sDeep| Racket wrappers is to reject certain operations
 without changing anything else about a program, wrappers can cause some
 programs to run differently.
One obvious case is code that explicitly looks for wrappers; the answers to
 low-level observations such as @tt{has-contract?} may depend on the type
 boundaries in a @|sdeep| program.
@Figure-ref{fig:both:index-of} presents a second, more subtle case.
This typed module imports an untyped function, @tt{index-of}, with a precise
 polymorphic type.
The wrapper that enforces this type
 creates a new wrapper for every input to the function---to enforce parametric
 polymorphism@~cite{gmfk-dls-2007}.
Unfortunately, these input wrappers change the behavior of @tt{index-of};
 it ends up searching the list for a wrapped version of the symbol @tt{'a} and returns
 a ``not found'' result (@tt{#f}) instead of the correct position.

@figure*[
  "fig:both:index-of"
  @elem{The @|sdeep| contract for an @tt{All} type can change the behavior of untyped code.}
  both:index-of]

@|sShallow| Racket avoids all such changes in behavior because the @|sTransient|
 semantics does not use wrappers to enforce types.


@subsection[#:tag "sec:both:performance"]{Performance}
@; - worst-case table (can trace "min" line in "fig:transient:overhead"
@; - 
@; - ?? 2-way lattice? 3-way
@; - ?? programs where mix is better than natural-only or transient-only

With the @|sShallow| Racket implementation, the tradeoffs of @chapter-ref{chap:transient} disappear.
For all our benchmarks, the choice improves the worst-case overhead of
 type boundaries.
By implication, Typed Racket can offer a new migration story:

@nested-inset[@emph{
 use @|sshallow| types when converting an untyped application and switch to
 @|sdeep| types after the boundaries stabilize.}]

Mixing @|sdeep| and @|sshallow| types in one program offers new ways of
 improving performance.


@subsubsection[#:tag "sec:both:perf:worst"]{GTP Benchmarks, Worst-Case}

@(let* ((WT (get-mixed-worst-table SHALLOW-CURRENT-BENCHMARK*))
       )
@list[
@figure*[
  "fig:both:mixed-worst-table"
  @elem{
   Worst-case overhead before (@|sdeep| types)
    and after (either @|sdeep| or @|sshallow|)
    the integration of @|sDeep| and @|sShallow| Racket.
  }
  @render-mixed-worst-table[WT]
]
@elem{
Now that Racket programmers can easily switch between @|sdeep| and @|sshallow|
 types, worst-case overheads improve by orders of magnitude.
Before, the cost of @|sdeep| types overwhelmed many configurations.
After, the costs can be avoided by changing the first line (the language specification)
 of the typed modules.

@Figure-ref{fig:both:mixed-worst-table} quantifies the improvements in the
 Typed Racket benchmarks.
The first data column reports the old worst-case overheads.
The second columns reports the new worst-case, now that programmers can
 pick the best of @|sdeep| and @|sshallow| types.
The final column is the quoient between the first two.
In short, the ``after'' case is always better and can be an arbitrarily large
 improvement.
}])


@;@subsubsection[#:tag "sec:both:perf:path"]{Migration Paths}
@;
@;@(let* ([bm* '(sieve forth fsm fsmoo mbta morsecode
@;               zombie dungeon jpeg zordoz lnm suffixtree
@;               kcfa snake take5)]
@;        [D 3]
@;        [PT (get-mixed-path-table D bm*)]
@;        [deep-dead* (for/list ((path-info (in-list PT)) #:when (string=? "0" (caddr path-info))) (car path-info))]
@;        [shallow-dead* (for/list ((path-info (in-list PT)) #:when (string=? "0" (cadddr path-info))) (car path-info))]
@;        [mix-dead* (for/list ((path-info (in-list PT)) #:when (string=? "0" (cadddr (cdr path-info)))) (car path-info))]
@;        [_md (unless (= 1 (length mix-dead*)) (printf "HELP expected one mix-dead row got ~s~n" mix-dead*))]
@;        [mix-path-bm 'fsm]
@;        [mix-best-D (find-lowest-3dpath-D mix-path-bm)]
@;        )
@;@list[
@;@figure*[
@;  "fig:both:mixed-path"
@;  @elem{Percent of @ddeliverable[D] paths in three lattices:
@;   the @|sdeep|-typed lattice, the @|sshallow|-typed lattice,
@;   and a hybrid that chooses the best of @|sdeep| or @|sshallow| types at each point.}
@;  @render-mixed-path-table[PT]
@;]
@;@elem{
@;@|sShallow| types make step-by-step migration more practical in Typed Racket.
@;Originally, with @|sdeep| types, a programmer who adds types one module at
@; a time is likely to hit a performance wall; that is, a few configurations
@; along the migration path are likely to suffer a large overhead.
@;Adding more @|sdeep| types is a sure way to reduce the overhead,
@; especially if the programmer adds the best-possible types (@figure-ref{fig:example-path}),
@; but these multi-step pitfalls contradict the promise of migratory typing.
@;High overhead makes it hard to tell whether the new types are compatible with
@; the rest of the codebase.
@;
@;By choosing @|sdeep| or @|sshallow| types at each point along a path, the
@; worst-case overhead along migration paths goes down.
@;@Figure-ref{fig:both:mixed-path} quantifies the improvement by showing the
@; percent of all paths that are @ddeliverable[D] at each step.
@;With @|sdeep| types alone, all paths in @integer->word[(length deep-dead*)]
@; benchmarks hit a point that exceeds the @~a[D]x limit.
@;With @|sshallow| types alone, all paths in @integer->word[(length shallow-dead*)] benchmarks
@; exceed the limit as well.
@;With the mix, however, only @integer->word[(length mix-dead*)] benchmark (@bm[(car mix-dead*)])
@; has zero @ddeliverable[D] paths.
@;Fine-grained combinations of @|sdeep| and @|sshallow| types can further improve
@; the number of viable migration paths.
@;In @bm[mix-path-bm], for example, every path is @ddeliverable[mix-best-D] if the programmer
@; picks the fastest-running mix of @|sdeep| and @|sshallow| types for each configuration.
@;}])


@subsubsection[#:tag "sec:both:perf:both"]{Case Studies: @|sDeep| and @|sShallow|}
@; Are there any mixed lattice points, using guarded and transient, that do
@;  better than a "pure" configuration?
@; For a negative answer, need a lattice on top of every lattice point.
@; Can try small benchmarks.... ok, then extrapolate.

Early experience with @|sShallow| Racket shows that the combination of
 @|sdeep| and @|sshallow| types can be better that either alone.
Here are three motivating case studies.


@parag{synth}
@(let* ((synth-url "http://github.com/stamourv/synth")
        (synth-data
         (hash
           'd-lib-d-client '(809 821 834 771 733)
           'd-lib-u-client '(11440 11040 11004 11923 11672)
           's-lib-u-client '(1645 1664 1558 1576 1539)
           's-lib-d-client '(7823 7885 9002 7955 8279)))
        (deep-delta (exact-round (/ (mean (hash-ref synth-data 'd-lib-u-client))
                                    (mean (hash-ref synth-data 'd-lib-d-client)))))
        (shallow-delta (exact-round (/ (mean (hash-ref synth-data 's-lib-d-client))
                                       (mean (hash-ref synth-data 's-lib-u-client)))))
        (ds-fast (exact-round (/ (mean (hash-ref synth-data 's-lib-u-client))
                                 (mean (hash-ref synth-data 'd-lib-d-client)))))
        (ds-slow-sec (quotient
                       (- (mean (hash-ref synth-data 'd-lib-u-client))
                          (mean (hash-ref synth-data 's-lib-d-client)))
                       1000))
        (ds-slow (rnd (/ (mean (hash-ref synth-data 'd-lib-u-client))
                         (mean (hash-ref synth-data 's-lib-d-client))))))
  @elem{
The @bm{synth} benchmark is derived from @hyperlink[synth-url]{an untyped program}
 that interacts with part of a typed math library.
When the library code uses @|sdeep| types, the original client runs with
 high overhead---@~a[deep-delta]x slower that a @|sdeep|-typed client.

Changing the library to use @|sshallow| types improves
 the gap between an untyped and @|sdeep|-typed client to
 @~a[shallow-delta]x.
This fast untyped configuration is about @~a[ds-fast]x slower than the fast
 @|sdeep|-@|sdeep| configuration, but the worst-case is @~a[ds-slow]x
 faster (@~a[ds-slow-sec] seconds) than before.
Overall, the @|sshallow| library is a better tradeoff for @bm{synth}.
})

@; if we have a few of these, organize into a figure with descriptions below
@; ... or use a "benchmarks" format to convey the bottom line
@parag{MsgPack}
@hyperlink["http://msgpack.org/"]{MessagePack} is a serialization format.
@hyperlink["https://gitlab.com/HiPhish/MsgPack.rkt"]{MsgPack} is a Typed Racket
 library that maps Racket values to binary data according to the format.
The author of this library
 @hyperlink["https://groups.google.com/g/racket-users/c/6KQxpfMLTn0/m/lil_6qSMDAAJ"]{reported a performance hit}
 after narrowing some types from @tt{Any} to a more-precise union type for serializable inputs.
Tests that formerly passed on the package server timed out after the change.

I cloned MsgPack commit @github-commit["HiPhish" "MsgPack.rkt"]{64a60986b149703ff9436877da1dd3e86c6e4094}
 and found that running all unit tests took 320 seconds.
Changing one file to @|sshallow| types brought the time down to 204 seconds---a
 huge improvement for a one-line switch.
Moving the rest of the library from @|sdeep| to @|sshallow| types adds only a slight
 improvement (down to 202 seconds), which suggests that a mix of @|sdeep| and
 @|sshallow| is best.

@; can do even better after changing the code:
@;  Deep, no pack casts = 117 seconds
@;  Shallow, no pack casts = 67 seconds
@;  untyped = 24 seconds!!!


@parag{External Data}
Typed code that deals with data from an external source is often better off
 with @|sshallow| types because they lazily validate data as it is accessed.
By contrast, the @|sdeep|-type guarantee requires a full traversal to validate
 data as soon as it reaches a type boundary.
If the boundary types allow mutable values, then the traversal is even more
 expensive because it creates wrappers as it copies the dataset.

@(let* ((script_name "QA/transient-expressive/json/custom.rkt")
        (s* '(169 157 162 159 162))
        (t* '(3007 2991 2920 3096 3308))
        (t/s (/ (mean t*) (mean s*)))
        (slowdown (if (< 10 t/s) "over 10x" (format "~ax" (rnd t/s)))))
  @elem{
To illustrate the pitfall, I wrote a typed script that reads a large dataset of
 apartment data using on off-the-shelf JSON parser and accesses one field
 from each object in the dataset.
@|sDeep| types make the script run @|slowdown| slower than @|sshallow| types.
})

In principle, @|sdeep| code can avoid the slowdown with a custom parser
 that validates data as it reads it.
Indeed, Phil Nguyen has written a @hyperlink["https://github.com/philnguyen/json-type-provider"]{library}
 for JSON that mitigates the overhead of @|sdeep| types.
Such libraries are ideal, but until we have them for the next data exchange
 format (SQL, XML, YAML, ...) @|sshallow| types get the job done with the parsers
 that are available today.


@;@parag{GTP Benchmarks}
@;
@;@(let* ((DDD (get-3d-table '(fsm morsecode jpeg kcfa zombie zordoz)))
@;        (num-DDD (length DDD))
@;        (S (tr:benchmark-name->performance-info 'fsm tr:default-rkt-version))
@;        (fsm-num-modules (performance-info->num-units S))
@;        (fsm-num-configs (expt 2 fsm-num-modules))
@;        (fsm-non-mixed (+ 1 fsm-num-modules))
@;        (fsm-mixed (- fsm-num-configs fsm-non-mixed)))
@;@list[
@;@elem{
@;  For @integer->word[num-DDD] small benchmarks, I measured the full
@;   space of @${3^N} configurations that can arise by combining @|sdeep|
@;   and @|sshallow| types.
@;  Each configuration ran successfully, affirming that @|sdeep| and @|sshallow|
@;   can interoperate.
@;  Furthermore, a surprising percent of all @${2^N} mixed-typed configurations
@;   in each benchmark ran fastest using a mixture of @|sdeep| and @|sshallow|
@;   types:
@;}
@;@(apply itemlist
@;   (for/list ((d-row (in-list DDD))
@;              (i (in-naturals 1)))
@;     (item (format "~a% of " (cadr d-row))
@;           (bm (car d-row))
@;           (format " configurations~a"
@;                   (cond
@;                    [(= i num-DDD)
@;                     "."]
@;                    [(= (+ i 1) num-DDD)
@;                     "; and"]
@;                    [else
@;                     ";"])))))
@;@elem{
@;@|noindent|In @bm{fsm}, for example, there are @integer->word[fsm-num-configs] mixed-typed configurations.
@;@Integer->word[fsm-non-mixed] of these cannot mix @|sdeep| and @|sshallow|
@; because they contain at most one typed module.
@;Of the remaining @~a[fsm-mixed] configurations, over half run fastest with a
@; combination of @|sdeep| and @|sshallow| types.
@;}
@;])


@; Last case study, three way lattice, how many points best with mix?
@; Prohibitive experiment, now 3n for normal points a whole lattice to explore at
@;  each, but also the both files can be toggled making an extra 2m for the m both
@;  files.
@; Those are the adaptors, extreme example is kcfa adds 4 for adaptor.
@; future work


@subsubsection[#:tag "sec:both:perf:release"]{Release Information}

@|sShallow| Typed Racket is publicly available in a pull request to Typed Racket:
@github-pull["racket" "typed-racket" "948"].
The patch adds support for @|sshallow| types, giving Typed Racket programmers
 a choice between @|sshallow| and @|sdeep| type guarantees.
I expect to merge the pull request early in 2021.
After the release, I look forward to studying programmers' experience with
 the multi-faceted system.


