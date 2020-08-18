#lang greenman-thesis/include
@(require
   (only-in greenman-thesis/shallow/main
     SHALLOW-CURRENT-BENCHMARK*
     get-mixed-worst-table
     render-mixed-worst-table
     s:cache-dir)
   (only-in greenman-thesis/oopsla-2019/pict
     both:model-interaction)
   (only-in math/statistics
     mean))

@title{Guarded and Transient, Combined}
@; TODO
@;   Might be able to prove a ``tag error'' lemma, but the elimination forms
@;    currently don't tell between static-typed and untyped


First, simple model of the combination.
Formal properties preserved.
Failed attempts at reducing cross-checks between Guarded and Transient.

New expressiveness, Transient TR allows boundaries that Guarded TR does not.

Implementation challenges, require/untyped-contract and others.
 define-typed/untyped-id, S-require-T-id hygienic

Evaluation, to be determined, 2-way vs 3-way lattice, programs where combination
 is better than Guarded-alone or Transient-alone.

Threats, especially no blame in Transient.


@section[#:tag "sec:both:model"]{Model and Properties}
@; - simple model
@;   - no worries, properties still hold
@;   - failed attempts at natural/transient cooperation to reduce checks,
@;     possible futures (forgetful)

This model compiles our three kinds of types down to a common, simple runtime.
The compiler removes data about what languages are sitting around a boundary;
 this is similar to the information that the Racket runtime has about checks
 generated via Typed Racket.

Ingredients:

@itemlist[
@item{
  1 surface language
}
@item{
  1 evaluation language + semantics
}
@item{
  3 linked surface type judgments
}
@item{
  3 linked surface-to-evaluation compilers
}
@item{
  3 linked evaluation type judgments
}
@item{
  1 consistent ownering judgment
}
]

The question is: can the language satisfy ``complete monitoring''
 if the compilers remove type and boundary info?

In the end, we need no types and three kinds of cast expression in the
 evaluation language.
Each kind of cast represents the ``most-concerned'' party:
 T and anyone gives T, U to S gives S, and S to U gives U.
The three evaluation sub-languages can mix via the cast expressions and the
 result satisfies both type soundness (exact guarantee varies by surface sub-lang)
 and complete monitoring (for ``honest'' types only).


@; Similar to @chapter-ref{chap:design}, but simpler more realistic .. condensed?
@; Worth repeating.


@subsection{Syntax}

Surface expresssions $\ssurface$,
 types $\stype$,
 type-shapes $\sshape$,
 evaluation expressions $\sexpr$.

@exact|{
\begin{langarray}
  \ssurface & \slangeq &
    \svar \mid \sint \mid \epair{\ssurface}{\ssurface}
    \mid \efun{\svar}{\ssurface}
    \mid \efun{\tann{\svar}{\stype}}{\ssurface}
    \mid \efun{\tann{\svar}{\tfloor{\stype}}}{\ssurface}
    \mid \eunop{\ssurface} \mid \ebinop{\ssurface}{\ssurface} \mid \eapp{\ssurface}{\ssurface}
    \mid \emod{\slang}{\ssurface}
  \\
  \stype & \slangeq &
    \tnat \mid \tint \mid \tpair{\stype}{\stype} \mid \tfun{\stype}{\stype}
  \\
  \sshape & \slangeq &
    \knat \mid \kint \mid \kpair \mid \kfun \mid \kany
  \\
  \stspec & \slangeq &
    \stype \mid \tfloor{\stype} \mid \tdyn
  \\
  \slang & \slangeq &
    \sT \mid \sS \mid \sU
  \\
  \sexpr & \slangeq &
    \svar \mid \sint \mid \epair{\sexpr}{\sexpr}
    \mid \efun{\svar}{\sexpr}
    \mid \efun{\tann{\svar}{\stype}}{\sexpr}
    \mid \efun{\tann{\svar}{\sshape}}{\sexpr}
    \mid \emon{\stype}{\svalue}
    \mid \eunop{\sexpr} \mid \ebinop{\sexpr}{\sexpr} \mid \eapp{\sexpr}{\sexpr}
    \mid \ewrap{\stype}{\sexpr}
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
  \serror & \slangeq &
    \stagerror \mid \sscanerror \mid \swraperror \mid \sdivzeroerror
  \\
  \sctx & \slangeq &
    \sctxhole \mid \eunop{\sctx} \mid \ebinop{\sctx}{\sexpr} \mid \ebinop{\svalue}{\sctx}
    \mid \eapp{\sctx}{\sexpr} \mid \eapp{\svalue}{\sctx} \mid \enoop{\sctx}
    \mid \escan{\sshape}{\sctx} \mid \ewrap{\stype}{\sctx}
\end{langarray}
}|


@subsection{Surface Typing}

Module expressions separate different surface languages.

A $\tfloor{\stype_0}$ is a (decorated) full type, not a shape.
The typing rules for these decorated types are similar to the rules
 for normal types, but these types give weaker guarantees; the notation
 is meant to illustrate the weaker-ness.

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
}|


@subsection{Completion (aka Compilation)}

Replace module boundaries with wraps, scans, and no-ops

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

  \inferrule*{
  }{
    \stypeenv_0 \sST \sint_0 : \tdyn \scompile \sint_0
  }

  \inferrule*{
  }{
    \stypeenv_0 \sST \sint_0 : \stype_0 \scompile \sint_0
  }

  \inferrule*{
  }{
    \stypeenv_0 \sST \sint_0 : \tfloor{\stype_0} \scompile \sint_0
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \tdyn \scompile \sexpr_2
    \\
    \stypeenv_0 \sST \sexpr_1 : \tdyn \scompile \sexpr_3
  }{
    \stypeenv_0 \sST \epair{\sexpr_0}{\sexpr_1} : \tdyn \scompile \epair{\sexpr_2}{\sexpr_3}
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \stype_0 \scompile \sexpr_2
    \\
    \stypeenv_0 \sST \sexpr_1 : \stype_1 \scompile \sexpr_3
  }{
    \stypeenv_0 \sST \epair{\sexpr_0}{\sexpr_1} : \tpair{\stype_0}{\stype_1} \scompile \epair{\sexpr_2}{\sexpr_3}
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \tfloor{\stype_0} \scompile \sexpr_2
    \\
    \stypeenv_0 \sST \sexpr_1 : \tfloor{\stype_1} \scompile \sexpr_3
  }{
    \stypeenv_0 \sST \epair{\sexpr_0}{\sexpr_1} : \tfloor{\tpair{\stype_0}{\stype_1}} \scompile \epair{\sexpr_2}{\sexpr_3}
  }

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
    \fcons{\tann{\svar_0}{\stype_0}}{\stypeenv_0} \sST \sexpr_0 : \stype_1 \scompile \sexpr_1
  }{
    \stypeenv_0 \sST \efun{\tann{\svar_0}{\stype_0}}{\sexpr_0} : \tfun{\stype_0}{\stype_1} \scompile \efun{\tann{\svar_0}{\stype_0}}{\sexpr_1}
  }

  \inferrule*{
    \fcons{\tann{\svar_0}{\tfloor{\stype_0}}}{\stypeenv_0} \sST \sexpr_0 : \tfloor{\stype_1} \scompile \sexpr_1
  }{
    \stypeenv_0 \sST \efun{\tann{\svar_0}{\tfloor{\stype_0}}}{\sexpr_0} : \tfloor{\tfun{\stype_0}{\stype_1}} \scompile \efun{\tann{\svar_0}{\sshape_0}}{\sexpr_1}
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \tdyn \scompile \sexpr_2
    \\
    \stypeenv_0 \sST \sexpr_1 : \tdyn \scompile \sexpr_3
  }{
    \stypeenv_0 \sST \eapp{\sexpr_0}{\sexpr_1} : \tdyn \scompile \eapp{\sexpr_2}{\sexpr_3}
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \tfun{\stype_1}{\stype_0} \scompile \sexpr_2
    \\
    \stypeenv_0 \sST \sexpr_1 : \stype_1 \scompile \sexpr_3
  }{
    \stypeenv_0 \sST \eapp{\sexpr_0}{\sexpr_1} : \stype_0 \scompile \eapp{\sexpr_2}{\sexpr_3}
  }

  \inferrule*{
    \stypeenv_0 \sST \sexpr_0 : \tfloor{\tfun{\stype_1}{\stype_0}} \scompile \sexpr_2
    \\
    \stypeenv_0 \sST \sexpr_1 : \tfloor{\stype_1} \scompile \sexpr_3
  }{
    \stypeenv_0 \sST \eapp{\sexpr_0}{\sexpr_1} : \tfloor{\stype_0} \scompile \escan{\sshape_0}{(\eapp{\sexpr_2}{\sexpr_3})}
  }

  (unop, binop)

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
    \stypeenv_0 \sST \emod{\sslang}{\sexpr_0} : \tdyn \scompile \enoop{\sshape_0}{\sexpr_1}
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
}|


@subsection{Reduction Relation}
one simple reduction relation for everyone

@exact|{
\begin{rrarray}
  \eunop{\svalue_0} & \snr
  & \stagerror
  \\
  \eunop{\svalue_0} & \snr
  & \sdelta(\sunop, \svalue_0)
  \\
  \ebinop{\svalue_0}{\svalue_1} & \snr
  & \stagerror
  \\
  \ebinop{\svalue_0}{\svalue_1} & \snr
  & \sdelta(\sbinop, \svalue_0, \svalue_1)
  \\
  \eapp{\svalue_0}{\svalue_1} & \snr &
  \stagerror
  \\
  \eapp{(\efun{\svar_0}{\sexpr_0})}{\svalue_0} & \snr
  & \esubst{\sexpr_0}{\svar_0}{\svalue_0}
  \\
  \eapp{(\efun{\tann{\svar_0}{\stype_0}}{\sexpr_0})}{\svalue_0} & \snr
  & \esubst{\sexpr_0}{\svar_0}{\svalue_0}
  \\
  \eapp{(\efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0})}{\svalue_0} & \snr
  & \sscanerror
  \\
  \eapp{(\efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0})}{\svalue_0} & \snr
  & \esubst{\sexpr_0}{\svar_0}{\svalue_0}
  \\
  \eapp{(\emon{\tfun{\stype_0}{\stype_1}}{\svalue_0})}{\svalue_1} & \snr
  & \ewrap{\stype_1}{(\eapp{\svalue_0}{(\ewrap{\stype_0}{\svalue_1})})}
  \\
  \enoop{\svalue_0} & \snr
  & \svalue_0
  \\
  \escan{\sshape_0}{\svalue_0} & \snr
  & \sscanerror
  \\
  \escan{\sshape_0}{\svalue_0} & \snr
  & \svalue_0
  \\
  \ewrap{\stype_0}{\svalue_0} & \snr
  & \swraperror
  \\
  \ewrap{\tfun{\stype_0}{\stype_1}}{\svalue_0} & \snr
  & \emon{\tfun{\stype_0}{\stype_1}}{\svalue_0}
  \\
  \ewrap{\tpair{\stype_0}{\stype_1}}{\epair{\svalue_0}{\svalue_1}} & \snr
  & \epair{\ewrap{\stype_0}{\svalue_0}}{\ewrap{\stype_1}{\svalue_1}}
  \\
  \ewrap{\sshape_0}{\svalue_0} & \snr
  & \svalue_0
\end{rrarray}
}|


@subsection{Evaluation Typing}

three rules, one for each kind of surface expression

@exact|{
\begin{mathpar}
  \inferrule*{
    \tann{\svar_0}{\sdyn} \in \stypeenv_0
  }{
    \stypeenv_0 \sWTU \svar_0 : \sdyn
  }

  \inferrule*{
  }{
    \stypeenv_0 \sWTU \sint_0 : \sdyn
  }

  \inferrule*{
    \stypeenv_0 \sWTU \sexpr_0 : \sdyn
    \\
    \stypeenv_0 \sWTU \sexpr_1 : \sdyn
  }{
    \stypeenv_0 \sWTU \epair{\sexpr_0}{\sexpr_1} : \sdyn
  }

  \inferrule*{
    \fcons{\tann{\svar_0}{\sdyn}}{\stypeenv_0} \sWTU \sexpr_0 : \sdyn
  }{
    \stypeenv_0 \sWTU \efun{\svar_0}{\sexpr_0} : \sdyn
  }

  \inferrule*{
    \fcons{\tann{\svar_0}{\sshape_0}}{\stypeenv_0} \sWTS \sexpr_0 : \sshape_1
  }{
    \stypeenv_0 \sWTU \efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0} : \sdyn
  }

  \inferrule*{
    \stypeenv_0 \sWTT \svalue_0 : \stype_0
  }{
    \stypeenv_0 \sWTU \emon{\stype_0}{\svalue_0} : \sdyn
  }

  \inferrule*{
    \stypeenv_0 \sWTU \sexpr_0 : \sdyn
  }{
    \stypeenv_0 \sWTU \eunop{\sexpr_0} : \sdyn
  }

  \inferrule*{
    \stypeenv_0 \sWTU \sexpr_0 : \sdyn
    \\
    \stypeenv_0 \sWTU \sexpr_1 : \sdyn
  }{
    \stypeenv_0 \sWTU \ebinop{\sexpr_0}{\sexpr_1} : \sdyn
  }

  \inferrule*{
    \stypeenv_0 \sWTU \sexpr_0 : \sdyn
    \\
    \stypeenv_0 \sWTU \sexpr_1 : \sdyn
  }{
    \stypeenv_0 \sWTU \eapp{\sexpr_0}{\sexpr_1} : \sdyn
  }

  \inferrule*{
    \stypeenv_0 \sWTU \sexpr_0 : \sdyn
  }{
    \stypeenv_0 \sWTU \enoop{\sexpr_0} : \sdyn
  }

  \inferrule*{
    \stypeenv_0 \sWTS \sexpr_0 : \sshape_0
  }{
    \stypeenv_0 \sWTU \enoop{\sexpr_0} : \sdyn
  }

  \inferrule*{
    \stypeenv_0 \sWTU \sexpr_0 : \sdyn
  }{
    \stypeenv_0 \sWTU \escan{\sshape_0}{\sexpr_0} : \sdyn
  }

  \inferrule*{
    \stypeenv_0 \sWTS \sexpr_0 : \sshape_1
  }{
    \stypeenv_0 \sWTU \escan{\sshape_0}{\sexpr_0} : \sdyn
  }

  \inferrule*{
    \stypeenv_0 \sWTT \sexpr_0 : \stype_0
  }{
    \stypeenv_0 \sWTU \ewrap{\stype_0}{\sexpr_0} : \sdyn
  }

  \inferrule*{
  }{
    \stypeenv_0 \sWTU \serror : \sdyn
  }
\end{mathpar}
}|

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
    \fcons{\tann{\svar_0}{\sdyn}}{\stypeenv_0} \sWTU \sexpr_0 : \sdyn
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
    \stypeenv_0 \sWTS \eapp{\sexpr_0}{\sexpr_1} : \kany
  }

  \inferrule*{
    \stypeenv_0 \sWTS \sexpr_0 : \sshape_0
  }{
    \stypeenv_0 \sWTS \enoop{\sexpr_0} : \sshape_0
  }

  \inferrule*{
    \stypeenv_0 \sWTS \sexpr_0 : \sdyn
  }{
    \stypeenv_0 \sWTS \enoop{\sexpr_0} : \kany
  }

  \inferrule*{
    \stypeenv_0 \sWTU \sexpr_0 : \sdyn
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
    \stypeenv_0 \sWTS \serror : \sdyn
  }
\end{mathpar}
}|

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
    \stypeenv_0 \sWTT \svalue_0 : \sdyn
  }{
    \stypeenv_0 \sWTT \emon{\stype_0}{\svalue_0} : \stype_0
  }

  \inferrule*{
    \stypeenv_0 \sWTT \svalue_0 : \sshape_0
  }{
    \stypeenv_0 \sWTT \emon{\stype_0}{\svalue_0} : \stype_0
  }

  \inferrule*{
    \stypeenv_0 \sWTT \sexpr_0 : \stype_0
    \\
    \sDelta(\sunop, \stype_0) = \stype_1
  }{
    \stypeenv_0 \sWTT \eunop{\sexpr_0} : \stype_1
  }

  \inferrule*{
    \stypeenv_0 \sWTT \sexpr_0 : \stype_0
    \\
    \stypeenv_0 \sWTT \sexpr_1 : \stype_1
    \\
    \sDelta(\sbinop, \stype_0, \stype_1) = \stype_2
  }{
    \stypeenv_0 \sWTT \ebinop{\sexpr_0}{\sexpr_1} : \stype_2
  }

  \inferrule*{
    \stypeenv_0 \sWTT \sexpr_0 : \tfun{\stype_0}{\stype_1}
    \\
    \stypeenv_0 \sWTT \sexpr_1 : \stype_0
  }{
    \stypeenv_0 \sWTT \eapp{\sexpr_0}{\sexpr_1} : \stype_1
  }

  \inferrule*{
    \stypeenv_0 \sWTT \sexpr_0 : \stype_0
  }{
    \stypeenv_0 \sWTT \enoop{\sexpr_0} : \stype_0
  }

  \inferrule*{
    \stypeenv_0 \sWTU \sexpr_0 : \sdyn
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
    \stypeenv_0 \sWTT \serror : \sdyn
  }
\end{mathpar}
}|


@subsection{Label Consistency}

all T-owners distinct from S/U (the latter can mix)

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
    \sowner_0; \sownerenv_0 \sWL \eapp{\sexpr_0}{\sexpr_1}
  }

  \inferrule*{
    \sowner_1; \sownerenv_0 \sWL \sexpr_0
  }{
    \sowner_0; \sownerenv_0 \sWL \enoop{\obnd{\sowner_0}{}{\sowner_1}}{\sexpr_0}
  }

  \inferrule*{
    \sowner_1; \sownerenv_0 \sWL \sexpr_0
  }{
    \sowner_0; \sownerenv_0 \sWL \escan{\obnd{\sowner_0}{\sshape_0}{\sowner_1}}{\sexpr_0}
  }

  \inferrule*{
    \sowner_1; \sownerenv_0 \sWL \sexpr_0
  }{
    \sowner_0; \sownerenv_0 \sWL \ewrap{\obnd{\sowner_0}{\stype_0}{\sowner_1}}{\sexpr_0}
  }

  \inferrule*{
    \stowner_1; \sownerenv_0 \sWL \sexpr_0
  }{
    \stowner_0; \sownerenv_0 \sWL \ownedby{\sexpr_0}{\stowner_1}
  }

  \inferrule*{
    \ssowner_1; \sownerenv_0 \sWL \sexpr_0
  }{
    \ssowner_0; \sownerenv_0 \sWL \ownedby{\sexpr_0}{\ssowner_1}
  }

  \inferrule*{
    \suowner_0; \sownerenv_0 \sWL \sexpr_0
  }{
    \ssowner_0; \sownerenv_0 \sWL \ownedby{\sexpr_0}{\suowner_0}
  }

  \inferrule*{
    \suowner_1; \sownerenv_0 \sWL \sexpr_0
  }{
    \suowner_0; \sownerenv_0 \sWL \ownedby{\sexpr_0}{\suowner_1}
  }

  \inferrule*{
    \ssowner_0; \sownerenv_0 \sWL \sexpr_0
  }{
    \suowner_0; \sownerenv_0 \sWL \ownedby{\sexpr_0}{\ssowner_0}
  }

\end{mathpar}
}|


@subsection{Theorems}

First a notation

@${\ssurface_0 \srr \sexpr_0
 \sdefeq
 \vdash \ssurface_0 : \stspec
  \mbox{ and } \vdash \ssurface_0 : \stspec \scompile \sexpr_1
  \mbox{ and } \sexpr_1 \srr \sexpr_0}

@exact|{
\begin{theorem}[TS$(\sX,\stypemap)$]
  Language\ $\sX$
  satisfies\ $\fTS{\stypemap}$
  if for all\ $\ssurface_0$
  such that\ $\vdash \ssurface_0 : \stspec$
  holds, one of the following holds:
  \begin{itemize}
    \item $\ssurface_0 \srr \svalue_0$ and\ $\sWTX \svalue_0 : \ftypemap{\stspec}$
    \item $\ssurface_0 \srr \serror$
    \item $\ssurface_0 \srr$ diverges
  \end{itemize}
\end{theorem}
}|

@exact|{
\begin{theorem}[type soundness]\leavevmode
  \begin{itemize}
    \item $\sU$ satisfies\ $\fTS{\stypemapzero}$
    \item $\sS$ satisfies\ $\fTS{\stypemapshape}$
    \item $\sT$ satisfies\ $\fTS{\stypemapone}$
  \end{itemize}
\end{theorem}
}|

@exact|{
\begin{theorem}[complete monitoring]
  If\ $~\vdash \ssurface_0 : \stspec$
  and\ $\sowner_0 \Vdash \ssurface_0$
  and\ $\ssurface_0 \srr \sexpr_0$
  then\ $\sowner_0 \Vdash \sexpr_0$
\end{theorem}
}|


@subsection{Lemmas}

@exact|{
\begin{lemma}[$\stypemap$-compile]
  If\ $~\vdash \ssurface_0 : \stspec$
  then\ $\vdash \ssurface_0 : \stspec \scompile \sexpr_0$
  and\ $\sWTX \sexpr_0 : \ftypemap{\stspec}$
\end{lemma}
}|

@exact|{
\begin{lemma}[decomposition]
  For all\ $\sexpr_0$
  there exists unique\ $\sexpr_1, \sctx_0$
  such that\ $\sexpr_0 \sexpreq \finhole{\sctx_0}[\sexpr_1]$
\end{lemma}
}|

@exact|{
\begin{lemma}[type progress]
  If\ $~\vdash \sexpr_0 : \stspec$
  then either\ $\sexpr_0 \in \svalue \cup \serror$
  or\ $\sexpr_0 \scc \sexpr_1$
\end{lemma}
}|

@exact|{
\begin{lemma}[type preservation]
  If\ $~\vdash \sexpr_0 : \stspec$
  and\ $\sexpr_0 \scc \sexpr_1$
  then\ $\vdash \sexpr_1 : \stspec$
\end{lemma}
}|

@exact|{
\begin{lemma}[$\sdelta, \sDelta$ agreement]\leavevmode
  \begin{itemize}
    \item
      If\ $~\sDelta(\sunop, \sdyn) = \sdyn$
      and\ $\vdash \svalue_0 : \sdyn$
      and\ $\fdefined{\sdelta(\sunop, \svalue_0)}$
      then\ $\vdash \sdelta(\sunop, \svalue_0) : \sdyn$
    \item
      If\ $~\sDelta(\sunop, \sshape_0) = \sshape_1$
      and\ $\vdash \svalue_0 : \sshape_0$
      and\ $\fdefined{\sdelta(\sunop, \svalue_0)}$
      then\ $\vdash \sdelta(\sunop, \svalue_0) : \sshape_1$
    \item
      If\ $~\sDelta(\sbinop, \sdyn, \sdyn) = \sdyn$
      and\ $\vdash \svalue_0 : \sdyn$
      and\ $\vdash \svalue_1 : \sdyn$
      and\ $\fdefined{\sdelta(\sbinop, \svalue_0, \svalue_1)}$
      then\ $\vdash \sdelta(\sbinop, \svalue_0, \svalue_1) : \sdyn$
    \item
      If\ $~\sDelta(\sbinop, \sshape_0, \sshape_1) = \sshape_2$
      and\ $\vdash \svalue_0 : \sshape_0$
      and\ $\vdash \svalue_1 : \sshape_1$
      and\ $\fdefined{\sdelta(\sbinop, \svalue_0, \svalue_1)}$
      then\ $\vdash \sdelta(\sbinop, \svalue_0, \svalue_1) : \sshape_2$
  \end{itemize}
\end{lemma}
}|

@exact|{
\begin{lemma}[type substitution]\leavevmode
  \begin{itemize}
    \item
      If\ $~\vdash \efun{\svar_0}{\sexpr_0} : \tdyn$
      and\ $~\vdash \svalue_0 : \tdyn$
      then\ $~\vdash \esubst{\sexpr_0}{\svar_0}{\svalue_0} : \tdyn$
    \item
      If\ $~\vdash \efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0} : \tdyn$
      and\ $~\vdash \svalue_0 : \tdyn$
      and\ $\fshapematch{\sshape_0}{\svalue_0}$
      then\ $~\vdash \esubst{\sexpr_0}{\svar_0}{\svalue_0} : \tdyn$
    \item
      If\ $~\vdash \efun{\svar_0}{\sexpr_0} : \kfun$
      and\ $~\vdash \svalue_0 : \sshape_0$
      then\ $~\vdash \esubst{\sexpr_0}{\svar_0}{\svalue_0} : \sdyn$
    \item
      If\ $~\vdash \efun{\tann{\svar_0}{\sshape_0}}{\sexpr_0} : \kfun$
      and\ $~\vdash \svalue_0 : \sshape_1$
      and\ $\fshapematch{\sshape_0}{\svalue_0}$
      then\ $~\vdash \esubst{\sexpr_0}{\svar_0}{\svalue_0} : \kany$
    \item
      If\ $~\vdash \efun{\tann{\svar_0}{\stype_0}}{\sexpr_0} : \tfun{\stype_0}{\stype_1}$
      and\ $~\vdash \svalue_0 : \stype_0$
      then\ $~\vdash \esubst{\sexpr_0}{\svar_0}{\svalue_0} : \stype_1$
  \end{itemize}
\end{lemma}
}|

@exact|{
\begin{lemma}[S to U]
  If\ $~\vdash \sexpr_0 : \sshape_0$
  then\ $~\vdash \sexpr_0 : \sdyn$
\end{lemma}
}|

@exact|{
\begin{lemma}[type in-hole]
  If\ $~\vdash \finhole{\sctx_0}{\sexpr_0} : \stspec_0$
  then\ $\fexistsone{\stspec_1} \vdash \sexpr_0 : \stspec_1$
\end{lemma}
}|

@exact|{
\begin{lemma}[type replace]
  If\ $~\vdash \finhole{\sctx_0}{\sexpr_0} : \stspec_0$
  and\ $~\vdash \sexpr_0 : \stspec_1$
  and\ $~\vdash \sexpr_1 : \stspec_1$
  then\ $~\vdash \finhole{\sctx_0}{\sexpr_1} : \stspec_0$
\end{lemma}
}|

@exact|{
\begin{lemma}[boundary]\leavevmode
  \begin{itemize}
    \item
      If\ $~\vdash \svalue_0 : \stspec$
      and\ $\fshapematch{\sshape_0}{\svalue_0}$
      then\ $\vdash \svalue_0 : \sshape_0$
    \item
      If\ $~\vdash \svalue_0 : \sshape_0$
      then\ $\vdash \svalue_0 : \sdyn$
    \item
      If\ $~\vdash \svalue_0 : \stype_0$
      and\ $\ewrap{\stype_0}{\svalue_0} \snr \svalue_1$
      then\ $\vdash \svalue_1 : \ftypemapshape{\stype_0}$
      and\ $\vdash \svalue_1 : \tdyn$
  \end{itemize}
\end{lemma}
}|

@exact|{
\begin{lemma}[owner preservation]
  If\ $~\vdash \sexpr_0 : \stspec$
  and\ $\sowner_0 \Vdash \sexpr_0$
  and\ $\sexpr_0 \snr \sexpr_1$
  then\ $\sowner_0 \Vdash \sexpr_1$
\end{lemma}
}|

@exact|{
\begin{lemma}[label in-hole]
  If\ $\sowner_0 \Vdash \finhole{\sctx_0}{\sexpr_0}$
  then\ $\fexistsone{\sowner_1} \sowner_1 \Vdash \sexpr_0$
\end{lemma}
}|

@exact|{
\begin{lemma}[label replace]
  If\ $\sowner_0 \Vdash \finhole{\sctx_0}{\sexpr_0}$
  and\ $\sowner_1 \Vdash \sexpr_0$
  and\ $\sowner_1 \Vdash \sexpr_1$
  then\ $\sowner_0 \Vdash \finhole{\sctx_0}{\sexpr_1}$
\end{lemma}
}|



@section[#:tag "sec:both:implementation"]{Implementation}
@; X typed-context? hook = easy
@; - reuse ctc + type = hard
@; x #%module-begin, to reuse contract defs save space ... NVM
@; X require/untyped-contract
@; X define-typed/untyped-id
@; X reuse G macros in S code
@; - manual type env needs manual trust

To combine @|sdeep| and @|sshallow| in a single implementation,
 we add a new entry point via the language.
Language @tt{typed/racket} enters the default, @|sdeep| types.
New language @tt{typed/racket/shallow} gives @|sshallow| types.
Both enter the same Typed Racket implementation, but set different values
 for a compile-time flag.

The flag influences several aspects of the compiler:
 how to protect boundaries,
 the interpretation of types as contracts,
 whether to rewrite typed code with @|stransient| checks,
 and whether to disable certain optimizations.
This section focuses on challenges for the three-way interaction.

@subsection{Code Re-Use}

@; TODO move figure up, to model?
@figure*[
  "fig:both:base-interactions"
  @elem{@|sDeep|, @|sShallow|, and untyped interactions.}
  both:model-interaction]


Extending Typed Racket to safely share type environments between @|sdeep|
 and @|sshallow| code required significant changes.

First, the context.
Typed Racket supports separate compliation.
Modules are type-checked individually; once a module is checked, the compiler
 serializes type environment info so that future modules can re-use the results.
When one @|sdeep| module imports from another, it gets an identifier that
 it can look up in the deserialized type environment.

For safe @|sdeep| and untyped interactions, Typed Racket provides identifies
 through one level of indirection.
Instead of providing a function @tt{f} directly, Typed Racket provides a
 macro that expands either to @tt{f} or a contract-protected version.
This macro chooses based on a compile-time flag; a @tt{typed/racket} module
 sets the flag while an untyped module leaves it unset.

So, the story so far is that there are two versions of @|sdeep| identifiers.
One has types and can only be used in @|sdeep| code.
The other has no types, but is safe for untyped.
When @|sShallow| Racket imports a @|sdeep| module, it needs both the type
 and the contract.
Achieving both required two changes:
@itemlist[
@item{
  First, each @|sdeep| identifier must be serialized with an extra bit to
  say it is @|sdeep|-typed.
  Expansion in a @|sshallow|-typed module can then give the identifier for
   untyped code.
}
@item{
  Second, complile-time type lookups need to undo the contract step.
  Fortunately, the contract library provides the needed metadata; each
   contract-provided identifier has a complile-time link to its parent.
  Type lookup checks for these links and uses them to find the parent's type
   when checking @|stransient| code.
}
]

In the other direction, @|sshallow| code can send plain identifiers to
 @|sshallow| and untyped code.
But @|sshallow| cannot go directly to @|sdeep| --- a @|sdeep| module must install
 a contract at the boundary.
TODO how can we do this? 


@subsection{Syntax Re-Use (Unsolved)}

Although @|sshallow| code can re-use @|sdeep| type definitions
 and can access @|sdeep| identifiers through contracts,
 @|sshallow| cannot re-use @|sdeep| macros.
Re-use is desirable to avoid copying code, but macros open a soundness hole.

The RackUnit testing library shows the need to re-use macros.
@|sDeep| Racket comes with a typed wrapper over this untyped testing library.
The wrapper has 300 lines of type definitions and macro definitions.
If @|sShallow| Racket were to copy these definitions, then both copies would
 need to be maintained.

But macros are clearly unsafe in general because they can smuggle a typed
 identifier across a boundary.
Consider a simple macro that expands to a function application:

@code-nested{(define-syntax-rule (call-f x) (f x))}

@|noindent|If this macro were used in @|sshallow| code, it could receive
 an argument @tt{x} that does not match its type assumptions.
This would break soundness.
And since there is no static check to tell good macros from dangerous ones,
 the solution is to prevent @|sdeep| from exporting a macro outside @|sdeep|
 code.

@; ;; rackunit/rackunit-typed/rackunit/main.rkt
@;(define-syntax (test-case stx)
@;  (syntax-case stx ()
@;    [(_ name expr ...)
@;     (quasisyntax/loc stx
@;       (parameterize
@;           ([current-test-name
@;             (ensure-string name (quote-syntax #,(datum->syntax #f 'loc #'name)))])
@;         (test-begin expr ...)))]))
@;
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

To enable re-use for libraries such as RackUnit, the @|sdeep| wrapper can
 disable optimization and unsafely export macros that pass a manual inspection
This is a kludge, but the manual inspection is easier than forking.
And without optimization, unsoundness can only lead to a Racket-level error.
Perhaps the hand-inspection of these and other macros can suggest
 techniques for an automated check.



@subsection{Typed/Untyped Utilities}
@; --- struggles = edit old code , 2 of these => new type errors

Typed Racket exposes a limited API to manually tweak typed/untyped interactions.
Deals with challenges that arose as users built libraries for mixed-typed users.
Two forms in this API can lead to unexpected results in the new
 @|sdeep| + @|sshallow| world.


@; --- require/untyped-contract

First is @tt{require/untyped-contract}.
This form is for untyped code to import a typed identifier whose type
 does not have a @|sdeep| contract.
Users can give a supertype that has a contract, and use the typed identifier
 in a more restricted way than typed code does.

For example, the @bm{jpeg} benchmark depends on a library for multi-dimensional
 arrays (@render-lib[(make-lib "math/array" "https://docs.racket-lang.org/math/array.html")]).
This library accepts two kinds of data for array indices:
 either a vector of natural numbers or a vector of integers.
Helper functions assert that values with the integer type do not actually
 contain negative numbers:

@code-nested{(: check-array-shape (-> (U (Vectorof Natural) (Vectorof Integer)) (Vectorof Natural)))}

Such types cannot be turned into useful contracts because Racket does not have
 true union contracts.
The work around is to give a supertype:

@code-nested{(require/untyped-contract [check-array-shape (-> (Vectorof Integer) (Vectorof Natural))])}

This form comes with a surprising design choice.
If an untyped-contract identifier goes back into typed code, it has its original
 type, not the supertype.
The form only talks about contracts, it does not narrow type checking.
This is a safe thing to do because @|sdeep|-typed code does not use
 the contract; it can tag along until it hits another boundary.

For @|sshallow| types, the contract is needed.
It must be applied immediately when the value reaches @|sshallow| code,
 and this means the original type is misleading because some would-be-type-correct
 behaviors are certainly ruled out by the contract.
So the supertype must be used.
This means, new typechecking errors can occur when changing from @|sdeep|
 to @|sshallow|.


@; --- define-typed/untyped-identifier
@; TODO more extreme version of untyped-contract

Second is @tt{define-typed/untyped-identifier}.
As the name suggests, this form creates a new identifier from two old ones.
The following example defines @tt{f} from two other names:

@code-nested{(define-typed/untyped-identifier f typed-f untyped-f)}

The meaning of the new @tt{f} depends on the context it appears.
In typed code, @tt{f} expands to @tt{typed-f}.
In untyped code, an @tt{f} is a synonym for @tt{untyped-f}.

The @tt{typed-f} is intended for @|sdeep|-typed code.
It cannot be safely used in a @|sshallow| module because it may
 assume certain interactions.
Consequently, @|sshallow| code must use the untyped id.
This means, unfortunately, that changing a @|sdeep| module to @|sshallow|
 can raise a type checking error.
In particular, occurrences of @tt{f} that expand to @tt{untyped-f} are
 untyped identifiers.
There is no way to uncover the type that a @tt{typed-f} would have, and
 it is unclear whether such a type is always desirable.

The way forward is to add a third argument to the form so that users can
 specify behavior for all three contexts.
Even so, old code needs changes.


@section[#:tag "sec:both:evaluation"]{Evaluation}

@subsection[#:tag "sec:both:expressiveness"]{Expressiveness}
@; new mixed programs, relative to TR alone
@; - (Syntaxof (-> Int Int)) ... new mixed programs that TR doesn't allow


@subsection[#:tag "sec:both:performance"]{Performance}
@; - worst-case table (can trace "min" line in "fig:transient:overhead"
@; - 
@; - ?? 2-way lattice? 3-way
@; - ?? programs where mix is better than natural-only or transient-only

Offering both @|sDeep| and @|sShallow| types removes the tradeoff evident
 in @chapter-ref{chap:transient}.
Programmers can easily switch from one semantics to the other
 to pick the best performance.
(Or the best guarantees, if needed.)
For all benchmarks, the choice improves the worst-case overhead of
 gradual typing (@section-ref{sec:both:perf:worst}).
By implication, Typed Racket can offer a new migration story.
If the plan is to migrate from untyped to fully-typed one module at
 a time, adding @|sshallow| types greatly reduces the odds of hitting a slow
 configuration along a randomly-chosen path (@section-ref{sec:both:perf:path}).
Mixing @|sdeep| and @|sshallow| types in one configuration opens new possibilities
 (@section-ref{sec:both:perf:both}).
One especially promising direction is to use @|sshallow| types in
 library code (@section-ref{sec:both:perf:lib}).


@subsubsection[#:tag "sec:both:perf:worst"]{Worst-Case, Table}

@(let* ((WT (get-mixed-worst-table SHALLOW-CURRENT-BENCHMARK*))
       )
@list[
@figure*[
  "fig:both:mixed-worst-table"
  @elem{
   Worst-case overhead after choosing the best of @|sdeep| and @|sshallow|.
  }
  @render-mixed-worst-table[WT]
]
@elem{
Allowing @|sdeep| and @|sshallow| types opens up the overhead plots
 in the previous chapter.
With the ability to choose one world or the other, programmers can
 trace the best-case line on such a plot.

@Figure-ref{fig:both:mixed-worst-table} summarizes the consequences
 of the new freedom by listing the worst-case overhead in each benchmark,
 after picking the best of either @|sdeep| or @|sshallow|.
Before, high-overheads were common.
After, all these perils are avoidable by switching languages.
}])

@subsubsection[#:tag "sec:both:perf:path"]{Paths, Migration Story}

@subsubsection[#:tag "sec:both:perf:both"]{Better Together?}

Are there any mixed lattice points, using guarded and transient, that do
 better than a "pure" configuration?

For a negative answer, need a lattice on top of every lattice point.
Can try small benchmarks --- ok, then extrapolate.


@subsubsection[#:tag "sec:both:perf:lib"]{Changing Library}

For benchmarks that depend on a typed library,
 excluding gregor and quad for now,
 how are both lattices when library is transient?

Wider implication for Racket?
@; really need to check math library asap


@subsection{Limitations and Threats}
@; - threats: no blame in transient, 



@section{Failed Attempt to Optimize}
@; - talk about with-boundary model, explain HLU-interactions, why failed,
@;   how to overcome maybe
@; - 
@; - 
@; - 
@;


