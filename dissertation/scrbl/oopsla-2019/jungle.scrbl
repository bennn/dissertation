#lang greenman-thesis/include
@(require greenman-thesis/oopsla-2019/main)

@title[#:tag "sec:design:jungle"]{Assorted Behaviors by Example}

Although every gradual typing system shares a common motivation,
 one and the same program may behave quite differently in the context
 of two different systems.
These behavioral differences stem from alternative views about how to enforce
 types at the boundaries between typed and untyped code.
To understand the existing behaviors and to develop new approaches, one
 must therefore understand the design space of checking strategies.

Other differences prevent a systematic comparison across languages.
Concrete gradual typing systems insist that every value comes with
 a declaration of intent@~citep{wzlov-popl-2010, mt-oopsla-2017}, 
which severely limits the kind of untyped code programmers may write.
Migratory typing systems augment an existing untyped language with a tailor-made
 static type checker@~citep{tfffgksst-snapl-2017}.
True gradually-typed languages, in the sense of  @citet{svcb-snapl-2015},
 include a type Dynamic (@${\mathbf{Dyn}},
@${\star}, or @${\mathbf{?}}) that satisfies certain
 typing and behavioral properties and permits the insertion of type
 declarations at arbitrary places.
Lastly, of course, different languages come with different syntax, values, and types.

One way to appreciate the essential and incidental variations is
 to translate mixed-typed interactions into different languages.
The first three subsections explore four examples:
  Flow@~citep{cvgrl-oopsla-2017},
  Reticulated@~citep{vss-popl-2017},
  Typed Racket@~citep{tfffgksst-snapl-2017},
  and Nom@~citep{mt-oopsla-2017}.
Flow is a migratory typing system for JavaScript,
 Reticulated equips Python with gradual types,
 Typed Racket extends Racket,
 and Nom is a new gradual-from-the-start object-oriented language.
@Sectionref{sec:design:landscape} offers a broad perspective on the
 mixed-typed design space.


@section{Enforcing a Base Type}

@figure*[
  "fig:example-atom"
  @elem{@exact|{\Programref{eq:example-atom}}| translated to four languages}

@exact|{
\begin{subfigure}[b]{0.48\columnwidth}
\begin{mdframed}[style=staframestyle,userdefinedwidth=61mm,align=center]\begin{alltt}
function f(x : number): number
\{ return x + 1; \}\end{alltt}\end{mdframed}
\begin{minipage}{17mm}~\end{minipage}\begin{minipage}{4mm}
  \vspace{-7mm}\begin{tikzpicture}
    \node (A) {};
    \node (B) [inner sep=0pt,below of=A,yshift=8mm] {};
    \node (C) [below of=B,yshift=2mm,xshift=6mm] {};
%%    \draw [-] (A) -- (B.north);
    \draw [->] (A) to[bend right=32,looseness=1.4] (C);
  \end{tikzpicture}
\end{minipage}\begin{minipage}{19mm}
\begin{mdframed}[style=dynframestyle,userdefinedwidth=12.5mm,align=center]\begin{alltt}
f(f);\end{alltt}\end{mdframed}
\end{minipage}\hspace{-5mm}\begin{minipage}{4mm}
  \vspace{-7mm}\begin{tikzpicture}
    \node (A) {};
    \node (B) [inner sep=0pt,below of=A,yshift=8mm] {};
    \node (C) [below of=B,yshift=2mm,xshift=-7mm] {};
    \node (D) [above of=B,yshift=-11mm,xshift=4mm] {\hugecheckmark};
    \draw [dashed] (B) to[bend left=32,looseness=1.4] (C);
    \draw [->,dashed] (B) -- (A);
  \end{tikzpicture}
\end{minipage}

\caption{Flow}
  \label{fig:flow-atom}
\end{subfigure}%
\qquad%
\begin{subfigure}[b]{0.44\columnwidth}
\begin{mdframed}[style=staframestyle,userdefinedwidth=41mm,align=center]\begin{alltt}
def f(x : Int)->Int:
  return x + 1\end{alltt}\end{mdframed}

\begin{minipage}{14mm}~\end{minipage}\begin{minipage}{4mm}
  \vspace{-7mm}\begin{tikzpicture}
    \node (A) {};
    \node (B) [inner sep=0pt,below of=A,yshift=8mm] {};
    \node (C) [below of=B,yshift=2mm,xshift=6mm] {};
%%    \draw [-] (A) -- (B.north);
    \draw [->] (A) to[bend right=32,looseness=1.4] (C);
  \end{tikzpicture}
\end{minipage}\begin{minipage}{20mm}
\begin{mdframed}[style=dynframestyle,userdefinedwidth=12mm,align=center]\begin{alltt}
f(f)\end{alltt}\end{mdframed}
\end{minipage}\hspace{-5mm}\begin{minipage}{4mm}
  \vspace{-7mm}\begin{tikzpicture}
    \node (A) {};
    \node (B) [inner sep=0pt,below of=A,yshift=8mm] {};
    \node (C) [below of=B,yshift=2mm,xshift=-7mm] {};
    \node (D) [above of=B,yshift=-11mm] {\kern-3pt\hugexmark};
    \draw [<-,dashed] (D) to[bend left=32,looseness=1.4] (C);
  \end{tikzpicture}
\end{minipage}

\caption{Reticulated}
  \label{fig:retic-atom}
\end{subfigure}

\vspace{1ex}
\begin{subfigure}[b]{0.45\columnwidth}
\begin{mdframed}[style=staframestyle,userdefinedwidth=53mm,align=center]\begin{alltt}
(: f (-> Integer Integer))
(define (f x)
  (+ x 1))\end{alltt}\end{mdframed}

\begin{minipage}{14mm}~\end{minipage}\begin{minipage}{4mm}
  \vspace{-7mm}\begin{tikzpicture}
    \node (A) {};
    \node (B) [inner sep=0pt,below of=A,yshift=8mm] {};
    \node (C) [below of=B,yshift=2mm,xshift=6mm] {};
    \draw [->] (A) to[bend right=32,looseness=1.4] (C);
  \end{tikzpicture}
\end{minipage}\begin{minipage}{21mm}
\begin{mdframed}[style=dynframestyle,userdefinedwidth=14mm,align=center]\begin{alltt}
(f f)\end{alltt}\end{mdframed}
\end{minipage}\hspace{-5mm}\begin{minipage}{4mm}
  \vspace{-7mm}\begin{tikzpicture}
    \node (A) {};
    \node (B) [inner sep=0pt,below of=A,yshift=8mm] {};
    \node (C) [below of=B,yshift=2mm,xshift=-7mm] {};
    \node (D) [above of=B,yshift=-11mm] {\kern-3pt\hugexmark};
    \draw [<-,dashed] (D) to[bend left=32,looseness=1.4] (C);
  \end{tikzpicture}
\end{minipage}

\caption{Typed Racket}
  \label{fig:tr-atom}
\end{subfigure}%
\qquad%
\begin{subfigure}[b]{0.46\columnwidth}
\begin{mdframed}[style=staframestyle,userdefinedwidth=54mm,align=center]\begin{alltt}
class F \{
  constructor () \{\}
  fun apply(Int x) : Int \{
    return x + 1;
  \}
\}\end{alltt}\end{mdframed}
\begin{minipage}{4.5mm}~\end{minipage}\begin{minipage}{4mm}
  \vspace{-8mm}\begin{tikzpicture}
    \node (A) {};
    \node (B) [inner sep=0pt,below of=A,yshift=4mm] {};
    \node (C) [below of=B,yshift=2mm,xshift=6mm] {};
    \draw [-] (A) -- (B.north);
    \draw [->] (B.north) to[bend right=32,looseness=1.4] (C);
  \end{tikzpicture}
\end{minipage}\begin{minipage}{41mm}
\begin{mdframed}[style=dynframestyle,userdefinedwidth=34mm,align=center]\begin{alltt}
dyn f = new F();
f.apply((dyn)f);\end{alltt}\end{mdframed}
\end{minipage}\hspace{-5mm}\begin{minipage}{4mm}
  \vspace{-9.5mm}\begin{tikzpicture}
    \node (A) {};
    \node (B) [inner sep=0pt,below of=A,yshift=2mm] {};
    \node (C) [below of=B,yshift=2mm,xshift=-6mm] {};
    \node (D) [above of=B,yshift=-7.2mm] {\raisebox{1mm}{\hugexmark}};
    \draw [<-,dashed] (B.north) to[bend left=32,looseness=1.4] (C);
  \end{tikzpicture}
\end{minipage}

\caption{Nom}
  \label{fig:nom-atom}
\end{subfigure}
}|]

One of the simplest ways that a mixed-typed interaction can go wrong
 is for untyped code to send incorrect input to a typed context that
 expects a flat value.
The first example illustrates one such interaction: 
@exact|{
\begin{equation}
  \label{eq:example-atom}
  \begin{minipage}{33mm}
    \begin{mdframed}[style=staframestyle,userdefinedwidth=33mm]\(
      f = \efun{\tann{\svar}{\tint}}{\svar + 1}
    \)\end{mdframed}
  \end{minipage}\begin{minipage}{23mm}\begin{tikzpicture}
    \node (A) {};
    \node (B) [right of=A,xshift=3em] {};
    \node (C) [below of=A,yshift=5ex] {};
    \node (D) [below of=B,yshift=5ex] {};
    \draw[->] (A) edge [bend left] (B);
    \draw[<-,dashed] (C) edge [bend right] (D);
  \end{tikzpicture}\end{minipage}\begin{minipage}{9mm}
    \begin{mdframed}[style=dynframestyle,userdefinedwidth=9mm]\(
      f~f
    \)\end{mdframed}
  \end{minipage}
\end{equation}
}|
The typed function on the left expects an integer.
The untyped context on the right imports this function @${f} and applies @${f} to
 itself; thus the typed function receives a function rather than an integer.
The question is whether the program halts
 or invokes the typed function @${f} on a nonsensical input.


@Figure-ref{fig:example-atom} translates the program to four languages.
Despite the differences in syntax and types, each clearly defines a
 typed function that expects an integer on the top
 and applies the function to itself in an untyped context on the bottom.

In Flow (@exact|{\figureref{fig:flow-atom}}|), the program does not detect a type mismatch.
The typed function receives a function and surprisingly computes a string
 (@tt{ECMA-262} edition 10, @exact{\S} @hyperlink["https://www.ecma-international.org/ecma-262/#sec-addition-operator-plus"]{12.8.3}).
In the other three languages, the program halts with 
a @emph{boundary error} message that alerts the programmer to the
 mismatch between two chunks of code.

Flow does not detect the run-time type mismatch because it follows the
 @emph{erasure}, or optional typing, approach to type enforcement.
@|ename| is hands-off;
 types have no effect on the behavior of a program.
In Flow, types enable static type checking for typo discovery
 and other IDE tools, but disappear during compilation.
Consequently, the author of a typed Flow function cannot assume that it receives only
 well-typed input.

The other languages enforce static types with some kind of dynamic check.
For base types, the check validates the shape of incoming data.
The checks for other types reveal differences among these non-trivial type enforcement strategies.


@section[#:tag "sec:design:anti-concrete"]{Validating an Untyped Data Structure}

The second example is about pair types. Specifically, it questions what
 happens when typed code declares a pair type and receives an untyped pair
 at runtime:
@exact|{
\begin{equation}
  \label{eq:example-pair}
  \begin{minipage}{48mm}
    \begin{mdframed}[style=staframestyle,userdefinedwidth=48mm]\(
      g = \efun{\tann{\svar}{\tpair{\tint}{\tint}}}{}(\sfst~\svar) + 1
    \)\end{mdframed}
  \end{minipage}\begin{minipage}{23mm}\begin{tikzpicture}
    \node (A) {};
    \node (B) [right of=A,xshift=3em] {};
    \node (C) [below of=A,yshift=5ex] {};
    \node (D) [below of=B,yshift=5ex] {};
    \draw[->] (A) edge [bend left] (B);
    \draw[<-,dashed] (C) edge [bend right] (D);
  \end{tikzpicture}\end{minipage}\begin{minipage}{20mm}
    \begin{mdframed}[style=dynframestyle,userdefinedwidth=20mm]\(g~\epair{``A''}{2}\)\end{mdframed}
  \end{minipage}
\end{equation}
}|
The typed function on the left expects a pair of integers and uses the
 first element of the input pair as a number.
The untyped code on the right applies this function to a pair that contains a
 string and an integer.

@Figure-ref{fig:example-pair} translates this idea into
 Reticulated, Typed Racket, and Nom.
The encodings in Reticulated and Typed Racket
 define a pair in untyped code and impose a type in typed code.
The encoding in Nom is different;
 @exact|{\figureref{fig:nom-pair}}| presents a Nom program
 in which the typed code expects an instance of one data structure but
 the untyped code provides something else.
This shape mismatch leads to a run-time error.

Nom cannot express @exact|{\programref{eq:example-pair}}| directly
 because the language does not allow partially-typed values.
There is no common pair constructor that: (1) untyped code can use without
 constraints and (2) typed code can receive at a particular type.
All type structure must be specified with the data structure.
On one hand, this requirement greatly simplifies run-time validation
 because the outermost shape of any value determines the shape of its elements.
On the other hand, it imposes a significant burden on the programmer.
To add refined static type checking at the use-sites of an untyped data structure,
 a programmer must either add a cast to each use in typed code
 or edit the untyped code for a new data definition.
Because Nom and other concrete languages require this kind of type structure in
 untyped code (@format-url{https://dart.dev})@~citep{wzlov-popl-2010,rzv-ecoop-2015,mt-oopsla-2017}.
 the model in @sectionref{sec:design:technical} does not support them.

Both Reticulated and Typed Racket raise an error on \programref{eq:example-pair},
 but for substantially different reasons.
Typed Racket rejects the untyped pair at the boundary to the typed context
 because the pair does not fully match the declared type.
Reticulated accepts the value at the boundary because it is a pair,
 but later raises an exception at the elimination form \codett{y[0]} because typed code
 expects an integer result but receives a string.
These sample behaviors are indicative of a wider difference;
 Typed Racket eagerly checks the contents of data structures
 while Reticulated lazily validates use-sites.

@figure*[
  "fig:example-pair"
  @elem{@exact{\Programref{eq:example-pair}} translations}
  @exact|{
\begin{minipage}{0.51\columnwidth}
\begin{subfigure}[b]{\columnwidth}
\begin{mdframed}[style=dynframestyle,userdefinedwidth=27mm,align=center]\begin{alltt}
x = ["A", 2]\end{alltt}\end{mdframed}
  \vspace{-10mm}\begin{center}\begin{tikzpicture}
    \node (A) {};
    \node (B) [below of=A,yshift=3mm] {};
    \draw[->] (A) -- (B);
    \node (C) [below of=A,yshift=6mm,xshift=3mm] {\hugexmark};
    \node (D) [below of=A,yshift=6mm,xshift=-3mm] {~~\hphantom{\hugexmark}};
\end{tikzpicture}\end{center}\vspace{-5mm}
\begin{mdframed}[style=staframestyle,userdefinedwidth=52mm,align=center]\begin{alltt}
def g(y : Tuple(Int,Int)):
  return y[0] + 1

g(x)\end{alltt}\end{mdframed}
\vspace{-4mm}

\caption{Reticulated}
  \label{fig:retic-pair}
\end{subfigure}

\vspace{1ex}
\begin{subfigure}[b]{\columnwidth}
\vspace{4mm}
\begin{mdframed}[style=dynframestyle,userdefinedwidth=47mm,align=center]\begin{alltt}
(define x (list "A" 2))\end{alltt}\end{mdframed}
\vspace{-10mm}\begin{center}\begin{tikzpicture}
    \node (A) {};
    \node (B) [below of=A,yshift=3mm] {};
    \node (E) [below of=A,yshift=5mm] {};
    \draw[->] (A) -- (E);
    \node (C) [below of=A,yshift=6mm,xshift=3mm] {\hugexmark};
    \node (D) [below of=A,yshift=6mm,xshift=-3mm] {~~\hphantom{\hugexmark}};
\end{tikzpicture}\end{center}\vspace{-5mm}
\begin{mdframed}[style=staframestyle,userdefinedwidth=58mm,align=center]\begin{alltt}
(require/typed
  [x (List Integer Integer)])

(+ (first x) 1)\end{alltt}\end{mdframed}
\vspace{-4mm}

\caption{Typed Racket}
  \label{fig:tr-pair}
\end{subfigure}
\end{minipage}\hfill\begin{minipage}{0.38\columnwidth}
\begin{subfigure}[t]{\columnwidth}
\begin{mdframed}[style=dynframestyle,userdefinedwidth=42mm,align=center]\begin{alltt}
class Pair \{
  private fst;
  private snd;
  # ....
\}

x = new Pair("A", 2)\end{alltt}\end{mdframed}
\vspace{-10mm}\begin{center}\begin{tikzpicture}
    \node (A) {};
    \node (B) [below of=A,yshift=3mm] {};
    \node (E) [below of=A,yshift=5mm] {};
    \draw[->] (A) -- (E);
    \node (C) [below of=A,yshift=6mm,xshift=3mm] {\hugexmark};
    \node (D) [below of=A,yshift=6mm,xshift=-3mm] {~~\hphantom{\hugexmark}};
\end{tikzpicture}\end{center}\vspace{-5mm}
\begin{mdframed}[style=staframestyle,userdefinedwidth=42mm,align=center]\begin{alltt}
class IntPair \{
  private Int fst;
  private Int snd;
  # ....
\}

((IntPair)x).fst + 1\end{alltt}\end{mdframed}
\vspace{-4mm}

\caption{Nom}
  \label{fig:nom-pair}
\end{subfigure}
\end{minipage}

}|]


@section[#:tag "sec:design:lying-type"]{Uncovering the Source of a Mismatch}

@figure*[
  "fig:tr-example"
  @elem{Using Typed Racket to define an API}
  @exact|{
  \begin{minipage}{0.56\columnwidth}
 \texttt{net/url}\\[-1ex]
\begin{mdframed}[style=dynframestyle,userdefinedwidth=64mm,align=center]\begin{alltt}
#lang racket
;; +600 lines of code ....

(define (call/input-url url c h)
  ;; connect to the url via c,
  ;; process the data via h
  ....)
\end{alltt}\end{mdframed}
 {\texttt{client}\hfill\raisebox{-3mm}[0pt][0pt]{\begin{tikzpicture}
  \node (A) {};
  \node (B) [below right of=A,yshift=2mm] {};
  \node (C) [below of=A] {};
  \draw [->] (A.south east) to[bend right=25] (B.north east);
  \draw [->] (B.south east) to[bend right=25] (C.north east);
  \draw [->,dashed] (C.north west) -- (A.south west);
\end{tikzpicture}}\hspace{01mm}~}\\[-1ex]
\begin{mdframed}[style=dynframestyle,userdefinedwidth=64mm,align=center]\begin{alltt}
#lang racket
(require html typed/net/url)

(define URL
  (string->url "https://sr.ht"))

;; connect to url, read html
(define (main)
  (call/input-url URL
                  (\(\lambda\) (str) ....)
                  read-html))
\end{alltt}\end{mdframed}
  \end{minipage}\hfill\begin{minipage}{0.44\columnwidth}
  \texttt{typed/net/url}\\[-1ex]
\begin{mdframed}[style=staframestyle,userdefinedwidth=55mm,align=left]\begin{alltt}
#lang typed/racket

(define-type URL ....)

(require/typed/provide
  ;; from this library
  net/url

  ;; import the following
  [string->url
   (-> String URL)]

  [call/input-url
   (\(\forall\) (A)
    (-> URL
        (-> String In-Port)
        (-> In-Port A)
        A))])
\end{alltt}\end{mdframed}
  \end{minipage}

}|]

@figure*[
  "fig:retic-example"
  @elem{Using Reticulated to define an API}
  @exact|{
  \begin{minipage}[t]{0.48\columnwidth}
  \texttt{requests}\\[-1ex]
\begin{mdframed}[style=dynframestyle,userdefinedwidth=54mm,align=left]\begin{alltt}
# 2,000 lines of code ....

def get(url, *args, **kws):
  # Sends a GET request
  ....
\end{alltt}\end{mdframed}

 {\texttt{client}\hfill\raisebox{-3.5mm}[0pt][0pt]{\begin{tikzpicture}
  \node (A) {};
  \node (B) [below right of=A,yshift=4mm] {};
  \node (C) [below of=A] {};
  \draw [->] (A.south east) to[bend right=30] (B.north east);
  \draw [->] (B.south east) to[bend right=20] (C.north east);
  \draw [->,dashed] (C.north west) -- (A.south west);
\end{tikzpicture}}\hspace{01mm}~}\\[-1ex]
\begin{mdframed}[style=dynframestyle,userdefinedwidth=64mm,align=right]\begin{alltt}
from typed_requests import get

wait_times = (2, "zero")
get("https://sr.ht", wait_times)
\end{alltt}\end{mdframed}

\end{minipage}\begin{minipage}[t]{0.52\columnwidth}
  \vspace{2ex}
  \texttt{typed\_requests}\\[-1ex]
\begin{mdframed}[style=staframestyle,userdefinedwidth=62mm,align=left]\begin{alltt}
import requests as r

def get(url:Str,
        to:Tuple(Float,Float)):
  return r.get(url, to)
\end{alltt}\end{mdframed}

\end{minipage}
}|]

@Figure-ref["fig:tr-example" "fig:retic-example"] present excerpts
 from realistic programs that mix typed and untyped code.
These examples follow the same general structure:
 an untyped client interacts with an untyped library through a thin layer of typed code.
Both programs also signal run-time errors, but for different reasons and
 with different implications for the programmer.

@Figure-ref{fig:tr-example} consists of an untyped library,
 an @emph{incorrect} layer of type annotations,
 and an untyped client of the types.
The module on the top left, @codett{net/url},
 is a snippet from an untyped library that has been part of Racket for two decades
 (@format-url{https://github.com/racket/net}).
The typed module on the right defines types for part of the untyped library.
Lastly, the module at the bottom left imports the typed library and calls
 the library function @codett{call/input-url}.

Operationally, the library function flows from @codett{net/url}
 to the typed module and then to the client.
When the client calls this function, it sends
 client data to the untyped library code via the typed module.
The client application clearly relies on the type specification from @codett{typed/net/url} because:
 the first argument is a URL structure,
 the second is a function that accepts a string,
 and the third is a function that maps an input port to an HTML representation.
Unfortunately for the client, the type declaration in @figure-ref{fig:tr-example}
 is buggy.
The library applies the first callback of
 @codett{call/input-url} to a URL struct, not a string.

Fortunately for the developer, Typed Racket compiles types to contracts and
 thereby catches the mismatch.
Here, the compilation of @codett{typed/net/url} generates a higher-order
 function contract for @codett{call/input-url}.
The generated contract ensures that the untyped client provides three
 type-matching argument values and that the library applies the callback to a
 string.
When the @codett{net/url} library eventually applies the callback
 function to a URL structure, the function contract for the callback halts
 the program.
The blame message says that @codett{net/url} broke the
 contract on the back-channel from it to @codett{client}, but warns the
 developer on the last line with ``assuming the contract is correct.''
A quick look confirms that the contract---that is, the type
 from which the contract is derived---is wrong.
@; Typed Racket is unusual in this regard;
@; other mixed-typed languages assume that ``well-typed programs can't be
@; blamed''@~cite{wf-esop-2009}.


@Figure-ref{fig:retic-example} presents an arrangement of three Transient
 Reticulated modules, similar to the code in @figure-ref{fig:tr-example}.
The module on the top left exports a function that retrieves data from a URL
 (@format-url{https://github.com/psf/requests}).
This function accepts several optional and keyword arguments.
The typed adaptor module on the right formulates types for one valid use of the
 function; a client may supply a URL as a string and a timeout as a pair of floats.
@; pair = (connect timeout, read timeout)
These types are correct, but the client module on the bottom left sends
 a tuple that contains an integer and a string.

Reticulated's runtime checks ensure that the typed function receives a
 string and a tuple, but do not validate the tuple's contents.
These same arguments then pass to the untyped @codett{get} function in the
 \codett{requests} module.
When the untyped @codett{get} eventually uses the string
 @codett{"zero"} as a float, Python raises an exception that
 originates from the @codett{requests} module---possibly leading the programmer to
 search for a bug in the library.

In this example, the programmer is lucky because the call to the typed
 version of @codett{get} is still visible on the stack trace,
 providing a hint that this call might be at fault.
If Python were to properly implement tail calls, or if the library accessed
 the pair some time after returning control to the client, this hint would disappear.

In sum, types in Transient Reticulated do not monitor all channels of
 communication between modules.
A value may cross a type boundary without a full check, making it difficult
 to discover type-value mismatches or pinpoint their source.
Reticulated mitigates this problem with a global map from heap
 addresses to type obligations.
The analysis in @section-ref{sec:design:technical} demonstrates, however,
 that this map can provide misleading and incomplete blame assignments.


@section[#:tag "sec:design:landscape"]{Landscape of Mixed-Typed Languages}

@; TODO better in thesis intro????
@; TODO other middling systems?
@; TODO flip, put erasure on the bottom?
@figure*[
  "fig:landscape"
  @elem{Landscape of mixed-typed languages,
    @exact{\(\displaystyle\mtlang\)} = migratory,
    @exact{\(\displaystyle\dynlang\)} = gradual}
  @exact|{
  \begin{tikzpicture}
    \def\embeddingskip{2cm}
    \renewcommand{\cite}[1]{}
    \node (E)
      [align=left]
      {\textstrat{\ename{}}};
    \node (EBOX)
      [left=of E.south west,anchor=north west,xshift=2.5em,draw=black!70!white,rectangle,rounded corners=5pt,align=center]
      {ActionScript\cite{rch-popl-2012}\({}^{\mtlangann}\) ~~~
       Common Lisp\({}^{\mtlangann}\) ~~~
       mypy\({}^{\mtlangann}_{\dynlangann}\) ~~~
       Flow\cite{cvgrl-oopsla-2017}\({}^{\mtlangann}_{\dynlangann}\) ~~~
       Hack\({}^{\mtlangann}_{\dynlangann}\) ~~~
       Pyre\({}^{\mtlangann}_{\dynlangann}\) ~~~
       Pytype\({}^{\mtlangann}_{\dynlangann}\) \\[0.4ex]
       rtc\cite{rtsf-sac-2013}\({}^{\mtlangann}_{\dynlangann}\) \quad
       Strongtalk\cite{bg-oopsla-1993}\({}^{\mtlangann}\) \quad
       TypeScript\cite{bat-ecoop-2014}\({}^{\mtlangann}_{\dynlangann}\) \quad
       Typed Clojure\cite{bdt-esop-2016}\({}^{\mtlangann}\) \quad
       Typed Lua\cite{mmi-dls-2015}\({}^{\mtlangann}\)};

    \node (NBOX)
      [below=of EBOX.south west,anchor=north west,xshift=0.5em,draw=black!70!white,rectangle,rounded corners=5pt,align=center]
      {Gradualtalk\cite{acftd-scp-2013}\({}^{\mtlangann}_{\dynlangann}\) ~~
       Grift\({}_{\dynlangann}\) \\[0.4ex]
       Pycket\cite{bbst-oopsla-2017}\({}^{\mtlangann}\) \quad
       TPD\cite{wmwz-ecoop-2017}\({}^{\mtlangann}\) \\[0.4ex]
       Typed Racket\cite{tf-popl-2008}\({}^{\mtlangann}\)};

    \node (N)
      [right=of NBOX.north west,anchor=south west,xshift=-2.5em]
      {\textstrat{\nname}};

    \node (TBOX)
      [right=of NBOX.north east,xshift=-1em,anchor=north west,yshift=3mm,draw=black!70!white,rectangle,rounded corners=5pt,align=center]
      {Grace\cite{rmhn-ecoop-2019} ~~
       Pallene\cite{gi-sblp-2018}\({}^{\mtlangann}\) \\[0.4ex]
       Reticulated\cite{vss-popl-2017}\({}^{\mtlangann}_{\dynlangann}\)};

    \node (T)
      [right=of TBOX.north west,anchor=south west,xshift=-2.5em]
      {\textstrat{\tname}};

    \node (CBOX)
      [right=of TBOX.north east,xshift=-0.5em,yshift=-2ex,anchor=north west,draw=black!70!white,rectangle,rounded corners=5pt,align=center]
      {\csharp{}  \quad
       Dart 2 \\[0.4ex]
       Nom\cite{mt-oopsla-2017}\({}_{\dynlangann}\) ~
       SafeTS\cite{rsfbv-popl-2015} \\[0.4ex]
       {TS\({}^*\)}\cite{sfrbcsb-popl-2014}};

    \node (C)
      [right=of CBOX.north west,anchor=south west,xshift=-2.5em]
      {\textstrat{Concrete}};

    \node (EC)
      [draw=black!80!white,dashed,ellipse,left=of EBOX.south east,xshift=0.9em,yshift=-1mm,anchor=north,align=center]
      {\(\!\!\!\)StrongScript\cite{rzv-ecoop-2015}\(\!\!\!\)\\[0.4ex]
       Thorn\cite{wzlov-popl-2010}};

    \node (ET)
      [draw=black!80!white,dashed,ellipse,left=of TBOX.south west,xshift=7mm,yshift=-2mm,x radius=10em,anchor=north west,align=center]
      {~Pyret~};

  \end{tikzpicture}

}|]

The four languages discussed above give a sense of the variety among
 mixed-typed systems.
@Figure-ref{fig:landscape} presents a more complete picture.
There are a number of languages that pursue the leading approaches.
@|ename| is by far the most popular strategy; perhaps because of its
 uncomplicated semantics, predictable performance, and ease of implementation.
The @|nname| and @|tname| languages all come from academic teams.
By contrast, the Concrete and @|ename| regions contain industry-led efforts
 such as @exact{\csharp{}} and Pytype.

Other languages explore a hybrid approach.
StrongScript and Thorn offer a choice of concrete and erased types@~citep{wzlov-popl-2010,rzv-ecoop-2015}.
Pyret uses @|nname|-style checks to validate fixed-size data and @|tname|-style checks
 for recursive types (e.g. lists) and higher-order
 types --- according to personal communication with Benjamin Lerner and Shriram Krishnamurthi.

The literature contains further variations as formal semantics.
@citet{cl-icfp-2017} present a semantics that drops certain higher-order wrappers.
@citet{svctg-esop-2015} present a monotonic semantics for references.

@Figure-ref{fig:landscape} also marks languages as gradual (@exact{\(\dynlang{}\)})
 or migratory (@exact{\(\mtlang{}\)}).
A gradual language includes a dynamic type that satisfies the
 gradual guarantees@~citep{svcb-snapl-2015}.
A migratory typing system adds types to an existing dynamically typed
 language@~citep{tfffgksst-snapl-2017}.
The gradual/migratory dimension is orthogonal to run-time type enforcement, but
 helps demonstrate the variety in the landscape.


