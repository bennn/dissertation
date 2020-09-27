#lang greenman-thesis/include
@(require greenman-thesis/oopsla-2019/main greenman-thesis/oopsla-2019/pict)

@title[#:tag (list "sec:design:landscape" "sec:design:jungle")]{Assorted Behaviors by Example}
@latex-label{sec:design:jungle}

@figure*[
  "fig:landscape"
  @elem{Landscape of mixed-typed languages}
  @exact|{
  \begin{tikzpicture}
    \def\embeddingskip{2cm}
    \renewcommand{\cite}[1]{}
    \node (E)
      [align=left]
      {\textstrat{\ename{}}};
    \node (EBOX)
      [left=of E.south west,anchor=north west,xshift=2.5em,draw=black!70!white,rectangle,rounded corners=5pt,align=center]
      {ActionScript\cite{rch-popl-2012}\({}^{\mtlangann}\) \quad
       Common Lisp\({}^{\mtlangann}\) \quad
       mypy\({}^{\mtlangann}_{\dynlangann}\) \quad
       Flow\cite{cvgrl-oopsla-2017}\({}^{\mtlangann}_{\dynlangann}\) \quad
       Hack\({}^{\mtlangann}_{\dynlangann}\) \quad
       \\[0.4ex]
       Pyre\({}^{\mtlangann}_{\dynlangann}\) \quad
       Pytype\({}^{\mtlangann}_{\dynlangann}\)
       RDL\cite{rtsf-sac-2013}\({}^{\mtlangann}_{\dynlangann}\) \quad
       Strongtalk\cite{bg-oopsla-1993}\({}^{\mtlangann}\) \quad
       TypeScript\cite{bat-ecoop-2014}\({}^{\mtlangann}_{\dynlangann}\)
       \\[0.4ex]
       Typed Clojure\cite{bdt-esop-2016}\({}^{\mtlangann}\) \quad
       Typed Lua\cite{mmi-dls-2015}\({}^{\mtlangann}\)};

    \node (NBOX)
      [below=of EBOX.south west,anchor=north west,xshift=0.5em,yshift=-1ex,draw=black!70!white,rectangle,rounded corners=5pt,align=center]
      {Gradualtalk\cite{acftd-scp-2013}\({}^{\mtlangann}_{\dynlangann}\) ~~
       Grift\({}_{\dynlangann}\) \\[0.4ex]
       Pycket\cite{bbst-oopsla-2017}\({}^{\mtlangann}\) \quad
       TPD\cite{wmwz-ecoop-2017}\({}^{\mtlangann}\) \\[0.4ex]
       Typed Racket\cite{tf-popl-2008}\({}^{\mtlangann}\)};

    \node (N)
      [right=of NBOX.north west,anchor=south west,xshift=-2.5em]
      {\textstrat{\nname}};

    \node (TBOX)
      [right=of NBOX.north east,xshift=-1em,anchor=north west,yshift=2ex,draw=black!70!white,rectangle,rounded corners=5pt,align=center]
      {Grace\cite{rmhn-ecoop-2019} ~~
       Pallene\cite{gi-sblp-2018}\({}^{\mtlangann}\) \\[0.4ex]
       Reticulated\cite{vss-popl-2017}\({}^{\mtlangann}_{\dynlangann}\)};

    \node (T)
      [right=of TBOX.north west,anchor=south west,xshift=-2.5em]
      {\textstrat{\tname}};

    \node (CBOX)
      [right=of TBOX.north east,xshift=-0.5em,yshift=-8.5ex,anchor=north west,draw=black!70!white,rectangle,rounded corners=5pt,align=center]
      {\csharp{}  \quad
       Dart 2 \\[0.4ex]
       Nom\cite{mt-oopsla-2017}\({}_{\dynlangann}\) ~
       SafeTS\cite{rsfbv-popl-2015} \\[0.4ex]
       {TS\({}^*\)}\cite{sfrbcsb-popl-2014}};

    \node (C)
      [right=of CBOX.north west,anchor=south west,xshift=-2.5em]
      {\textstrat{Concrete}};

    \node (EC)
      [draw=black!80!white,dashed,ellipse,left=of EBOX.south east,xshift=0.9em,yshift=-1ex,anchor=north,align=center]
      {\(\!\!\!\)StrongScript\cite{rzv-ecoop-2015}\(\!\!\!\)\\[0.4ex]
       Thorn\cite{wzlov-popl-2010}};

    \node (ET)
      [draw=black!80!white,dashed,ellipse,left=of TBOX.south west,xshift=4.5ex,yshift=-2ex,x radius=10em,anchor=north west,align=center]
      {~Pyret~};

  \end{tikzpicture}

  \hbox{$\displaystyle\mtlang$ = migratory, $\displaystyle\dynlang$ = gradual}
  }|]

There are many mixed-typed languages.
@Figureref{fig:landscape} arranges a few of their names into a rough picture
 of the design space.
Each language enables some kind of mix between typed and untyped code.
Languages marked with a star (@${\dynlang{}}) come with a special dynamic
 type, often styled as @${\star}, or @${\mathbf{?}}, that allows partially-defined types@~citep{svcb-snapl-2015}.
Technically, the type system supports implicit down-casts from the dynamic
 type to any other type---unlike, say, @exact{\texttt{Object}} in Java.
Languages marked with a cross (@${\mtlang{}}) add a tailor-made type system to
 an untyped language, but may require types for an entire module at a time@~citep{tfffgksst-snapl-2017}.
Other languages satisfy different goals.

For the most part, these mixed-typed languages fit into the broad forms
 introduced in @sectionref{sec:design:introduction}.
@|ename| is by far the most popular strategy; perhaps because of its
 uncomplicated semantics and ease of implementation@~citep{s-lisp-1990,rtsf-sac-2013,mmi-dls-2015,bdt-esop-2016}.
The @|nname| languages come from academic teams that are interested in
 types that offer strong guarantees@~citep{tf-popl-2008,acftd-scp-2013,wmwz-ecoop-2017,bbst-oopsla-2017}.
@|tname| is gaining traction as a compromise between types and performance@~citep{vss-popl-2017,rmhn-ecoop-2019,gi-scp-2020},
 and Concrete has generated interest among industry teams@~citep{bmt-ecoop-2010,dart-types}
 as well as academics@~citep{sfrbcsb-popl-2014,rsfbv-popl-2015,mt-oopsla-2017}.
Nevertheless, several languages explore a hybrid approach.
StrongScript and Thorn offer a choice of concrete and erased types@~citep{wzlov-popl-2010,rzv-ecoop-2015}.
Pyret uses @|nname|-style checks to validate fixed-size data and @|tname|-style checks
 for recursive types (e.g. lists) and higher-order types
 (personal communication with Benjamin Lerner and Shriram Krishnamurthi).
The literature presents additional languages, as formal semantics, that defy a broad categorization.
@citet{cl-icfp-2017} drop certain higher-order wrappers.
@citet{svctg-esop-2015} give a monotonic semantics for references.

Our goal is a systematic comparison of type guarantees across the wide design space.
Such a comparison is possible because, despite the variety, the different guarantees
 arise from choices about how to enforce types at the boundaries between
 type-checked code and arbitrary dynamically-typed code.
To illustrate, the following three subsections discuss type boundary examples in the
 context of:
  Flow@~citep{cvgrl-oopsla-2017},
  Reticulated@~citep{vss-popl-2017},
  Typed Racket@~citep{tfffgksst-snapl-2017},
  and Nom@~citep{mt-oopsla-2017}.
Flow is a migratory typing system for JavaScript,
 Reticulated equips Python with gradual types,
 Typed Racket extends Racket,
 and Nom is a new gradual-from-the-start object-oriented language.


@section{Enforcing a Base Type}

One of the simplest ways that a mixed-typed interaction can go awry
 is for untyped code to send incorrect input to a typed context that
 expects a flat value.
The first example illustrates one such interaction:

@equation[
  "eq:example-atom"
  jungle:example-atom]

@|noindent|The typed function on the left expects an integer.
The untyped context on the right imports this function @${f} and applies @${f} to
 itself; thus the typed function receives a function rather than an integer.
The question is whether the program halts
 or invokes the typed function @${f} on a nonsensical input.

@figure*[
  "fig:example-atom"
  @elem{@exact|{\Programref{eq:example-atom}}| translated to four languages}
  jungle:example-atom*]

@Figureref{fig:example-atom} translates the program to four languages.
Each white box represents type-checked code and each grey box represents
 untyped and, ideally, un-analyzed code that is linked in at run-time.
Nom is an exception, however, because it cannot interact with truly untyped code (@sectionref{sec:design:anti-concrete}).
Despite the differences in syntax and types, each clearly defines a
 typed function that expects an integer on the top
 and applies the function to itself in an untyped context on the bottom.

In Flow, the program does not detect a type mismatch.
The typed function receives a function from untyped JavaScript and surprisingly computes a string
 (@tt{ECMA-262} edition 10, @exact{\S} @hyperlink["https://www.ecma-international.org/ecma-262/#sec-addition-operator-plus"]{12.8.3}).
In the other three languages, the program halts with 
a @emph{boundary error} message that alerts the programmer to the
 mismatch between two chunks of code.

Flow does not detect the run-time type mismatch because it follows the
 @emph{erasure}, or optional typing, approach to type enforcement.
@|ename| is hands-off;
 types have no effect on the behavior of a program.
These static-only types help find logical mistakes and enable IDE tools,
 but disappear during compilation.
Consequently, the author of a typed Erasure function cannot assume that it
 receives only well-typed input.

The other languages enforce static types with some kind of dynamic check.
For base types, the check validates the shape of incoming data.
The checks for other types reveal differences among these non-trivial type enforcement strategies.


@section[#:tag "sec:design:anti-concrete"]{Validating an Untyped Data Structure}

The second example is about pair types. Specifically, it questions what
 happens when typed code declares a pair type and receives an untyped pair
 at runtime:

@equation[
  "eq:example-pair"
  jungle:example-pair]

@|noindent|The typed function on the left expects a pair of integers and uses the
 first element of the input pair as a number.
The untyped code on the right applies this function to a pair that contains a
 string and an integer.

@Figureref{fig:example-pair} translates this idea into
 Reticulated, Typed Racket, and Nom.
The encodings in Reticulated and Typed Racket
 define a pair in untyped code and impose a type in typed code.
The encoding in Nom is different;
 the typed code expects an instance of one data structure but
 the untyped code provides something else.
This shape mismatch leads to a run-time error.

Nom cannot express @programref{eq:example-pair} directly
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
 untyped code@~citep{wzlov-popl-2010,rzv-ecoop-2015,mt-oopsla-2017,dart-types},
 the model in @sectionref{sec:design:technical} does not support them.

Both Reticulated and Typed Racket raise an error on @programref{eq:example-pair},
 but for substantially different reasons.
Typed Racket rejects the untyped pair at the boundary to the typed context
 because the pair does not fully match the declared type.
Reticulated accepts the value at the boundary because it is a pair,
 but raises an exception at the elimination form @codett{y[0]} because typed code
 expects an integer result but receives a string.
These sample behaviors are indicative of a wider difference;
 Typed Racket eagerly checks the contents of data structures
 while Reticulated lazily validates use-sites.

@figure*[
  "fig:example-pair"
  @elem{@exact|{\Programref{eq:example-pair}}| translations}
  jungle:example-pair*]


@section[#:tag "sec:design:lying-type"]{Uncovering the Source of a Mismatch}

@figure*[
  "fig:tr-example"
  @elem{Using Typed Racket to define an API}
  jungle:tr-api]

@figure*[
  "fig:retic-example"
  @elem{Using Reticulated to define an API}
  jungle:rp-api]

@Figure-ref["fig:tr-example" "fig:retic-example"] present excerpts
 from realistic programs that mix typed and untyped code.
These examples follow the same general structure:
 an untyped client interacts with an untyped library via a thin layer of typed code.
Both programs also signal run-time errors, but for different reasons and
 with different implications for the programmer.

@Figureref{fig:tr-example} consists of an untyped library,
 an @emph{incorrect} layer of type annotations,
 and an untyped client of the types.
The module on the top left, @codett{net/url},
 is a snippet from an untyped library that has been part of Racket for two decades
 (@shorturl["https://" "github.com/racket/net"]).
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
Unfortunately for the client, the type declaration in @figureref{fig:tr-example}
 is buggy.
The library applies the first callback of
 @codett{call/input-url} to a URL struct,
 rather than a string as the developer expects.

Fortunately for the developer, Typed Racket compiles types to contracts and
 thereby catches the mismatch.
Here, the compilation of @codett{typed/net/url} generates a
 contract for @codett{call/input-url}.
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


@Figure-ref{fig:retic-example} presents an arrangement of three Transient
 Reticulated modules, similar to the code in @figureref{fig:tr-example}.
The module on the top left exports a function that retrieves data from a URL
(adapted from the @tt{requests} library, @shorturl["https://" "github.com/psf/requests"]).
This function accepts several optional and keyword arguments.
The typed adaptor module on the right formulates types for one valid use of the
 function; a client may supply a URL as a string and a timeout as a pair of floats.
These types are correct, but the client module on the bottom left sends
 a tuple that contains an integer and a string.

Reticulated's runtime checks ensure that the typed function receives a
 string and a tuple, but do not validate the tuple's contents.
These same arguments then pass to the untyped @codett{get} function in the
 @codett{requests} module.
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
 addresses to source locations.
The analysis in @sectionref{sec:design:technical} demonstrates, however,
 that this map can result in incorrect blame.


