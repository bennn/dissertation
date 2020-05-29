#lang classicthesis @parts @pdfspacing

@title{Temporary and Permanent Types}
@author{Ben Greenman}
@degree{Doctor of Philosophy}
@department{Khoury College of Computer Sciences}
@university{Northeastern University}
@location{Boston, Massachusetts}
@submit-date{December 2020}


@approval{src/approval.pdf}
@; TODO allow - in name

@include-abstract{abstract.scrbl}

@; need newline above
@include-acknowledgements{acknowledgments.scrbl}

@table-of-contents[]
@end-front-matter[]

@include-section{introduction.scrbl}
@include-section{performance.scrbl}
@include-section{design.scrbl}
@include-section{transient.scrbl}
@include-section{both.scrbl}
@include-section{related.scrbl}
@include-section{future.scrbl}
@include-section{conclusion.scrbl}



@;\input{package}
@;\begin{document}
@;\input{def}
@;\maketitle
@;
@;\begin{abstract}
@;\input{abstract}
@;\end{abstract}
@;
@;
@;\newcommand{\mksec}[3]{\section{#3} \label{#1} \input{#2}}
@;
@;\mksec{sec:introduction}{introduction}{Introduction}
@;
@;
@;\footnotesize
@;\bibliographystyle{ACM-Reference-Format}
@;\bibliography{bib}
@;
@;\end{document}
