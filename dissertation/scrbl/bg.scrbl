#lang greenman-thesis @parts @pdfspacing
@; TODO
@; - dedication?
@; - reorder contents, acks, etc?

@title{@|sDeep| and @|sShallow| Types}
@author{Ben Greenman}
@degree{Doctor of Philosophy}
@department{Khoury College of Computer Sciences}
@university{Northeastern University}
@location{Boston, Mass.}
@submit-date{November 2020}


@approval{src/approval.pdf}
@; TODO allow - in name

@include-abstract{abstract.scrbl}

@; need newline above
@include-acknowledgements{acknowledgments.scrbl}

@table-of-contents[]
@end-front-matter[]

@include-section{introduction.scrbl}
@include-section{why.scrbl}
@include-section{performance.scrbl}
@include-section{design.scrbl}
@include-section{transient.scrbl}
@include-section{both.scrbl}
@;@include-section{related.scrbl}
@include-section{future.scrbl}
@include-section{conclusion.scrbl}


