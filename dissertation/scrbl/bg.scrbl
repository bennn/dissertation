#lang greenman-thesis @parts @pdfspacing

@;  How about this for a title: 
@;  
@;  “A Systematic Analysis of the Gradual Typing Design Space” 
@;  
@;  Then the introduction follows this pattern: 
@;  
@;  — design analysis needs performance 
@;  — design analysis needs foundational properties 
@;  — design analysis needs confirmation with the construction of new points in the space 
@;  
@;  Since the entire Transitional exercise may not turn out all that well, the revised title shifts the emphasis. 
@;  
@;  If you want to go even further, we could try a new thesis. 

@title{Temporary and Permanent Types}
@author{Ben Greenman}
@degree{Doctor of Philosophy}
@department{Khoury College of Computer Sciences}
@university{Northeastern University}
@location{Boston, Massachusetts}
@submit-date{December 2020}


@approval{src/approval.pdf}
@; TODO allow - in name

@exact|{\bibliographystyle{plainnat}}|

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
@include-section{related.scrbl}
@include-section{future.scrbl}
@include-section{conclusion.scrbl}


@exact|{\bibliography{bg}}|
