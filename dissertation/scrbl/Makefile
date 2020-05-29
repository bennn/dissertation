MAIN=bg

all: abstract.scrbl acknowledgments.scrbl bg.scrbl both.scrbl conclusion.scrbl design.scrbl future.scrbl introduction.scrbl performance.scrbl related.scrbl transient.scrbl
	raco scribble --latex ${MAIN}.scrbl
	xelatex ${MAIN}.tex

