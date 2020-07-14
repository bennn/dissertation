MAIN=bg

all: pkg compile abstract.scrbl acknowledgments.scrbl bg.scrbl both.scrbl conclusion.scrbl design.scrbl future.scrbl introduction.scrbl performance.scrbl related.scrbl transient.scrbl why.scrbl scribble-load-replace.tex
	PLTSTDERR="error info@bg-thesis" raco scribble --latex ++style texstyle.tex ++extra package.tex ++extra def.tex ++extra scribble-load-replace.tex ${MAIN}.scrbl
	latexmk -silent --pdf ${MAIN}.tex ${MAIN}.bib

compile:
	raco make -v ${MAIN}.scrbl

tex: bg.tex
	xelatex ${MAIN}.tex

pkg:
	raco pkg install --auto --skip-installed

clean:
	latexmk -C
