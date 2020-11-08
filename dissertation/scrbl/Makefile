MAIN=bg

bg.pdf: pkg compile bg.tex
	pdflatex ${MAIN} && \
	bibtex ${MAIN} && \
	pdflatex ${MAIN} && \
	pdflatex ${MAIN} && \
	echo "done, fully" 

fast: compile bg.tex
	pdflatex ${MAIN}.tex && \
	open ${MAIN}.pdf && \
	echo "done"

ben: pkg compile bg.tex
	latexmk -silent --pdf ${MAIN}.tex ${MAIN}.bib

bg.tex: abstract.scrbl acknowledgments.scrbl bg.scrbl both.scrbl conclusion.scrbl design.scrbl future.scrbl introduction.scrbl performance.scrbl related.scrbl transient.scrbl why.scrbl scribble-load-replace.tex oopsla-2019/basic.scrbl oopsla-2019/jungle.scrbl oopsla-2019/technical.scrbl oopsla-2019/conclusion.scrbl oopsla-2019/pict.rkt
	PLTSTDERR="error info@bg-thesis" raco scribble --latex ++style texstyle.tex ++extra package.tex ++extra def.tex ++extra scribble-load-replace.tex ${MAIN}.scrbl

compile:
	raco make -v ${MAIN}.scrbl

pkg:
	raco pkg install --auto --skip-installed

clean:
	latexmk -C
