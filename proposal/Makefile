all: pdf

pdf:	proposal.tex bib.bib
	latexmk -silent --pdf $<

guide:	guide.tex bib.bib
	latexmk -silent --pdf $<

clean:
	latexmk -C
