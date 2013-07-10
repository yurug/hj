.PHONY: all clean 

all: slides.tex
	TEXINPUTS=common:.:${TEXINPUTS} pdflatex slides.tex
	TEXINPUTS=common:.:${TEXINPUTS} pdflatex slides.tex
	TEXINPUTS=common:.:${TEXINPUTS} pdflatex slides.tex

slides.tex: main.tex
	latexpp -g color yes main.tex > slides.tex


clean:
	rm --force *.log *.aux *.vrb *.toc *~ *.nav *.out *.snm *.pdf

