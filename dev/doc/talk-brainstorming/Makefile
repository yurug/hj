.PHONY: all clean 

all: main.tex
	TEXINPUTS=common:.:${TEXINPUTS} pdflatex main.tex
	TEXINPUTS=common:.:${TEXINPUTS} pdflatex main.tex
	TEXINPUTS=common:.:${TEXINPUTS} pdflatex main.tex

clean:
	rm --force *.log *.aux *.vrb *.toc *~ *.nav *.out *.snm *.pdf

