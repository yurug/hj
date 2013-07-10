.PHONY: all clean examples

all: slides.tex
	TEXINPUTS=common:${TEXINPUTS} pdflatex slides.tex
	TEXINPUTS=common:${TEXINPUTS} pdflatex slides.tex
	TEXINPUTS=common:${TEXINPUTS} pdflatex slides.tex

slides.tex: main.tex
	latexpp -g color yes main.tex > slides.tex


clean:
	rm --force *.log *.aux *.vrb *.toc *~ *.nav *.out *.snm *.pdf

examples:
	ocamlbuild pp.byte
	./pp.byte < exists.ml > exists.tex
	./pp.byte < exists-nude.ml > exists-nude.tex
	./pp.byte < exists-spec.ml > exists-spec.tex
	./pp.byte < exists-bis.ml > exists-bis.tex
	./pp.byte < sorted.ml > sorted.tex
	./pp.byte < fold.ml > fold.tex
