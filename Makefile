LAT = latex -shell-escape
#TEX = pdflatex --shell-escape
TEXINPUTS=.:./sty:
TEX = TEXINPUTS=.:./sty: latexmk -pdf -xelatex
BIB = bibtex8
TEXFILES = $(wildcard *.tex)
PDFFILES = $(TEXFILES:.tex=.pdf)
BIBFILES = $(wildcard *.bib)
DOT = dot -Tpdf

a1.pdf: p1-plot1.pdf p1-plot2.pdf a1.tex
	$(TEX) a1.tex

p1-plot1.pdf p1-plot2.pdf: src/q700/a1.clj
	lein run -m q700.a1/run

tags:
	ctags -R src test checkouts

clean:
	rm -f p1-plot1.pdf p1-plot2.pdf

.PHONY: tags clean
