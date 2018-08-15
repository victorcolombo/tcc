all: main.pdf

main.pdf: main.tex capitulos/*.tex definitions.tex main.bib
	pdflatex main.tex
	bibtex main.aux
	pdflatex main.tex
	pdflatex main.tex