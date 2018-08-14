GARBAGE = *.aux *.blg *.log *.toc *.bbl *.out
ALL = GARBAGE *.pdf

all: main.pdf clean
	open main.pdf

clean:
	$(foreach var, $(GARBAGE), find . -type f -name '$(var)' -delete ;)

clear:
	$(foreach var, $(ALL), find . -type f -name '$(var)' -delete ;)

main.pdf: main.tex capitulos/*.tex definitions.tex main.bib
	pdflatex main.tex
	bibtex main.aux
	pdflatex main.tex
	pdflatex main.tex