.PHONY: clean clean-all

clean:
	rm -f *.aux *.log *~ .*~ pdflatex.stderr pdflatex.stdout

clean-all: clean
	rm -f *.pdf *.tex
	rm -fr target
