talk = essence-of-ad

texdeps = formatting.fmt Makefile

.PRECIOUS: %.tex %.pdf %.web

all: google.pdf
all: icfp.pdf
all: full.pdf

# see: $(TARG).see

dots = $(wildcard Figures/*.dot)
pdfs = $(addsuffix .pdf, $(basename $(dots))) $(wildcard Figures/circuits/*-scaled.pdf)

full.tex: $(talk).lhs $(texdeps)
	lhs2TeX --set=extended --set=full -o $*.tex $(talk).lhs

icfp.tex: $(talk).lhs $(texdeps)
	lhs2TeX --set=extended --set=icfp -o $*.tex $(talk).lhs

google.tex: $(talk).lhs $(texdeps)
	lhs2TeX --set=extended --set=google -o $*.tex $(talk).lhs

%.pdf: %.tex macros.tex $(pdfs) Makefile
	pdflatex $*.tex

%.tex: %.lhs macros.tex formatting.fmt Makefile
	lhs2TeX -o $*.tex $*.lhs

showpdf = open -a Skim.app

%.see: %.pdf
	${showpdf} $*.pdf

# Cap the size so that LaTeX doesn't choke.
%.pdf: %.dot # Makefile
	dot -Tpdf -Gmargin=0 -Gsize=10,10 $< -o $@

pdfs: $(pdfs)

clean:
	rm -f {full,icfp,google}.{tex,pdf,aux,nav,snm,ptb,log,out,toc}

STASH=conal@conal.net:/home/conal/web/talks

# # make: Circular macros.pdf <- macros.tex dependency dropped.
# %.web: %.pdf
# 	scp $? $(STASH)/essence-of-automatic-differentiation-$?
# 	touch $@

google.web: google.pdf
	scp $? $(STASH)/essence-of-automatic-differentiation-$?
	touch $@

full.web: full.pdf
	scp $? $(STASH)/essence-of-automatic-differentiation-$?
	touch $@

web: google.web full.web
