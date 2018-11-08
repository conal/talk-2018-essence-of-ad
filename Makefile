talk = essence-of-ad

full = $(talk)
icfp = $(full)-icfp
google = $(full)-google

# TARG = $(full)
# TARG = $(icfp)
TARG = $(google)

texdeps = formatting.fmt Makefile

.PRECIOUS: %.tex %.pdf %.web

# all: $(TARG).pdf
all: $(google).pdf $(full).pdf $(icfp).pdf

see: $(TARG).see

dots = $(wildcard Figures/*.dot)
pdfs = $(addsuffix .pdf, $(basename $(dots))) $(wildcard Figures/circuits/*-scaled.pdf)

$(icfp).tex: $(talk).lhs $(texdeps)
	lhs2TeX --set=extended --set=icfp -o $*.tex $(talk).lhs

$(google).tex: $(talk).lhs $(texdeps)
	lhs2TeX --set=extended --set=google -o $*.tex $(talk).lhs

%.pdf: %.tex $(pdfs) Makefile
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
	rm -f {$(full),$(icfp),$(google)}.{tex,pdf,aux,nav,snm,ptb,log,out,toc}

web: web-token

STASH=conal@conal.net:/home/conal/web/talks

# web: web-token

# web-token: $(TARG).pdf
# 	scp $? $(STASH)/essence-of-automatic-differentiation-icfp.pdf
# 	touch $@

# %.web-token: %.pdf
# 	scp $? $(STASH)/essence-of-automatic-differentiation-$@
# 	touch $@
