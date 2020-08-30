# This version of the Makefile assumes that rst2pdf and its dependencies
# are installed.

RST2PDF=rst2pdf --break-level=1 \
            #--fit-literal-mode=overflow \
            -e preprocess
            #--font-path /Library/Fonts/Microsoft \
            #--font-path /System/Library/Fonts \

.PHONY: default
default: html pdf

# We don't try to provide an HTML version of the slides in this version
# (not least because the `.. page::` directive won't be recognised)
.PHONY: html
html:
	rst2html.py README.rst README.html
	rst2html.py old-syntax-notes.rst old-syntax-notes.html

.PHONY: pdf
pdf: slides
	rst2pdf -e preprocess --fit-literal-mode=overflow old-syntax-notes.rst -o old-syntax-notes.pdf

.PHONY: slides
slides: make4x3 make16x9

.PHONY: make16x9
make16x9:
	$(RST2PDF) -s light16x9.style old-syntax-slides.rst -o old-syntax-slides-16x9.pdf

.PHONY: 16x9
16x9:   make16x9
	open old-syntax-slides-16x9.pdf

.PHONY: make4x3
make4x3:
	$(RST2PDF) -s light4x3.style old-syntax-slides.rst -o old-syntax-slides-4x3.pdf

.PHONY: 4x3
4x3:   make4x3
	open old-syntax-slides-4x3.pdf

.PHONY: clean
clean:
	rm -f *.html

.PHONY: distclean
distclean: clean
	rm -f *.pdf

.PHONY: help
help:
	@echo 'make           same as: make html pdf'
	@echo 'make pdf       create old-syntax-slides-[4x3|16x9].pdf and other PDF files'
	@echo 'make 4x3       make and open old-syntax-slides-4x3.pdf'
	@echo 'make 16x9      make and open old-syntax-slides-16x9.pdf'
	@echo 'make html      create HTML files using rst2html'
	@echo 'make slides    just create old-syntax-slides-[4x3|16x9].pdf'
	@echo 'make clean     delete HTML files'
	@echo 'make distclean delete PDF and HTML files
