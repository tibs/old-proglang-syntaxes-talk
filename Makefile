# This version of the Makefile assumes that rst2pdf and its dependencies
# are installed.

RST2PDF=rst2pdf -s light.style \
            --break-level=1 \
            --fit-literal-mode=overflow \
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

# We only produce one aspect ratio with rst2pdf - we'd need to write a new
# style for a different aspect ratio
# (I'm calling the current aspect ratio "4x3", which it sort of looks like.
# What we don't have is 16x9)
.PHONY: slides
slides: 43


.PHONY: 43
43:
	$(RST2PDF) old-syntax-slides.rst -o old-syntax-slides-4x3.pdf
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
	@echo 'make html      create HTML files using rst2html'
	@echo 'make slides    just create old-syntax-slides-[4x3|16x9].pdf'
	@echo 'make clean     delete HTML files'
	@echo 'make distclean delete PDF and HTML files
