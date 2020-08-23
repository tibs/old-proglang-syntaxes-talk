# This version of the Makefile assumes that pandoc and (enough of) TeX are
# available.

.PHONY: default
default: html pdf

# We don't try to provide an HTML version of the slides in this version
# - use the PDF produces by 'slides' instead.
.PHONY: html
html:
	rst2html.py README.rst README.html
	rst2html.py old-syntax-notes.rst old-syntax-notes.html

.PHONY: pdf
pdf: slides
	pandoc old-syntax-notes.rst -o old-syntax-notes.pdf -V papersize:a4

# The available aspect ratio of slides (for beamer only) are 1610 for 16:10,
# 169 for 16:9, 149 for 14:9, 141 for 1.41:1, 54 for 5:4, 43 for 4:3 which is
# the default, and 32 # for 3:2. It's probably enough to go for the following
# pair of resolutions.
.PHONY: slides
slides:
	pandoc old-syntax-slides.rst -t beamer -o old-syntax-slides-4x3.pdf -V aspectratio:43
	pandoc old-syntax-slides.rst -t beamer -o old-syntax-slides-16x9.pdf -V aspectratio:169

.PHONY: 43
43:
	pandoc old-syntax-slides.rst -t beamer -o old-syntax-slides-4x3.pdf -V aspectratio:43
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
