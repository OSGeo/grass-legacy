
MODULE_TOPDIR = ../..

SUBDIRS = rtree \
	dglib \
	diglib \
	Vlib

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: subdirs

clean: cleansubdirs

#check for program
checkdoxygen:
	@(type -p doxygen > /dev/null || (echo "ERROR: Install 'doxygen' software first (get from http://www.doxygen.org)" && exit 1))

htmldocs: checkdoxygen
	doxygen ./Doxyfile_arch_html
	@echo "HTML reference in directory ./html/index.html"

# NOTE: dglib, diglib and rtree are excluded in ./Doxyfile_arch_latex
# currently only Vect_*() functions in document:
latexdocs: checkdoxygen
	test ! -d latex || (cd ./latex && make clean)
	doxygen ./Doxyfile_arch_latex
	(cd ./latex && make)
	@echo "Latex reference in directory ./latex/refman.dvi"

pdfdocs: checkdoxygen
	test ! -d latex || (cd ./latex && make clean)
	doxygen ./Doxyfile_arch_latex
	(cd ./latex && make pdf && mv refman.pdf grass51_`date '+%Y_%m_%d'`_refman.pdf)
	@echo "PDF reference in directory ./latex/grass51_`date '+%Y_%m_%d'`_refman.pdf"

cleandocs:
	rm -rf ./latex ./html
