
MODULE_TOPDIR = ../..

SUBDIRS = \
	dbmi \
	sqlp \
	stubs

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: subdirs

clean: cleansubdirs

#check for program
checkdoxygen:
	@(type -p doxygen > /dev/null || (echo "ERROR: Install 'doxygen' software first (get from http://www.doxygen.org)" && exit 1))

generatelocaldox:
	(cd dbmi ; sh generate_dox.sh)

htmldocs: checkdoxygen generatelocaldox
	doxygen ./Doxyfile_arch_html
	@echo "HTML reference in directory ./html/index.html"

# NOTE: stubs/ and sqlp/ are excluded in ./Doxyfile_arch_latex
# currently only Vect_*() functions in document:
latexdocs: checkdoxygen generatelocaldox
	test ! -d latex || (cd ./latex && make clean)
	doxygen ./Doxyfile_arch_latex
	(cd ./latex && make)
	@echo "Latex reference in directory ./latex/refman.dvi"

pdfdocs: checkdoxygen generatelocaldox
	test ! -d latex || (cd ./latex && make clean)
	doxygen ./Doxyfile_arch_latex
	(cd ./latex && make pdf && mv refman.pdf grass51dbmi_`date '+%Y_%m_%d'`_refman.pdf)
	@echo "PDF reference in directory ./latex/grass51dbmi_`date '+%Y_%m_%d'`_refman.pdf"

cleandocs:
	rm -rf ./latex ./html dbmi/*.dox
