
MODULE_TOPDIR = ../..

#the order is relevant
SUBDIRS = \
	dbmi_base \
	dbmi_client \
	stubs \
	dbmi_driver \
	sqlp

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: subdirs

clean: cleansubdirs

#check for program
checkdoxygen:
	@(type doxygen > /dev/null || (echo "ERROR: Install 'doxygen' software first (get from http://www.doxygen.org)" && exit 1))

generatelocaldox:
	(cd dbmi_client ; sh generate_dox.sh)

htmldocs: checkdoxygen generatelocaldox
	doxygen ./Doxyfile_arch_html
	@echo "HTML reference in directory ./html/index.html"

# NOTE: stubs/ and sqlp/ are excluded in ./Doxyfile_arch_latex
# currently only Vect_*() functions in document:
latexdocs: checkdoxygen generatelocaldox
	test ! -d latex || (cd ./latex && $(MAKE) clean)
	doxygen ./Doxyfile_arch_latex
#this hack is needed to make Acroread's search engine happy:
	(cd ./latex ; echo "\usepackage[T1]{fontenc}" >> doxygen.sty)
	(cd ./latex && $(MAKE) )
	@echo "Latex reference in directory ./latex/refman.dvi"

pdfdocs: checkdoxygen generatelocaldox
	test ! -d latex || (cd ./latex && $(MAKE) clean)
	doxygen ./Doxyfile_arch_latex
#this hack is needed to make Acroread's search engine happy:
	(cd ./latex ; echo "\usepackage[T1]{fontenc}" >> doxygen.sty)
	(cd ./latex && $(MAKE) pdf && mv refman.pdf grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR}dbmi_`date '+%Y_%m_%d'`_refman.pdf)
	@echo "PDF reference in directory ./latex/grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR}dbmi_`date '+%Y_%m_%d'`_refman.pdf"

cleandocs:
	rm -rf ./latex ./html dbmi_client/*.dox
