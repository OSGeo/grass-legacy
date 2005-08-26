
MODULE_TOPDIR = ../..

SUBDIRS = rtree \
	dglib \
	diglib \
	Vlib \
	transform

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: subdirs

clean: cleansubdirs

DOXNAME=vlib
#check for program
checkdoxygen:
	@(type doxygen > /dev/null || (echo "ERROR: Install 'doxygen' software first (get from http://www.doxygen.org)" && exit 1))

generatelocaldox:
	(cd Vlib ; sh generate_dox.sh)

htmldocs: checkdoxygen generatelocaldox
# hack needed to get main page at beginning:
	@mv $(DOXNAME)lib.dox $(DOXNAME)lib.dox.org
	@cat $(DOXNAME)lib.dox.org | sed 's+/\*! \\page +/\*! \\mainpage +g' > $(DOXNAME)lib.dox
	doxygen ./Doxyfile_arch_html
	@echo "HTML reference in directory ./html/index.html"

# NOTE: dglib, diglib and rtree are excluded in ./Doxyfile_arch_latex
# currently only Vect_*() functions in document:
latexdocs: checkdoxygen generatelocaldox
	test ! -d latex || (cd ./latex && $(MAKE) clean)
# hack needed to get main page at beginning:
	@mv $(DOXNAME)lib.dox $(DOXNAME)lib.dox.org
	@cat $(DOXNAME)lib.dox.org | sed 's+/\*! \\page +/\*! \\mainpage +g' > $(DOXNAME)lib.dox
	doxygen ./Doxyfile_arch_latex
#this hack is needed to make Acroread's search engine happy:
	(cd ./latex ; echo "\usepackage[T1]{fontenc}" >> doxygen.sty)
	(cd ./latex && $(MAKE) )
	@echo "Latex reference in directory ./latex/refman.dvi"

pdfdocs: checkdoxygen generatelocaldox
	test ! -d latex || (cd ./latex && $(MAKE) clean)
# hack needed to get main page at beginning:
	@mv $(DOXNAME)lib.dox $(DOXNAME)lib.dox.org
	@cat $(DOXNAME)lib.dox.org | sed 's+/\*! \\page +/\*! \\mainpage +g' > $(DOXNAME)lib.dox
	doxygen ./Doxyfile_arch_latex
	(cd ./latex ; echo "\usepackage[T1]{fontenc}" >> doxygen.sty)
	(cd ./latex && $(MAKE) refman.pdf && mv refman.pdf grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR}${DOXNAME}_`date '+%Y_%m_%d'`_refman.pdf)
	@echo "PDF reference in directory ./latex/grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR}${DOXNAME}_`date '+%Y_%m_%d'`_refman.pdf"

cleandocs:
	rm -rf ./latex ./html Vlib/*.dox
