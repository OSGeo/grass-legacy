MODULE_TOPDIR = ../..

EXTRA_LIBS=$(GISLIB) $(FFTWLIB) $(LAPACKLIB) $(BLASLIB)

EXTRA_CFLAGS = $(FFTWINC) $(PICFLAGS)

LIB_NAME = $(GMATH_LIBNAME)

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: lib

DOXNAME=gmath
#check for program
checkdoxygen:
	@(type doxygen > /dev/null || (echo "ERROR: Install 'doxygen' software first (get from http://www.doxygen.org)" && exit 1))

htmldocs: checkdoxygen
# hack needed to get main page at beginning:
	@mv $(DOXNAME)lib.dox $(DOXNAME)lib.dox.org
	@cat $(DOXNAME)lib.dox.org | sed 's+/\*! \\page +/\*! \\mainpage +g' > $(DOXNAME)lib.dox
	doxygen ../g3d//Doxyfile_arch_html
	@mv $(DOXNAME)lib.dox.org $(DOXNAME)lib.dox
	@echo "HTML reference in directory ./html/index.html"

# NOTE: stubs/ and sqlp/ are excluded in ./Doxyfile_arch_latex
latexdocs: checkdoxygen
	test ! -d latex || (cd ./latex && $(MAKE) clean)
# hack needed to get main page at beginning:
	@mv $(DOXNAME)lib.dox $(DOXNAME)lib.dox.org
	@cat $(DOXNAME)lib.dox.org | sed 's+/\*! \\page +/\*! \\mainpage +g' > $(DOXNAME)lib.dox
	doxygen ../g3d//Doxyfile_arch_latex
#this hack is needed to make Acroread's search engine happy:
	(cd ./latex ; echo "\usepackage[T1]{fontenc}" >> doxygen.sty)
	(cd ./latex && $(MAKE) )
	@mv $(DOXNAME)lib.dox.org $(DOXNAME)lib.dox
	@echo "Latex reference in directory ./latex/refman.dvi"

pdfdocs: checkdoxygen
	test ! -d latex || (cd ./latex && $(MAKE) clean)
# hack needed to get main page at beginning:
	@mv $(DOXNAME)lib.dox $(DOXNAME)lib.dox.org
	@cat $(DOXNAME)lib.dox.org | sed 's+/\*! \\page +/\*! \\mainpage +g' > $(DOXNAME)lib.dox
	doxygen ../g3d//Doxyfile_arch_latex
#this hack is needed to make Acroread's search engine happy:
	(cd ./latex ; echo "\usepackage[T1]{fontenc}" >> doxygen.sty)
	(cd ./latex && $(MAKE) refman.pdf && mv refman.pdf grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR}$(DOXNAME)_`date '+%Y_%m_%d'`_refman.pdf)
	@mv $(DOXNAME)lib.dox.org $(DOXNAME)lib.dox
	@echo "PDF reference in directory ./latex/grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR}$(DOXNAME)_`date '+%Y_%m_%d'`_refman.pdf"

cleandocs:
	rm -rf ./latex ./html
