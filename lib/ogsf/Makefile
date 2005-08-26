MODULE_TOPDIR = ../..

EXTRA_LIBS=$(GISLIB) $(BITMAPLIB) $(VECTLIB) $(XLIBPATH) $(OPENGLLIB) $(OPENGLULIB) $(TIFFLIBPATH) $(TIFFLIB) $(IMAGELIB) $(SITESLIB) $(G3DLIB)

LIB_NAME = $(OGSF_LIBNAME)

include $(MODULE_TOPDIR)/include/Make/Lib.make

EXTRA_INC = $(VECT_INC) $(OPENGLINC) $(TIFFINCPATH)
EXTRA_CFLAGS = $(VECT_CFLAGS) $(XCFLAGS)

default: lib inc

inc:
	$(INSTALL) -m 644 gsurf.h $(ARCH_INCDIR)/gsurf.h
	$(INSTALL) -m 644 gsurf.h $(ARCH_DISTDIR)/include//gsurf.h
	$(INSTALL) -m 644 kftypes.h $(ARCH_INCDIR)/kftypes.h
	$(INSTALL) -m 644 kftypes.h $(ARCH_DISTDIR)/include/kftypes.h
	$(INSTALL) -m 644 keyframe.h $(ARCH_INCDIR)/keyframe.h
	$(INSTALL) -m 644 keyframe.h $(ARCH_DISTDIR)/include/keyframe.h
	$(INSTALL) -m 644 local_proto.h $(ARCH_INCDIR)/ogsf_proto.h
	$(INSTALL) -m 644 local_proto.h $(ARCH_DISTDIR)/include/ogsf_proto.h
	cat gstypes.h | sed -e 's/local_proto.h/ogsf_proto.h/' > $(ARCH_INCDIR)/gstypes.h
	cat gstypes.h | sed -e 's/local_proto.h/ogsf_proto.h/' > $(ARCH_DISTDIR)/include/gstypes.h

DOXNAME=ogsf
#check for program
checkdoxygen:
	@(type doxygen > /dev/null || (echo "ERROR: Install 'doxygen' software first (get from http://www.doxygen.org)" && exit 1))

htmldocs: checkdoxygen
# hack needed to get main page at beginning:
	@mv $(DOXNAME)lib.dox $(DOXNAME)lib.dox.org
	@cat $(DOXNAME)lib.dox.org | sed 's+/\*! \\page +/\*! \\mainpage +g' > $(DOXNAME)lib.dox
	doxygen ./Doxyfile_arch_html
	@echo "HTML reference in directory ./html/index.html"

latexdocs: checkdoxygen
	test ! -d latex || (cd ./latex && $(MAKE) clean)
# hack needed to get main page at beginning:
	@mv $(DOXNAME)lib.dox $(DOXNAME)lib.dox.org
	@cat $(DOXNAME)lib.dox.org | sed 's+/\*! \\page +/\*! \\mainpage +g' > $(DOXNAME)lib.dox
	doxygen ./Doxyfile_arch_latex
#this hack is needed to make Acroread's search engine happy:
	(cd ./latex ; echo "\usepackage[T1]{fontenc}" >> doxygen.sty)
	(cd ./latex && $(MAKE) )
	@echo "Latex reference in directory ./latex/refman.dvi"

pdfdocs: checkdoxygen
	test ! -d latex || (cd ./latex && $(MAKE) clean)
# hack needed to get main page at beginning:
	@mv $(DOXNAME)lib.dox $(DOXNAME)lib.dox.org
	@cat $(DOXNAME)lib.dox.org | sed 's+/\*! \\page +/\*! \\mainpage +g' > $(DOXNAME)lib.dox
	doxygen ./Doxyfile_arch_latex
	(cd ./latex ; echo "\usepackage[T1]{fontenc}" >> doxygen.sty)
	(cd ./latex && $(MAKE) refman.pdf && mv refman.pdf grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR}${DOXNAME}_`date '+%Y_%m_%d'`_refman.pdf)
	@echo "PDF reference in directory ./latex/grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR}${DOXNAME}_`date '+%Y_%m_%d'`_refman.pdf"

cleandocs:
	rm -rf ./latex ./html
