MODULE_TOPDIR = ../..

LIB_NAME = $(GIS_LIBNAME)
EXTRA_LIBS = $(XDRLIB) $(DATETIMELIB) $(INTLLIB)

include $(MODULE_TOPDIR)/include/Make/Lib.make

#compile if LFS Large File Support present:
ifneq ($(USE_LARGEFILES),)
	EXTRA_CFLAGS = -D_FILE_OFFSET_BITS=64
endif

default: lib \
	$(FMODE_OBJ) \
	$(ETC)/ellipse.table \
	$(ETC)/datum.table \
	$(ETC)/datumtransform.table \
	$(ETC)/projections \
	$(ETC)/FIPS.code \
	$(ETC)/state27 \
	$(ETC)/state83 \
	$(ETC)/gui.tcl

$(FMODE_OBJ):
	@test -d $(OBJDIR) || mkdir $(OBJDIR)
	echo -e "#include <stdlib.h>\n#include <fcntl.h>\n#undef _fmode\nint _fmode = _O_BINARY;" > $(OBJDIR)/fmode.c
	$(CC) $(CFLAGS) $(INC) -o $(FMODE_OBJ) -c $(OBJDIR)/fmode.c

$(ETC)/ellipse.table: ellipse.table
	rm -f $@
	$(INSTALL) -m 644 $? $@

$(ETC)/datum.table: datum.table
	rm -f $@
	$(INSTALL) -m 644 $? $@

$(ETC)/datumtransform.table: datumtransform.table
	rm -f $@
	$(INSTALL) -m 644 $? $@

$(ETC)/FIPS.code: FIPS.code
	rm -f $@
	$(INSTALL) -m 644 $? $@

$(ETC)/state27: state27
	rm -f $@
	$(INSTALL) -m 644 $? $@

$(ETC)/state83: state83
	rm -f $@
	$(INSTALL) -m 644 $? $@

$(ETC)/projections: projections
	rm -f $@
	$(INSTALL) -m 644 $? $@

$(ETC)/gui.tcl: gui.tcl
	rm -f $@
	$(INSTALL) -m 644 $? $@

DOXNAME=gis
#check for program
checkdoxygen:
	@(type doxygen > /dev/null || (echo "ERROR: Install 'doxygen' software first (get from http://www.doxygen.org)" && exit 1))

htmldocs: checkdoxygen
# hack needed to get main page at beginning:
	@mv $(DOXNAME)lib.dox $(DOXNAME)lib.dox.org
	@cat $(DOXNAME)lib.dox.org | sed 's+/\*! \\page +/\*! \\mainpage +g' > $(DOXNAME)lib.dox
	doxygen ./Doxyfile_arch_html
	@mv $(DOXNAME)lib.dox.org $(DOXNAME)lib.dox
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
	@mv $(DOXNAME)lib.dox.org $(DOXNAME)lib.dox
	@echo "Latex reference in directory ./latex/refman.dvi"

pdfdocs: checkdoxygen
	test ! -d latex || (cd ./latex && $(MAKE) clean)
# hack needed to get main page at beginning:
	@mv $(DOXNAME)lib.dox $(DOXNAME)lib.dox.org
	@cat $(DOXNAME)lib.dox.org | sed 's+/\*! \\page +/\*! \\mainpage +g' > $(DOXNAME)lib.dox
	doxygen ./Doxyfile_arch_latex
#this hack is needed to make Acroread's search engine happy:
	(cd ./latex ; echo "\usepackage[T1]{fontenc}" >> doxygen.sty)
	(cd ./latex && $(MAKE) refman.pdf && mv refman.pdf grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR}${DOXNAME}_`date '+%Y_%m_%d'`_refman.pdf)
	@mv $(DOXNAME)lib.dox.org $(DOXNAME)lib.dox
	@echo "PDF reference in directory ./latex/grass${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR}${DOXNAME}_`date '+%Y_%m_%d'`_refman.pdf"

cleandocs:
	rm -rf ./latex ./html


$(OBJDIR)/area_ellipse.o: pi.h
$(OBJDIR)/area_sphere.o: pi.h
$(OBJDIR)/auto_mask.o: G.h
$(OBJDIR)/closecell.o: G.h
$(OBJDIR)/format.o: G.h
$(OBJDIR)/geodesic.o: pi.h
$(OBJDIR)/geodist.o: pi.h
$(OBJDIR)/get_row.o: G.h
$(OBJDIR)/get_window.o: G.h
$(OBJDIR)/gisinit.o: G.h
$(OBJDIR)/maskfd.o: G.h
$(OBJDIR)/opencell.o: G.h
$(OBJDIR)/put_row.o: G.h
$(OBJDIR)/radii.o: pi.h
$(OBJDIR)/rhumbline.o: pi.h
$(OBJDIR)/set_window.o: G.h
$(OBJDIR)/window_map.o: G.h
