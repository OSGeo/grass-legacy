MODULE_TOPDIR = ../../..

PACKAGE ="libvect"
DEFS=-DPACKAGE=\"$(PACKAGE)\"

EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS = $(GETHOSTNAME) $(ZLIBINCPATH) $(VECT_CFLAGS) $(DEFS)
EXTRA_LIBS=$(GISLIB) $(DIG2LIB) $(GRAPHLIB) $(RTREELIB) $(LINKMLIB) $(DBMILIB) $(GDALLIBS)

LIB_NAME = $(VECTR_LIBNAME)
DEPENDENCIES =  $(GRASS_INCDIR)/Vect.h $(GRASS_INCDIR)/V_.h \
		$(GRASS_INCDIR)/vect/dig_defines.h \
		$(GRASS_INCDIR)/vect/dig_macros.h $(GRASS_INCDIR)/vect/dig_structs.h \
               	$(GRASS_INCDIR)/vect/dig_externs.h $(GRASS_INCDIR)/vect/dig_globs.h $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: lib

