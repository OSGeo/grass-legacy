MODULE_TOPDIR = ../../..

EXTRA_LIBS=$(GISLIB) $(VECTLIB) $(BITMAPLIB) $(QTREELIB) $(INTERPDATALIB) $(DBMILIB) $(GMATHLIB)

LIB_NAME = $(INTERPFL_LIBNAME)

DEPENDENCIES = $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Lib.make

EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS)

default: $(ARCH_INCDIR)/interpf.h lib

$(ARCH_INCDIR)/interpf.h: interpf.h
	cp interpf.h $(ARCH_INCDIR)/interpf.h

