MODULE_TOPDIR = ../../..

EXTRA_LIBS = $(GMATHLIB) $(GISLIB) $(SITESLIB) $(VECTLIB) $(BITMAPLIB) $(QTREELIB) $(INTERPDATALIB)

LIB_NAME = $(INTERPFL_LIBNAME)

LIB_OBJS = func2d.o input2d.o vinput2d.o interp2d.o output2d.o point2d.o \
	   secpar2d.o segmen2d.o write2d.o matrix.o minmax.o init2d.o ressegm2d.o resout2d.o

DEPENDENCIES = $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Lib.make

EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS)

default: $(ARCH_INCDIR)/interpf.h lib

$(ARCH_INCDIR)/interpf.h: interpf.h
	$(INSTALL) -m 644 interpf.h $(ARCH_INCDIR)/interpf.h
	$(INSTALL) -m 644 interpf.h $(ARCH_DISTDIR)/include/interpf.h

