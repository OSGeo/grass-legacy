
MODULE_TOPDIR = ../../..

LIB_NAME = $(SHAPE_LIBNAME)

LIB_OBJS = \
	shpopen.o \
	dbfopen.o

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: $(ARCH_INCDIR)/shapefil.h lib

$(ARCH_INCDIR)/shapefil.h: shapefil.h
	cp shapefil.h $(ARCH_INCDIR)/shapefil.h
