MODULE_TOPDIR = ../..

EXTRA_CFLAGS= $(OPENGLINC) $(XCFLAGS) $(TIFFINCPATH)

LIB_NAME = $(OGSF_LIBNAME)

include $(MODULE_TOPDIR)/include/Make/Lib.make

EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS)

default: lib inc

inc:
	cp gsurf.h $(ARCH_INCDIR)/gsurf.h
	cp kftypes.h $(ARCH_INCDIR)/kftypes.h
	cp keyframe.h $(ARCH_INCDIR)/keyframe.h
	cp local_proto.h $(ARCH_INCDIR)/ogsf_proto.h
	cat gstypes.h | sed -e 's/local_proto.h/ogsf_proto.h/' > $(ARCH_INCDIR)/gstypes.h

