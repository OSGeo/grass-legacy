MODULE_TOPDIR = ../..

EXTRA_LIBS=$(GISLIB) $(BITMAPLIB) $(VECTLIB) $(OPENGLLIB) $(TIFFLIB) $(IMAGELIB) $(SITESLIB) $(XLIBPATH)

LIB_NAME = $(OGSF_LIBNAME)

include $(MODULE_TOPDIR)/include/Make/Lib.make

EXTRA_INC = $(VECT_INC) $(OPENGLINC) $(TIFFINCPATH)
EXTRA_CFLAGS = $(VECT_CFLAGS) $(XCFLAGS)

default: lib inc

inc:
	$(INSTALL) -m 644 gsurf.h $(ARCH_INCDIR)/gsurf.h
	$(INSTALL) -m 644 kftypes.h $(ARCH_INCDIR)/kftypes.h
	$(INSTALL) -m 644 keyframe.h $(ARCH_INCDIR)/keyframe.h
	$(INSTALL) -m 644 local_proto.h $(ARCH_INCDIR)/ogsf_proto.h
	cat gstypes.h | sed -e 's/local_proto.h/ogsf_proto.h/' > $(ARCH_INCDIR)/gstypes.h

