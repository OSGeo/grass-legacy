MODULE_TOPDIR = ../..

EXTRA_LIBS=$(GISLIB) $(BITMAPLIB) $(VECTLIB) $(XLIBPATH) $(OPENGLLIB) $(OPENGLULIB) $(TIFFLIB) $(IMAGELIB) $(SITESLIB) $(G3DLIB)

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

