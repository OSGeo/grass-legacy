MODULE_TOPDIR = ../../..

LIB_NAME = $(INTERPDATA_LIBNAME)
DEPENDENCIES = $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: $(ARCH_INCDIR)/dataquad.h lib

$(ARCH_INCDIR)/dataquad.h: dataquad.h
	$(INSTALL) -m 644 dataquad.h $(ARCH_INCDIR)/dataquad.h
	$(INSTALL) -m 644 dataquad.h $(ARCH_DISTDIR)/include/dataquad.h

