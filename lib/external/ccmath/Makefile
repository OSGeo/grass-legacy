MODULE_TOPDIR = ../../..

EXTRA_LIBS=$(MATHLIB)
LIB_NAME = $(CCMATH_LIBNAME)

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: $(ARCH_INCDIR)/ccmath_grass.h
	$(MAKE) lib

$(ARCH_INCDIR)/ccmath_grass.h: ccmath_grass.h
	$(INSTALL_DATA) ccmath_grass.h $(ARCH_INCDIR)/ccmath_grass.h

#doxygen:
DOXNAME=ccmath
