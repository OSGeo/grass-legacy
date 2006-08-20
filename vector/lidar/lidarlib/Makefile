MODULE_TOPDIR = ../../..

EXTRA_LIBS = $(VECTLIB) $(GISLIB) $(MATHLIB) $(SEGMENTLIB) $(GMATHLIB)
DEPENDENCIES= $(VECTDEP) $(GISDEP) $(BSPLINE_ARCH_OBJ) $(GMATHDEP)

EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS)

LIDAR_LIBNAME      = grass_lidar

LIB_NAME = $(LIDAR_LIBNAME)

#LIB_OBJS = 

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: $(ARCH_INCDIR)/PolimiFunct.h lib

$(ARCH_INCDIR)/PolimiFunct.h: PolimiFunct.h
	cp PolimiFunct.h $(ARCH_INCDIR)/PolimiFunct.h
