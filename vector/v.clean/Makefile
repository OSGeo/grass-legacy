
MODULE_TOPDIR = ../..

PGM = v.clean

LIBES = $(VECTLIB) $(GISLIB)
DEPENDENCIES= $(VECTDEP) $(GISDEP)
EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS)
EXTRA_CLEAN_DIRS = test

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd ctest

ctest:
	cd test; $(MAKE)
