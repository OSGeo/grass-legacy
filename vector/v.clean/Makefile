
MODULE_TOPDIR = ../..

PGM = v.clean

LIBES = $(VECTLIB) $(GISLIB) $(VASKLIB)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd ctest

ctest:
	cd test; make
