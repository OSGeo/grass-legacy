
MODULE_TOPDIR = ../..

PGM = v.clean

LIBES = $(VECTLIB) $(DBMILIB) $(GISLIB) $(VASKLIB)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd ctest

ctest:
	cd test; make
