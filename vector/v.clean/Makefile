
MODULE_TOPDIR = ../..

PGM = v.clean

LIBES = $(VECTLIB) $(DBMILIB) $(GISLIB) $(VASKLIB)
DEPENDENCIES= $(VECTDEP) $(DBMIDEP) $(GISDEP) $(VASKDEP)
EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd ctest

ctest:
	cd test; make
