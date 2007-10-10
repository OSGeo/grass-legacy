MODULE_TOPDIR = ../..

PGM = r.category

LIBES = $(GISLIB)
DEPENDENCIES = $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd
	-ln -s -f $(PGM) $(BIN)/r.cats
