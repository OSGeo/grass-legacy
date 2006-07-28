
MODULE_TOPDIR = ../..

PGM = d.labels

LIBES        = $(D_LIB) $(DISPLAYLIB) $(RASTERLIB) $(GISLIB)

DEPENDENCIES = $(DISPLAYDEP) $(RASTERDEP) $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd
	-ln -s -f $(PGM) $(BIN)/d.paint.labels



