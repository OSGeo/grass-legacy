MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Module.make

PGM = r.out.tiff
EXTRA_CFLAGS = $(TIFFINCPATH) $(TIFFLIBPATH)

LIBES = $(GISLIB) $(TIFFLIB)
DEPENDENCIES = $(GISDEP)

default: cmd
