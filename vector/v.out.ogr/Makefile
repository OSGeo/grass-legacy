
MODULE_TOPDIR = ../..

PGM=v.out.ogr
EXTRA_CFLAGS = $(GDALCFLAGS)

OBJS = main.o

DEPENDENCIES = $(VECTLIB) $(VECTLIB_REAL) $(DBMILIB) $(GISLIB)
LIBES     = $(VECTLIB) $(VECTLIB_REAL) $(DBMILIB) $(GISLIB) $(GDALLIBS)
 
include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd

