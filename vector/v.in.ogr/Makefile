
MODULE_TOPDIR = ../..

PGM=v.in.ogr
EXTRA_CFLAGS = $(GDALCFLAGS)

DEPENDENCIES = $(VECTDEP) $(DBMIDEP) $(GISDEP)
LIBES     = $(VECTLIB) $(DBMILIB) $(GISLIB) $(GDALLIBS)
 
include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd

