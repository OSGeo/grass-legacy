
MODULE_TOPDIR = ../..

PGM=v.in.ogr

DEPENDENCIES = $(VECTDEP) $(DBMIDEP) $(GISDEP)
LIBES     = $(VECTLIB) $(DBMILIB) $(GISLIB) $(GDALLIBS)
EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS)
 
include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd

