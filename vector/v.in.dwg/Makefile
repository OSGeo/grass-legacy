
MODULE_TOPDIR = ../..

PGM=v.in.dwg
ADINIT = $(GISBASE)/etc/adinit.dat

DEPENDENCIES = $(VECTDEP) $(DBMIDEP) $(GISDEP)
LIBES     = $(VECTLIB) $(DBMILIB) $(GISLIB) $(OPENDWGLIB)
EXTRA_INC = $(VECT_INC) $(OPENDWGINC)
EXTRA_CFLAGS = $(VECT_CFLAGS)
 
include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd $(ADINIT)

$(ADINIT): adinit.dat
	cp adinit.dat $(ADINIT)

