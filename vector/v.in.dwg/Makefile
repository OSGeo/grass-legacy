
MODULE_TOPDIR = ../..

PGM=v.in.dwg
ADINIT = $(GISBASE)/etc/adinit.dat

DEPENDENCIES = $(VECTDEP) $(DBMIDEP) $(GISDEP)
LIBES     = $(VECTLIB) $(DBMILIB) $(GISLIB) ad2.a
EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS)
 
include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd $(ADINIT)

$(ADINIT): adinit.dat
	cp adinit.dat $(ADINIT)

