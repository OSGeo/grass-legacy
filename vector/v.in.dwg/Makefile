
MODULE_TOPDIR = ../..

PGM=v.in.dwg
ADINIT = $(GISBASE)/etc/adinit.dat

DEPENDENCIES = $(VECTDEP) $(DBMIDEP) $(GISDEP)
LIBES     = $(VECTLIB) $(DBMILIB) $(GISLIB) $(OPENDWGLIBPATH) $(OPENDWGLIB)
EXTRA_INC = $(VECT_INC) $(OPENDWGINCPATH)
EXTRA_CFLAGS = $(VECT_CFLAGS)
 
include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd $(ADINIT)

#a bit ugly:
$(ADINIT):
	cp `echo $(OPENDWGINCPATH) | cut -b3-`/adinit/adinit.dat $(ADINIT)
