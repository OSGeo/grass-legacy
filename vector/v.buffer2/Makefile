MODULE_TOPDIR = ../..

PGM = v.buffer

LIBES = $(VECTLIB) $(DBMILIB) $(GISLIB)  $(GEOSLIBS)
DEPENDENCIES= $(VECTDEP) $(DBMIDEP) $(GISDEP)
EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS) $(GEOSCFLAGS)

include $(MODULE_TOPDIR)/include/Make/Module.make

ifneq ($(strip $(USE_GEOS)),)
default: cmd
endif
