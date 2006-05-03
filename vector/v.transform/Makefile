
MODULE_TOPDIR = ../..

PGM=v.transform

ifdef MINGW
EXTRA_LIB="$(VASKLIB) $(CURSES)"
endif

DEPENDENCIES = $(VECTDEP) $(DBMIDEP) $(GISDEP)
LIBES     = $(VECTLIB) $(DBMILIB) $(TRANSLIB) $(EXTRA_LIB) $(GISLIB)
EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS)
 
include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd

