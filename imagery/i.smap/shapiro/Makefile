MODULE_TOPDIR = ../../..

PGM = i.smap

EXTRA_CFLAGS = -I../bouman/

LIBES     = $(ISMAPLIB) $(IMAGERYLIB) $(GISLIB) $(VASKLIB) $(CURSES) $(GMATHLIB)
DEPENDENCIES= $(ISMAPDEP) $(IMAGERYDEP) $(GISDEP) $(VASKDEP) $(GMATHDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd
