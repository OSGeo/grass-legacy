MODULE_TOPDIR = ../../..

PGM = i.smap

EXTRA_CFLAGS = -I../bouman/

LIBES     = $(ISMAPLIB) $(IMAGERYLIB) $(GISLIB) $(VASKLIB) $(CURSES)
DEPENDENCIES= $(ISMAPDEP) $(IMAGERYDEP) $(GISDEP) $(VASKDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd
