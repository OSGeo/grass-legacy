MODULE_TOPDIR = ../../..

PGM = photo.camera

EXTRA_CFLAGS = -I../libes

LIBES     = $(IORTHOLIB) $(IMAGERYLIB) $(GISLIB) $(VASKLIB) $(CURSES)
DEPENDENCIES= $(IMAGERYDEP) $(IORTHODEP) $(GISDEP) $(VASKDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: etc
