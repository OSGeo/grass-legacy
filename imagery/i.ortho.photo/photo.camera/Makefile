MODULE_TOPDIR = ../../..

PGM = photo.camera

EXTRA_CFLAGS = -I../libes

LIBES     = $(IMAGERYLIB) $(GISLIB) $(IORTHOLIB) $(VASKLIB) $(CURSES)
DEPENDENCIES= $(IMAGERYDEP) $(IORTHODEP) $(GISDEP) $(VASKDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: etc
