MODULE_TOPDIR = ../../..

PGM = i.ortho.photo

EXTRA_CFLAGS = -I../libes

LIBES     = $(IMAGERYLIB) $(GISLIB) $(IORTHOLIB) $(VASKLIB) $(CURSES) $(GMATHLIB)
DEPENDENCIES= $(IMAGERYDEP) $(IORTHODEP) $(GISDEP) $(VASKDEP) $(GMATHDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd

htmlcmd1:
	$(MKDIR) $(GISBASE)/docs/html
	$(INSTALL) -m 644 description.html $(GISBASE)/docs/html/$(PGM).html
