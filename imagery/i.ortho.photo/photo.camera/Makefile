MODULE_TOPDIR = ../../..

PGM = photo.camera

EXTRA_CFLAGS = -I../libes

LIBES     = $(IORTHOLIB) $(IMAGERYLIB) $(GISLIB) $(VASKLIB) $(CURSES) $(GMATHLIB)
DEPENDENCIES= $(IMAGERYDEP) $(IORTHODEP) $(GISDEP) $(VASKDEP) $(GMATHDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: etc

htmletc1:
	$(MKDIR) $(GISBASE)/docs/html
	$(INSTALL) -m 644 description.html $(GISBASE)/docs/html/$(PGM).html
	$(INSTALL) -m 644 *.png $(GISBASE)/docs/html/
