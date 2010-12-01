MODULE_TOPDIR = ../../..

PGM = i.photo.camera

EXTRA_CFLAGS = -I../libes

LIBES     = $(IORTHOLIB) $(IMAGERYLIB) $(GISLIB) $(VASKLIB) $(CURSES) $(GMATHLIB)
DEPENDENCIES= $(IMAGERYDEP) $(IORTHODEP) $(GISDEP) $(VASKDEP) $(GMATHDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: etc

$(PGM).tmp.html: description.html
	$(INSTALL_DATA) $< $@
	$(INSTALL_DATA) *.png $(GISBASE)/docs/html/
