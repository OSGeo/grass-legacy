MODULE_TOPDIR = ../../..

PGM = r.le.setup

LIBES = $(DISPLAYLIB) $(RASTERLIB) $(SITESLIB) $(VECTLIB) $(GISLIB) $(DATETIMELIB) $(VASKLIB) $(CURSES)
DEPENDENCIES = $(RASTERDEP) $(DISPLAYDEP) $(SITESDEP) $(VECTDEP) $(GISDEP) $(DATETIMEDEP) $(VASKDEP)
EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS)
EXTRA_CLEAN_DIRS = polytocell

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd polytocell

polytocell:
	$(MAKE) -C $@

.PHONY: polytocell
