
MODULE_TOPDIR = ../..

LIB_NAME = $(FORM_LIBNAME)
DEPENDENCIES = $(GRASS_INCDIR)/form.h

HTMLLIB=$(GISBASE)/etc/form/html_library.tcl
FORM=$(GISBASE)/etc/form/form.tcl

include $(MODULE_TOPDIR)/include/Make/Lib.make

default: lib $(HTMLLIB) $(FORM)

$(HTMLLIB): html_library_grass.tcl
	if [ ! -d $(GISBASE)/etc ]; then mkdir $(GISBASE)/etc; fi
	if [ ! -d $(GISBASE)/etc/form ]; then mkdir $(GISBASE)/etc/form; fi
	cp html_library_grass.tcl $(HTMLLIB)

$(FORM): form.tcl
	cp form.tcl $(FORM)
