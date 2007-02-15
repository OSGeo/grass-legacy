
MODULE_TOPDIR = ../..

SUBDIRS = HTMLMAP

include $(MODULE_TOPDIR)/include/Make/Dir.make

# compile PNGdriver if lib available:
ifneq ($(USE_PNG),)
  SUBDIRS += PNG
endif
ifneq ($(USE_X11),)
  SUBDIRS += XDRIVER
endif

default: subdirs $(ETC)/monitorcap
	$(MKDIR) $(GISBASE)/docs/html
	$(INSTALL) -m 644 description.html $(GISBASE)/docs/html/displaydrivers.html

# Touch an empty monitorcap if none exists (e.g. native Windows)
$(ETC)/monitorcap:
	if [ ! -f $@ ] ; then touch $@ ; fi

clean: cleansubdirs
