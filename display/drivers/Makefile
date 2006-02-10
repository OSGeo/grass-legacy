
MODULE_TOPDIR = ../..

SUBDIRS = lib
#commented unless HTMLMAP accepts G_plot_area():
# SUBDIRS = lib HTMLMAP

include $(MODULE_TOPDIR)/include/Make/Dir.make

# compile PNGdriver if lib available:
ifneq ($(USE_PNG),)
  SUBDIRS += PNG
endif
ifneq ($(USE_X11),)
  SUBDIRS += XDRIVER
endif

default: subdirs
	$(MKDIR) $(GISBASE)/docs/html
	$(INSTALL) -m 644 description.html $(GISBASE)/docs/html/displaydrivers.html


clean: cleansubdirs
