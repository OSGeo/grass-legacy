
MODULE_TOPDIR = ../..

SUBDIRS = \
	lib \
	monitorcap \
	XDRIVER 
#commented unless HTMLMAP accepts G_plot_area():
#	HTMLMAP

include $(MODULE_TOPDIR)/include/Make/Dir.make

# compile PNGdriver if lib available:
ifneq ($(USE_PNG),)
  SUBDIRS += PNG
endif

default: subdirs
	mkdir -p $(GISBASE)/docs/html
	cp description.html $(GISBASE)/docs/html/displaydrivers.html


clean: cleansubdirs
