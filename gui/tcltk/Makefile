MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Dir.make

SUBDIRS =

TCLTKBASED = d.m gis.m menus

#compile if TCLTKBASED present:
ifneq ($(strip $(TCLTKLIBS)),)
    SUBDIRS += $(TCLTKBASED)
endif

default: subdirs

clean: cleansubdirs

