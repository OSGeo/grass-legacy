MODULE_TOPDIR = ..

include $(MODULE_TOPDIR)/include/Make/Dir.make

SUBDIRS = icons scripts

TCLTKBASED = tcltk

#compile if TCLTKBASED present:
ifneq ($(strip $(TCLTKLIBS)),)
    SUBDIRS += $(TCLTKBASED)
endif

default: htmldir

htmldir: subdirs

clean: cleansubdirs

