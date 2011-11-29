
MODULE_TOPDIR = ../..

SUBDIRS = \
	ccmath \
	shapelib

include $(MODULE_TOPDIR)/include/Make/Dir.make

TCLTKBASED = \
	bwidget

#compile if TCLTKLIBS present:
ifneq ($(strip $(TCLTKLIBS)),)
    SUBDIRS += $(TCLTKBASED)
endif

default:
	$(MAKE) subdirs

clean: cleansubdirs

