MODULE_TOPDIR = ..

SUBDIRS = icons scripts

TCLTKBASED = tcltk

include $(MODULE_TOPDIR)/include/Make/Platform.make

#compile if TCLTKBASED present:
ifneq ($(strip $(TCLTKLIBS)),)
    SUBDIRS += $(TCLTKBASED)
endif

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: parsubdirs

clean: cleansubdirs

