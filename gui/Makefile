MODULE_TOPDIR = ..

SUBDIRS = icons scripts

TCLTKBASED = tcltk
PYTHONBASED = wxpython

include $(MODULE_TOPDIR)/include/Make/Platform.make

#compile if TCLTKBASED present:
ifneq ($(strip $(TCLTKLIBS)),)
    SUBDIRS += $(TCLTKBASED)
endif

#compile if Python present:
ifneq ($(strip $(USE_PYTHON)),)
    SUBDIRS += $(PYTHONBASED)
endif

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: parsubdirs

clean: cleansubdirs

