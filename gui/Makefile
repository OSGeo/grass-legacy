MODULE_TOPDIR = ..

SUBDIRS = icons scripts xml

TCLTKBASED = tcltk
PYTHONBASED = wxpython

include $(MODULE_TOPDIR)/include/Make/Platform.make

#compile if TCLTKBASED present:
ifneq ($(strip $(TCLTKLIBS)),)
    SUBDIRS += $(TCLTKBASED)
endif

#compile if wxPython present:
ifneq ($(strip $(USE_WXPYTHON)),)
    SUBDIRS += $(PYTHONBASED)
endif

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: parsubdirs

clean: cleansubdirs

