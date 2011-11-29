MODULE_TOPDIR = ..

SUBDIRS = icons images scripts xml

include $(MODULE_TOPDIR)/include/Make/Platform.make

#compile if Tcl/Tk present:
ifneq ($(strip $(TCLTKLIBS)),)
    SUBDIRS += tcltk
endif

#compile if wxwidgets present:
ifneq ($(strip $(WXVERSION)),)
    SUBDIRS += wxpython
endif

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: parsubdirs

clean: cleansubdirs
