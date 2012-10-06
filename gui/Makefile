MODULE_TOPDIR = ..

SUBDIRS = icons images scripts xml

include $(MODULE_TOPDIR)/include/Make/Platform.make

#compile if Tcl/Tk present:
ifneq ($(strip $(TCLTKLIBS)),)
    SUBDIRS += tcltk
endif

#compile if python present:
ifneq ($(USE_PYTHON),)
    SUBDIRS += wxpython
endif

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: parsubdirs

clean: cleansubdirs
