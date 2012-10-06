MODULE_TOPDIR = ..

SUBDIRS = icons images scripts xml tcltk

include $(MODULE_TOPDIR)/include/Make/Platform.make

#compile if python present:
ifneq ($(USE_PYTHON),)
    SUBDIRS += wxpython
endif

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: parsubdirs

clean: cleansubdirs
