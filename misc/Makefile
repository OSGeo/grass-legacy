MODULE_TOPDIR = ..

include $(MODULE_TOPDIR)/include/Make/Platform.make

SUBDIRS = \
	m.cogo

ifneq ($(strip $(OPENGLLIB)),)
    SUBDIRS += m.nviz.image
endif

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: parsubdirs

clean: cleansubdirs
