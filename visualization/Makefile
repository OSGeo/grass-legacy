MODULE_TOPDIR = ..

OPENGLBASED = nviz

#compile if OPENGLBASED present:
ifneq ($(strip $(OPENGLLIB)),)
    SUBDIRS += $(OPENGLBASED)
endif

SUBDIRS = \
	nviz

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: subdirs

clean: cleansubdirs

