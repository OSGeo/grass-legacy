MODULE_TOPDIR = ..

include $(MODULE_TOPDIR)/include/Make/Dir.make

SUBDIRS =

OPENGLBASED = nviz
MOTIFBASED = xganim

#compile if OPENGLBASED present:
ifneq ($(strip $(OPENGLLIB)),)
    SUBDIRS += $(OPENGLBASED)
endif

#compile if MOTIFBASED present:
ifneq ($(strip $(XMLIB)),)
    SUBDIRS += $(MOTIFBASED)
endif

default: subdirs

clean: cleansubdirs

