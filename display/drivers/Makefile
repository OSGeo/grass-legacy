
MODULE_TOPDIR = ../..

SUBDIRS = \
	lib \
	monitorcap \
	XDRIVER

include $(MODULE_TOPDIR)/include/Make/Dir.make

ifneq ($(USE_GD),)
  SUBDIRS += PNG
endif

default: subdirs

clean: cleansubdirs

