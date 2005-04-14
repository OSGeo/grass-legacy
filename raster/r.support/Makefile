MODULE_TOPDIR = ../..

SUBDIRS = \
	modcats \
	modcolr \
	modhist	\
	modhead	\
	front

PGM = r.support

LIBES = $(GISLIB)
DEPENDENCIES = $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: subdirs

clean: cleansubdirs
