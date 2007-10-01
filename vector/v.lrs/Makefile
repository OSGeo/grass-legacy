
MODULE_TOPDIR = ../..

SUBDIRS = \
	v.lrs.create \
	v.lrs.segment \
	v.lrs.label \
	v.lrs.where

#to install documentation:
PGM = lrs

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: parsubdirs htmldir

clean: cleansubdirs

$(SUBDIRS): lrslib

lrslib:
	$(MAKE) -C lib

.PHONY: lrslib

