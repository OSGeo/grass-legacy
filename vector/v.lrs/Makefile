
MODULE_TOPDIR = ../..

DOC=$(GISBASE)/docs/html/lrs.html

SUBDIRS = \
	lib \
	v.lrs.create \
	v.lrs.segment \
	v.lrs.label \
	v.lrs.where

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: subdirs $(DOC)

$(DOC): lrs.html
	$(INSTALL) -m 644 lrs.html $(DOC)

clean: cleansubdirs

