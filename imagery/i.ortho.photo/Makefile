
MODULE_TOPDIR = ../..

SUBDIRS = \
	menu \
	photo.2image \
	photo.2target \
	photo.camera \
	photo.elev \
	photo.init \
	photo.rectify \
	photo.target

include $(MODULE_TOPDIR)/include/Make/Dir.make

$(SUBDIRS): libes

default: parsubdirs

clean: cleansubdirs

libes:
	$(MAKE) -C $@

.PHONY: libes

