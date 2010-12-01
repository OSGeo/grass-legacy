
MODULE_TOPDIR = ../..

SUBDIRS1 = \
	menu \
	i.photo.2image \
	i.photo.2target \
	i.photo.camera \
	i.photo.elev \
	i.photo.init \
	i.photo.rectify \
	i.photo.target

SUBDIRS = libes $(SUBDIRS1)

include $(MODULE_TOPDIR)/include/Make/Dir.make

$(SUBDIRS1): libes

default: parsubdirs

clean: cleansubdirs

