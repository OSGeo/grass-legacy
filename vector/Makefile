
MODULE_TOPDIR = ..

SUBDIRS1 = \
	v.ascii \
	v.build \
	v.build.polylines \
	v.category \
	v.clean \
	v.convert \
	v.database \
	v.db.connect \
	v.digit \
	v.extract \
	v.format \
	v.info \
	v.in.sites \
	v.patch \
	v.surf.rst \
	v.transform \
	v.to.rast

OGRBASED = v.out.ogr v.in.ogr

include $(MODULE_TOPDIR)/include/Make/Dir.make

#compile if OGR present:
ifneq ($(strip $(GDALLIBS)),)
    SUBDIRS2 = $(SUBDIRS1) $(OGRBASED)
else
    SUBDIRS2 = $(SUBDIRS1)
endif


SUBDIRS = $(SUBDIRS2)
default: subdirs

clean: cleansubdirs
