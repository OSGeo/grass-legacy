
MODULE_TOPDIR = ..

SUBDIRS1 = \
	v.build \
	v.build.polylines \
	v.category \
	v.clean \
	v.convert \
	v.database \
	v.db.connect \
	v.digit \
	v.distance \
	v.extract \
	v.format \
	v.info \
	v.in.ascii \
	v.in.db \
	v.in.sites \
	v.label \
	v.proj \
	v.net.alloc \
	v.net.iso \
	v.net.salesman \
	v.net.steiner \
	v.out.ascii \
	v.patch \
	v.surf.rst \
	v.transform \
	v.to.db \
	v.to.rast

include $(MODULE_TOPDIR)/include/Make/Dir.make

OGRBASED = v.out.ogr v.in.ogr

#compile if OGR present:
ifneq ($(strip $(GDALLIBS)),)
    SUBDIRS2 = $(OGRBASED)
endif


OPENDWGBASED = v.in.dwg

#compile if OPENDWG present:
ifneq ($(strip $(OPENDWGINCPATH)),)
    SUBDIRS3 = $(OPENDWGBASED)
endif

SUBDIRS = $(SUBDIRS1) $(SUBDIRS2) $(SUBDIRS3)

default: subdirs

clean: cleansubdirs
