
MODULE_TOPDIR = ..

SUBDIRS = \
	v.buffer \
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
	v.hull \
	v.info \
	v.in.ascii \
	v.in.db \
	v.in.region \
	v.in.sites \
	v.label \
	v.proj \
	v.net \
	v.net.alloc \
	v.net.iso \
	v.net.path \
	v.net.salesman \
	v.net.steiner \
	v.out.ascii \
	v.out.pov \
	v.overlay \
	v.patch \
	v.random \
	v.reclass \
	v.segment \
	v.select \
	v.surf.idw \
	v.surf.rst \
	v.transform \
	v.to.db \
	v.to.points \
	v.to.rast \
	v.type \
	v.univar \
	v.what.rast\
	v.vol.rst

include $(MODULE_TOPDIR)/include/Make/Dir.make

OGRBASED = v.out.ogr v.in.ogr v.external

#compile if OGR present:
ifneq ($(USE_OGR),)
    SUBDIRS += $(OGRBASED)
endif


OPENDWGBASED = v.in.dwg

#compile if OPENDWG present:
ifneq ($(USE_OPENDWG),)
    SUBDIRS += $(OPENDWGBASED)
endif

default: subdirs

clean: cleansubdirs
