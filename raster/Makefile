
MODULE_TOPDIR = ..

SUBDIRS1 = \
	r.bilinear \
	r.buffer \
	r.clump \
	r.colors \
	r.composite \
	r.cost \
	r.his \
	r.mapcalc \
	r.proj \
	r.neighbors \
	r.stats

GDALBASED = r.in.gdal

include $(MODULE_TOPDIR)/include/Make/Dir.make

#compile if GDAL present:
ifneq ($(strip $(GDALLIBS)),)
    SUBDIRS2 = $(GDALBASED)
endif


SUBDIRS = $(SUBDIRS1) $(SUBDIRS2)
default: subdirs

clean: cleansubdirs
