
MODULE_TOPDIR = ..

SUBDIRS1 = \
	r.buffer \
	r.clump \
	r.colors \
	r.composite \
	r.cost \
	r.gdal \
	r.his \
	r.mapcalc

GDALBASED = r.gdal

include $(MODULE_TOPDIR)/include/Make/Dir.make

#compile if GDAL present:
ifneq ($(strip $(GDALLIBS)),)
    SUBDIRS2 = $(SUBDIRS1) $(GDALBASED)
else
    SUBDIRS2 = $(SUBDIRS1)
endif


SUBDIRS = $(SUBDIRS2)
default: subdirs

clean: cleansubdirs
