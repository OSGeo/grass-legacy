
MODULE_TOPDIR = ..

include $(MODULE_TOPDIR)/include/Make/Dir.make

SUBDIRS = \
	drivers \
	d.ask \
	d.barscale \
	d.colorlist \
	d.colors \
	d.colortable \
	d.erase \
	d.extend \
	d.font \
	d.frame \
	d.geodesic \
	d.graph \
	d.grid \
	d.his \
	d.histogram \
	d.info \
	d.legend \
	d.linegraph \
	d.m \
	d.mapgraph \
	d.measure \
	d.mon \
	d.nviz \
	d.paint.labels \
	d.path \
	d.profile \
	d.rast \
	d.rast.arrow \
	d.rast.edit \
	d.rast.num \
	d.rgb \
	d.rhumbline \
	d.save \
	d.text \
	d.title \
	d.vect \
	d.vect.chart \
	d.what.rast \
	d.what.vect \
	d.where \
	d.zoom

FREETYPEBASED = d.text.freetype d.font.freetype

#compile if FreeType Lib present:
ifneq ($(strip $(FTLIB)),)
    SUBDIRS += $(FREETYPEBASED)
endif

default: subdirs

clean: cleansubdirs

