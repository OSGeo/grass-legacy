
MODULE_TOPDIR = ..

#order is relevant:
SUBDIRS = \
	datetime \
	gis \
	linkm \
	raster \
	bitmap \
	btree \
	display \
	D \
	db \
	vask \
	edit \
	external \
	fonts \
	gmath \
	gtcltk \
	form \
	image \
	imagery \
	proj \
	rowio \
	segment \
	vector \
	front.end \
	rst \
	ogsf \
	symbol \
	init

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: headers subdirs

headers: 
	if [ ! -d $(GISBASE)/include ]; then mkdir $(GISBASE)/include; fi
	if [ ! -d $(GISBASE)/include/vect ]; then mkdir $(GISBASE)/include/vect; fi
	cp ../include/*.h $(GISBASE)/include/
	cp ../include/vect/*.h $(GISBASE)/include/vect/

clean: cleansubdirs

