MODULE_TOPDIR = ..

#order is relevant:
SUBDIRS = \
	datetime \
	gis \
	gmath \
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
	gtcltk \
	form \
	image \
	imagery \
	proj \
	rowio \
	segment \
	vector \
	sites \
	rst \
	g3d \
	dspf \
	symbol \
	init \
	cdhc

include $(MODULE_TOPDIR)/include/Make/Dir.make

OPENGLBASED = ogsf

#compile if OPENGLBASED present:
ifneq ($(strip $(OPENGLLIB)),)
    SUBDIRS += $(OPENGLBASED)
endif

default: headers subdirs

headers: 
	if [ ! -d $(GISBASE)/include ]; then $(MKDIR) $(GISBASE)/include; fi
	if [ ! -d $(GISBASE)/include/vect ]; then $(MKDIR) $(GISBASE)/include/vect; fi
	$(INSTALL) -m 644 ../include/*.h $(GISBASE)/include/
	$(INSTALL) -m 644 ../include/vect/*.h $(GISBASE)/include/vect/
	if [ ! -d $(GISBASE)/include/grass ]; then $(MKDIR) $(GISBASE)/include/grass; fi
	if [ ! -d $(GISBASE)/include/grass/vect ]; then $(MKDIR) $(GISBASE)/include/grass/vect; fi
	$(INSTALL) -m 644 ../include/*.h $(GISBASE)/include/grass/
	$(INSTALL) -m 644 ../include/vect/*.h $(GISBASE)/include/grass/vect/

clean: cleansubdirs

