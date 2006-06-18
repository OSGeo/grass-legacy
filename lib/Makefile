MODULE_TOPDIR = ..

#order is relevant:
SUBDIRS = \
	datetime \
	gis \
	gmath \
	linkm \
	driver \
	pngdriver \
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
include $(MODULE_TOPDIR)/include/Make/Doxygen.make

OPENGLBASED = ogsf

#compile if OPENGLBASED present:
ifneq ($(strip $(OPENGLLIB)),)
    SUBDIRS += $(OPENGLBASED)
endif

default: headers subdirs

headers: 
	if [ ! -d $(GISBASE)/include/grass ]; then $(MKDIR) $(GISBASE)/include/grass; fi
	if [ ! -d $(GISBASE)/include/grass/vect ]; then $(MKDIR) $(GISBASE)/include/grass/vect; fi
	$(INSTALL) -m 644 ../include/*.h $(GISBASE)/include/grass/
	$(INSTALL) -m 644 ../include/vect/*.h $(GISBASE)/include/grass/vect/

clean: cleansubdirs

#doxygen:
DOXNAME=grasslibs
