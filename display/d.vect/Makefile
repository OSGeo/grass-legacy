
MODULE_TOPDIR = ../..

PGM = d.vect

LIBES        = $(DISPLAYLIB) $(RASTERLIB) $(VECTLIB) $(DBMILIB) $(GISLIB)

DEPENDENCIES = $(DISPLAYDEP) $(RASTERDEP) $(VECTDEP) $(DBMIDEP) $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: cmd

html:
	GISBASE=$(GISBASE)
	GISRC=/tmp/grass51_makefile_gisrc.$(PGM)
	echo $(GISBASE)
	result=`echo "LOCATION_NAME=none.$(PGM)" > $(GISRC)`
	LOCATION_NAME=none.$(PGM)
	$(GISBASE)/etc/bin/cmd/$(PGM) --html-description
