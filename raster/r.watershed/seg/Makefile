MODULE_TOPDIR = ../../..

PGM = seg

LIBES = $(GISLIB) $(SEGMENTLIB)
DEPENDENCIES = $(GISDEP) $(SEGMENTDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: etc
	mkdir -p $(ETC)/water
	mv $(ETC)/$(PGM) $(ETC)/water

htmlcmd:
	echo "nothing to do"
