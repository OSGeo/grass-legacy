MODULE_TOPDIR = ../../..

PGM = ram

LIBES = $(GISLIB)
DEPENDENCIES = $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: etc 
	mkdir -p $(ETC)/water
	mv $(ETC)/$(PGM) $(ETC)/water

htmletc:
	@echo "No docs to generate." 
