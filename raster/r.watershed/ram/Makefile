MODULE_TOPDIR = ../../..

PGM = ram

LIBES = $(GISLIB)
DEPENDENCIES = $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: etc 
	$(MKDIR) $(ETC)/water
	$(INSTALL) $(ETC)/$(PGM) $(ETC)/water

htmletc:
	@echo "No docs to generate." 
