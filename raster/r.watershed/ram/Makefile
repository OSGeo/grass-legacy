MODULE_TOPDIR = ../../..

PGM = ram

LIBES = $(GISLIB)
DEPENDENCIES = $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: etc 
	$(MKDIR) $(ETC)/water
	$(INSTALL) -m 755 $(ETC)/$(PGM) $(ETC)/water

htmletc1:
	@echo "No docs to generate." 
