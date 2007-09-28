MODULE_TOPDIR = ../../..

PGM = r.watershed.ram

LIBES = $(GISLIB)
DEPENDENCIES = $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: etc 
	$(MKDIR) $(ETC)/water
	$(INSTALL) -m 755 $(ETC)/$(PGM) $(ETC)/water

htmletc:
	@echo "No docs to generate." 
