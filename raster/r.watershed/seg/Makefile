MODULE_TOPDIR = ../../..

PGM = seg

LIBES = $(GISLIB) $(SEGMENTLIB)
DEPENDENCIES = $(GISDEP) $(SEGMENTDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: etc
	$(MKDIR) $(ETC)/water
	$(INSTALL) -m 755 $(ETC)/$(PGM) $(ETC)/water

htmletc1:
	@echo "No docs to generate."
