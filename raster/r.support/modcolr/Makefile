MODULE_TOPDIR = ../../..

PGM = modcolr

LIBES = $(GISLIB) 
DEPENDENCIES = $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: etc
	$(MKDIR) $(ETC)/support
	$(INSTALL) -m 755 $(ETC)/$(PGM) $(ETC)/support

htmletc1:
	@echo "No docs to generate."

