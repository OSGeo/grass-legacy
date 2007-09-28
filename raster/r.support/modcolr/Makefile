MODULE_TOPDIR = ../../..

PGM = modcolr

LIBES = $(GISLIB) 
DEPENDENCIES = $(GISDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: etc
	$(MKDIR) $(ETC)/support
	mv $(ETC)/$(PGM)$(EXE) $(ETC)/support

htmletc:
	@echo "No docs to generate."

