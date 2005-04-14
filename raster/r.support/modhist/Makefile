MODULE_TOPDIR = ../../..

PGM = modhist

LIBES = $(GISLIB) $(EDITLIB)
DEPENDENCIES = $(GISDEP) $(EDITDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: etc
	$(MKDIR) $(ETC)/support
	$(INSTALL) -m755 $(ETC)/$(PGM) $(ETC)/support

htmletc1:
	@echo "No docs to generate."

