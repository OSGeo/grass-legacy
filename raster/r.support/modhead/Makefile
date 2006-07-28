MODULE_TOPDIR = ../../..

PGM = modhead$(EXE)

LIBES = $(EDITLIB) $(VASKLIB) $(GISLIB) $(CURSES)
DEPENDENCIES = $(GISDEP) $(EDITDEP) $(VASKDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: etc
	$(MKDIR) $(ETC)/support
	mv $(ETC)/$(PGM) $(ETC)/support

htmletc1:
	@echo "No docs to generate."

