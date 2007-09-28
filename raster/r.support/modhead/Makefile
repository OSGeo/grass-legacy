MODULE_TOPDIR = ../../..

PGM = modhead

LIBES = $(EDITLIB) $(VASKLIB) $(GISLIB) $(CURSES)
DEPENDENCIES = $(GISDEP) $(EDITDEP) $(VASKDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: etc
	$(MKDIR) $(ETC)/support
	mv $(ETC)/$(PGM)$(EXE) $(ETC)/support

htmletc:
	@echo "No docs to generate."

