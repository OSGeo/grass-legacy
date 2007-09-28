MODULE_TOPDIR = ../../..

PGM = modhist

LIBES = $(GISLIB) $(EDITLIB) $(VASKLIB) $(CURSES)
DEPENDENCIES = $(GISDEP) $(EDITDEP) $(VASKDEP)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: etc
	$(MKDIR) $(ETC)/support
	mv $(ETC)/$(PGM)$(EXE) $(ETC)/support

htmletc:
	@echo "No docs to generate."

