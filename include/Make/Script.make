
# common dependencies and rules for building scripts

PROGDIR =  $(ARCH_DISTDIR)/scripts

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

SCRIPT_ACTIONS = $(PROGDIR)/$(PGM) htmlscript scriptstrings
ifdef MINGW
SCRIPT_ACTIONS += $(BIN)/$(PGM).bat
endif

script: $(SCRIPT_ACTIONS)

$(PROGDIR)/$(PGM): $(PGM)
	if [ ! -d $(PROGDIR) ]; then mkdir $(PROGDIR); fi
	$(INSTALL) $(PGM) $(PROGDIR)/$(PGM)

$(BIN)/$(PGM).bat: $(MODULE_TOPDIR)/scripts/windows_launch.bat
	sed -e "s#SCRIPT_NAME#$(PGM)#" $(MODULE_TOPDIR)/scripts/windows_launch.bat > $@

# Make strings in a fake .c file so that they get picked up by the internationalizer stuff.
# These are only the options (parser.c) type things.
# See locale/scriptstrings/README for more information

scriptstrings = \
	GISRC=$(RUN_GISRC) \
	GISBASE=$(RUN_GISBASE) \
	PATH=$(BIN):$$PATH \
	$(LD_LIBRARY_PATH_VAR)="$(ARCH_LIBDIR):$($(LD_LIBRARY_PATH_VAR))" \
	g.parser -t $(1) | sed s/\"/\\\\\"/g | sed 's/.*/_("&")/' > \
	$(MODULE_TOPDIR)/locale/scriptstrings/$(PGM)_to_translate.c ; true

scriptstrings:
	$(call scriptstrings,$(PGM))

