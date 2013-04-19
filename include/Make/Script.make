
# common dependencies and rules for building scripts

SCRIPTDIR = $(ARCH_DISTDIR)/scripts
STRINGDIR = $(MODULE_TOPDIR)/locale/scriptstrings

SCRIPT = $(SCRIPTDIR)/$(PGM)

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

SCRIPT_ACTIONS = $(SCRIPT)
ifdef MINGW
SCRIPT_ACTIONS += $(BIN)/$(PGM).bat
endif

script: $(SCRIPT_ACTIONS)

$(SCRIPT): $(PGM)
	if [ ! -d $(SCRIPTDIR) ]; then $(MKDIR) $(SCRIPTDIR); fi
	$(INSTALL) $(PGM) $(SCRIPT)
	$(MAKE) htmlscript scriptstrings
	$(MAKE) mancmd

$(BIN)/$(PGM).bat: $(MODULE_TOPDIR)/scripts/windows_launch.bat
	sed -e "s#SCRIPT_NAME#$(PGM)#" $(MODULE_TOPDIR)/scripts/windows_launch.bat > $@
	unix2dos $@

# Make strings in a fake .c file so that they get picked up by the internationalizer stuff.
# These are only the options (parser.c) type things.
# See locale/scriptstrings/README for more information

scriptstrings = \
	GISRC=$(RUN_GISRC) \
	GISBASE=$(RUN_GISBASE) \
	PATH=$(BIN):$$PATH \
	$(LD_LIBRARY_PATH_VAR)="$(ARCH_LIBDIR):$($(LD_LIBRARY_PATH_VAR))" \
	g.parser -t $(1) | sed s/\"/\\\\\"/g | sed 's/.*/_("&")/' > \
	$(STRINGDIR)/$(PGM)_to_translate.c ; true

$(STRINGDIR)/$(PGM)_to_translate.c: $(PGM)
	$(call scriptstrings,$(PGM))

scriptstrings: $(STRINGDIR)/$(PGM)_to_translate.c

install:
	$(INSTALL) $(ARCH_DISTDIR)/scripts/$(PGM)$(EXE) $(INST_DIR)/scripts/
	$(INSTALL_DATA) $(HTMLDIR)/$(PGM).html $(INST_DIR)/docs/html/
	$(INSTALL_DATA) $(ARCH_DISTDIR)/man/man1/$(PGM).1 $(INST_DIR)/man/man1/
