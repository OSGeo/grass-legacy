
# common dependencies and rules for building scripts

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

PROGDIR =  $(ARCH_DISTDIR)/scripts/

script: $(PROGDIR)/$(PGM) htmlscript scriptstrings

$(PROGDIR)/$(PGM): $(PGM)
	if [ ! -d $(PROGDIR) ]; then mkdir $(PROGDIR); fi
	cp $(PGM) $(PROGDIR)
	chmod 0755 $(PROGDIR)/$(PGM)

# Make strings in a fake .c file so that they get picked up by the internationalizer stuff.
# These are only the options (parser.c) type things.
# See locale/scriptstrings/README for more information

scriptstrings = \
	GISRC=$(RUN_GISBASE)/demolocation/.grassrc${GRASS_VERSION_MAJOR}${GRASS_VERSION_MINOR} \
	GISBASE=$(RUN_GISBASE) \
	PATH=$(BIN):$$PATH \
	$(LD_LIBRARY_PATH_VAR)="$(ARCH_LIBDIR):$($(LD_LIBRARY_PATH_VAR))" \
	g.parser -t $(1) | sed s/\"/\\\\\"/g | sed 's/.*/_("&")/' > \
	$(MODULE_TOPDIR)/locale/scriptstrings/$(PGM)_to_translate.c ; true

scriptstrings:
	$(call scriptstrings,$(PGM))

