
# common dependencies and rules for building scripts

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

PROGDIR =  $(GISBASE)/scripts/

script: $(PROGDIR)/$(PGM) htmlscript

$(PROGDIR)/$(PGM): $(PGM)
	if [ ! -d $(PROGDIR) ]; then mkdir $(PROGDIR); fi
	cp $(PGM) $(PROGDIR)
	chmod 0755 $(PROGDIR)/$(PGM)
