MODULE_TOPDIR = ../..

PGM = g.manual
PROGDIR =  $(GISBASE)/scripts/

include $(MODULE_TOPDIR)/include/Make/Module.make

default: $(PROGDIR)/$(PGM) htmlscript

$(PROGDIR)/$(PGM): $(PGM)
	if [ ! -d $(PROGDIR) ]; then mkdir $(PROGDIR); fi
	cp $(PGM) $(PROGDIR)
	chmod 0755 $(PROGDIR)/$(PGM)
