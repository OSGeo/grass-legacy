MODULE_TOPDIR = ../..
include $(MODULE_TOPDIR)/include/Make/Module.make

PGM1    = tcltkgrass
PROGDIR = $(GISBASE)/tcltkgrass/

default: $(PROGDIR)/$(PGM1)

$(PROGDIR)/$(PGM1):
	if [ ! -d $(PROGDIR) ] ; then mkdir -p $(PROGDIR); fi
	cp -r * $(PROGDIR)
	mv $(PROGDIR)/$(PGM1) $(GISBASE)/bin
	chmod 0755 $(GISBASE)/bin/$(PGM1)
