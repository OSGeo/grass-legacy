MODULE_TOPDIR = ../..
include $(MODULE_TOPDIR)/include/Make/Module.make

PGM1    = tcltkgrass
PROGDIR = $(GISBASE)/tcltkgrass/

default: $(PROGDIR)/$(PGM1)

$(PROGDIR)/$(PGM1):
	if [ ! -d $(PROGDIR) ] ; then $(MKDIR) $(PROGDIR); fi
	if [ ! -d $(PROGDIR)/script ] ; then $(MKDIR) $(PROGDIR)/script ; fi
	( cd script ; for i in `find . -type f | grep -v CVS` ; do $(INSTALL) $$i $(PROGDIR)/script/ ; done )
	if [ ! -d $(PROGDIR)/main ] ; then $(MKDIR) $(PROGDIR)/main ; fi
	( cd main   ; for i in `find . -type f | grep -v CVS` ; do $(INSTALL) $$i $(PROGDIR)/main/ ; done )
	$(INSTALL) -m 755 $(PGM1) $(GISBASE)/bin
