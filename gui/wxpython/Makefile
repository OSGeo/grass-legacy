MODULE_TOPDIR = ../..

PGM=gm.wxp

include $(MODULE_TOPDIR)/include/Make/Module.make

default: script
	if [ ! -d $(GISBASE)/scripts ] ; then $(MKDIR) $(GISBASE)/scripts; fi
	$(INSTALL) -m 755 gm.wxp $(GISBASE)/scripts
	if [ ! -d $(GISBASE)/etc/gmwxp ] ; then $(MKDIR) $(GISBASE)/etc/gmwxp; fi
	if [ ! -d $(GISBASE)/etc/gmwxp/scripts/ ] ; then $(MKDIR) $(GISBASE)/etc/gmwxp/scripts/ ; fi
	$(INSTALL) -m 755 *.py $(GISBASE)/etc/gmwxp
	$(INSTALL_DATA) *.dtd $(GISBASE)/etc/gmwxp
	$(INSTALL) -m 755 scripts/*.* $(GISBASE)/etc/gmwxp/scripts
