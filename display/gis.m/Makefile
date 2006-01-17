MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Dir.make

default:
	if [ ! -d $(GISBASE)/scripts ]; then $(MKDIR) $(GISBASE)/scripts; fi
	$(INSTALL) -m 755 gis.m $(GISBASE)/scripts
	if [ ! -d $(GISBASE)/etc/gm ]; then $(MKDIR) $(GISBASE)/etc/gm; fi
	$(INSTALL) -m 755 *.tcl  $(GISBASE)/etc/gm
	$(INSTALL) -m 644 *.gif $(GISBASE)/etc/gm
