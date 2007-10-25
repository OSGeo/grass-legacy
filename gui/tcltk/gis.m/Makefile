MODULE_TOPDIR = ../../..

PGM=gis.m

include $(MODULE_TOPDIR)/include/Make/Script.make

default: script install_scripts

install_scripts:
	if [ ! -d $(GISBASE)/etc/gm ] ; then $(MKDIR) $(GISBASE)/etc/gm; fi
	for file in *.tcl ; do $(INSTALL) $$file $(GISBASE)/etc/gm/ ; done
	for file in *.gif ; do $(INSTALL_DATA) $$file $(GISBASE)/etc/gm/ ; done
	for file in gm_*.html ; do $(INSTALL_DATA) $$file $(GISBASE)/docs/html/ ; done
