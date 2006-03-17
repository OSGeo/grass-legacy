MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Dir.make

default:
	if [ ! -d $(GISBASE)/etc/gui/ ] ; then $(MKDIR) $(GISBASE)/etc/gui/ ; fi
	if [ ! -d $(GISBASE)/etc/gui/icons/ ] ; then $(MKDIR) $(GISBASE)/etc/gui/icons/ ; fi
	$(INSTALL) -m 644 *.gif $(GISBASE)/etc/gui/icons/
