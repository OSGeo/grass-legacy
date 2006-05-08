MODULE_TOPDIR = ../../..

include $(MODULE_TOPDIR)/include/Make/Dir.make

default:
	if [ ! -d $(GISBASE)/etc/gui/ ] ; then $(MKDIR) $(GISBASE)/etc/gui/ ; fi
	if [ ! -d $(GISBASE)/etc/gui/menus/ ] ; then $(MKDIR) $(GISBASE)/etc/gui/menus/ ; fi
	$(INSTALL) -m 755 *.tcl $(GISBASE)/etc/gui/menus/
