MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Dir.make

default:
	if [ ! -d $(GISBASE)/etc/gui/ ] ; then $(MKDIR) $(GISBASE)/etc/gui/ ; fi
	if [ ! -d $(GISBASE)/etc/gui/icons/ ] ; then $(MKDIR) $(GISBASE)/etc/gui/icons/ ; fi
	$(INSTALL) -m 644 *.gif $(GISBASE)/etc/gui/icons/
	if [ ! -d $(GISBASE)/docs/ ] ; then $(MKDIR) $(GISBASE)/docs/ ; fi
	if [ ! -d $(GISBASE)/docs/html/ ] ; then $(MKDIR) $(GISBASE)/docs/html/ ; fi
	if [ ! -d $(GISBASE)/docs/html/icons/ ] ; then $(MKDIR) $(GISBASE)/docs/html/icons/ ; fi
	$(INSTALL) -m 644 *.gif $(GISBASE)/docs/html/icons/
