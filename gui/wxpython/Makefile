MODULE_TOPDIR = ../..

# SUBDIRS=vdigit docs
SUBDIRS = docs
ETCDIR = $(ETC)/wxpython

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: parsubdirs install_scripts

install_scripts:
	$(MKDIR) $(ETCDIR) $(ETCDIR)/compat $(ETCDIR)/gui_modules $(ETCDIR)/icons $(ETCDIR)/icons/silk $(ETCDIR)/images $(ETCDIR)/scripts $(ETCDIR)/vdigit 
	$(INSTALL_DATA) compat/* $(ETCDIR)/compat/
	$(INSTALL_DATA) gui_modules/* $(ETCDIR)/gui_modules/
	$(INSTALL_DATA) icons/*.* $(ETCDIR)/icons/
	$(INSTALL_DATA) icons/silk/* $(ETCDIR)/icons/silk/
	$(INSTALL_DATA) images/* $(ETCDIR)/images/
	$(INSTALL_DATA) scripts/* $(ETCDIR)/scripts/
	$(INSTALL_DATA) gis_set.py wxgui.py README $(ETCDIR)/
#	$(INSTALL_DATA) vdigit/_grass6_wxvdigit.so $(ETCDIR)/vdigit/
#	$(INSTALL_DATA) vdigit/grass6_wxvdigit.py $(ETCDIR)/vdigit/
	$(INSTALL) -m 755 wxgui $(GISBASE)/scripts/

clean: cleansubdirs
