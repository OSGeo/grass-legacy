MODULE_TOPDIR = ../..

SUBDIRS = docs

include $(MODULE_TOPDIR)/include/Make/Platform.make

ifneq ($(USE_WXWIDGETS),)
  SUBDIRS += vdigit
endif

include $(MODULE_TOPDIR)/include/Make/Dir.make

ETCDIR = $(ETC)/wxpython

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
	$(INSTALL) -m 755 wxgui $(ETCDIR)/scripts/

clean: cleansubdirs
