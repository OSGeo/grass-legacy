MODULE_TOPDIR = ../..

PGM=wxgrass

SUBDIRS=vdigit

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: parsubdirs install_scripts

install_scripts:
	$(MKDIR) $(ETC)/wx/wx $(ETC)/wx/compat $(ETC)/wx/vdigit $(ETC)/wx/gui_modules $(ETC)/wx/icons $(ETC)/wx/icons/silk $(ETC)/wx/images  $(ETC)/wx/scripts
	$(INSTALL_DATA) compat/* $(ETC)/wx/compat/
	$(INSTALL_DATA) gui_modules/* $(ETC)/wx/gui_modules/
	$(INSTALL_DATA) icons/*.* $(ETC)/wx/icons/
	$(INSTALL_DATA) icons/silk/* $(ETC)/wx/icons/silk/
	$(INSTALL_DATA) images/* $(ETC)/wx/images/
	$(INSTALL_DATA) scripts/* $(ETC)/wx/scripts/
	$(INSTALL_DATA) gis_set.py wxgui.py $(ETC)/wx/
	$(INSTALL_DATA) vdigit/_grass6_wxvdigit.so $(ETC)/wx/vdigit/
	$(INSTALL_DATA) vdigit/grass6_wxvdigit.py $(ETC)/wx/vdigit/
	$(INSTALL) -m 755 wxgrass $(GISBASE)/scripts/

clean: cleansubdirs
