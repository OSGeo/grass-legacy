MODULE_TOPDIR = ../..

PGM=wxgrass

include $(MODULE_TOPDIR)/include/Make/Script.make

default: install_scripts

install_scripts:
	$(MKDIR) $(ETC)/wx/wx $(ETC)/wx/compat $(ETC)/wx/display_driver  $(ETC)/wx/gui_modules  $(ETC)/wx/icons $(ETC)/wx/icons/silk $(ETC)/wx/images  $(ETC)/wx/scripts
	$(INSTALL_DATA) compat/* $(ETC)/wx/compat/
	$(INSTALL_DATA) display_driver/* $(ETC)/wx/display_driver/
	$(INSTALL_DATA) gui_modules/* $(ETC)/wx/gui_modules/
	$(INSTALL_DATA) icons/*.* $(ETC)/wx/icons/
	$(INSTALL_DATA) icons/silk/* $(ETC)/wx/icons/silk/
	$(INSTALL_DATA) images/* $(ETC)/wx/images/
	$(INSTALL_DATA) scripts/* $(ETC)/wx/scripts/
	$(INSTALL_DATA) gis_set.py wxgui.py $(ETC)/wx/
	$(INSTALL) -m 755 wxgrass $(GISBASE)/scripts/
