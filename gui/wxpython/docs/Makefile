MODULE_TOPDIR = ../../..

FILES = wxGUI \
	wxGUI.Vector_Digitizer \
	wxGUI.Attribute_Table_Manager \
	wxGUI.Nviz \
	wxGUI.Icons \
	wxGUI.Modeler \
	wxGUI.GCP_Manager

include $(MODULE_TOPDIR)/include/Make/Platform.make
include $(MODULE_TOPDIR)/include/Make/Grass.make
include $(MODULE_TOPDIR)/include/Make/Rules.make

default:
	for file in $(FILES) ; do \
	    rm -f $$file.tmp.html ; \
	    rm -f "$(GISBASE)/docs/html/$$file.html"; \
	    $(MAKE) htmldir PGM=$$file; \
	done
