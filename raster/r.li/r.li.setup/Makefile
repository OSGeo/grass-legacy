TARGET= $(ETC)/r.li.setup

MODULE_TOPDIR = ../../..

PGM = r.li.setup

include $(MODULE_TOPDIR)/include/Make/Script.make

default: script
	$(MKDIR) $(TARGET)
	$(INSTALL) -m 755 ./area_query $(TARGET)/area_query
	$(INSTALL) -m 755 ./masked_area_selection $(TARGET)/masked_area_selection
	$(INSTALL) -m 755 ./r.li.setup.main $(TARGET)/r.li.setup.main
	$(INSTALL) -m 755 ./r.li.setup.procedures.tcl $(TARGET)/r.li.setup.procedures.tcl
	$(INSTALL) -m 755 ./r.li.windows.tcl $(TARGET)/r.li.windows.tcl
	$(INSTALL) -m 755 ./sample_area_vector.sh $(TARGET)/sample_area_vector.sh
	$(INSTALL) -m 755 ./square_mouse_selection.sh $(TARGET)/square_mouse_selection.sh
	$(INSTALL) -m 755 ./square_query $(TARGET)/square_query
	$(INSTALL_DATA) ./circle.txt $(TARGET)/circle.txt
	$(INSTALL_DATA) ./polygon.txt $(TARGET)/polygon.txt


