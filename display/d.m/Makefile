MODULE_TOPDIR = ../..

PGM=d.m
PGM2=d.m.tcl
PROG1 =  $(GISBASE)/scripts/$(PGM)
PROG2 =  $(ETC)/dm/$(PGM2)

include $(MODULE_TOPDIR)/include/Make/Module.make

default: $(PROG1) $(PROG2) htmlscript

TCL = cmd.tcl grassabout.tcl group.tcl labels.tcl menu.tcl print.tcl raster.tcl tksys.tcl tool1.tcl tool2.tcl tree.tcl vector.tcl monitorsel.tcl

$(PROG1): $(PGM)
	if [ ! -d $(GISBASE)/scripts ]; then $(MKDIR) $(GISBASE)/scripts; fi
	$(INSTALL) -m 755 $(PGM) $(PROG1)

$(PROG2): *.gif *.tcl 
	if [ ! -d $(ETC)/dm ]; then $(MKDIR) $(ETC)/dm; fi
	$(INSTALL) -m 644 *.gif $(ETC)/dm/
	for tcl in $(TCL); do \
	    $(INSTALL) $$tcl $(ETC)/dm/ ; \
	done
	$(CHMOD) 755 $(ETC)/dm/tksys.tcl
	$(INSTALL) -m 755 $(PGM2) $(PROG2)
	$(MKDIR) $(GISBASE)/etc/dm/script
	$(INSTALL) -m 755 script/*.* $(GISBASE)/etc/dm/script
